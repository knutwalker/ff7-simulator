/*
 * Copyright 2015 Paul Horn
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ff7
package characters

import algebra.Effect._
import algebra.Input.Special
import algebra.{Effect, Input, Interact, Random, TeamId, UiItem}
import battle._
import stats._

import scalaz._
import Maybe._
import Scalaz._
import scalaz.std.list

import shapeless.contrib.scalaz._
import spire.math.Rational

import math._

final case class Character(
  name: String,
  level: Level,
  hp: HP, maxHp: HP,
  mp: MP, maxMp: MP,
  strength: Strength,
  dexterity: Dexterity,
  vitality: Vitality,
  magic: Magic,
  spirit: Spirit,
  luck: Luck,
  xp: XP,
  weapon: Maybe[Weapon],
  armour: Maybe[Armour]) extends Person with Target {

  def power = weapon.foldMap(_.power)
  def attack = weapon.foldMap(_.attack) + strength.x
  def attackPercent = weapon.foldMap(_.attackPercent)
  def defense = armour.foldMap(_.defense) + vitality.x
  def defensePercent = armour.foldMap(_.defensePercent) + (dexterity / 4).x

  val asTarget: Target = this
  val asPerson: Person = this
  val isHero: Boolean = true

  def physicalAttack: MonsterAttack =
    MonsterAttack.physical("Attack", attackPercent, power)

  def magicalAttack: MonsterAttack =
    MonsterAttack.magical("Fire", MP(4), MagicAttackPercent(100), Power(Rational(1, 2)))

  def attackPhysical: Attacker =
    CharacterAttacks(this, physicalAttack)

  def attackMagical: Attacker =
    CharacterAttacks(copy(mp = MP(mp.x - magicalAttack.cost.fold(0)(_.x))), magicalAttack)

  def chooseAttack[F[_]: Interact : Random](opponents: Team, allies: Team): Effect[F, Special \/ BattleAttack] =
    list.toNel(opponents.alivesInOrder)
      .fold(Character.noAttack)(Character.selectAttack(this, allies))

  def hit(h: Hit): Person = h match {
    case Hit.Missed      ⇒ this
    case Hit.Hits(c)     ⇒ copy(hp = HP(hp.x - c))
    case Hit.Critical(c) ⇒ copy(hp = HP(hp.x - c))
  }

  def hasMagicAttack: Boolean = {
    magicalAttack.cost.exists(_.x <= mp.x)
  }

  override def toString: String =
    s"$name [HP ${hp.x}/${maxHp.x} | MP ${mp.x}/${maxMp.x}]"

  def magicAttack = MagicAttack(magic.x)
  def magicDefense = armour.foldMap(_.magicDefense) + spirit.x
  def magicDefensePercent = armour.foldMap(_.magicDefensePercent)
}
object Character {

  private def noAttack[F[_]: Interact : Random]: Effect[F, Special \/ BattleAttack] =
    point(\/.right(BattleAttack.none))

  private def selectAttack[F[_]: Interact](
    c: Character,
    allies: Team)(
    persons: NonEmptyList[Person])
  : Effect[F, Special \/ BattleAttack] = {
    val aliveAllies = allies.alivesInOrder
    val currentAttacker = aliveAllies.indexOf(c.asPerson)
    val actions: List[CharacterAction] = CharacterAction.actions.list.collect {
      case a@CharacterAction.Magic if c.hasMagicAttack ⇒ a
      case a@CharacterAction.Attack ⇒ a
      case a@CharacterAction.Skip   ⇒ a
    }

    for {
      _      ← showItems(formatItems(aliveAllies, currentAttacker), TeamId.Allies)
      _      ← showMessage(s"$c: Choose your attack")
      action ← readList(actions.toNel.get, 0) // TODO: actions should really be some, but just in case
      act    = action.map(_ | CharacterAction.skip)
      result ← evaluateMaybeDecision(act, c, allies, persons)
    } yield result
  }

  private def evaluateMaybeDecision[F[_]: Interact](
    d: Special \/ CharacterAction,
    c: Character,
    as: Team,
    ps: NonEmptyList[Person])
  : Effect[F, Special \/ BattleAttack] =
    d.traverse[({type λ[α] = Effect[F, α]})#λ, Special, Special \/ BattleAttack](
      evaluateDecision[F](_, c, as, ps)).map(_.flatMap(x ⇒ x))

  private def evaluateDecision[F[_]: Interact](
    d: CharacterAction,
    c: Character,
    as: Team,
    ps: NonEmptyList[Person])
  : Effect[F, Special \/ BattleAttack] = d match {
    case CharacterAction.Attack ⇒
      selectPerson(c.attackPhysical, as, ps)
    case CharacterAction.Magic ⇒
      selectPerson(c.attackMagical, as, ps)
//    case CharacterAction.Item ⇒
//      BattleAttack.none.right[Special].effect[F]
//    case CharacterAction.Defend ⇒
//      BattleAttack.none.right[Special].effect[F]
    case CharacterAction.Skip ⇒
      BattleAttack.none.right[Special].effect[F]
  }

  private def selectPerson[F[_]: Interact](a: Attacker, allies: Team, persons: NonEmptyList[Person]): Effect[F, Special \/ BattleAttack] = {
    val aliveAllies = allies.alivesInOrder
    val currentAttacker = aliveAllies.indexOf(a.asPerson)
    for {
      _      ← showItems(formatItems(aliveAllies, currentAttacker), TeamId.Allies)
      _      ← showMessage(s"$a: Choose your enemy")
      result ← readList(persons, 0)
    } yield result.map(_.cata(p ⇒ BattleAttack(a, p.asTarget), BattleAttack.none))
  }

  private def readList[F[_]: Interact, A](things: NonEmptyList[A], current: Int = 0): Effect[F, Special \/ Maybe[A]] = {
    val lowerBound = 0
    val upperBound = things.size - 1
    val bounded = min(max(lowerBound, current), upperBound)
    def readsInput: Effect[F, Special \/ Maybe[A]] = readInput.flatMap {
      case Input.Quit   ⇒ point(\/.left(Input.Quit))
      case Input.Undo   ⇒ point(\/.left(Input.Undo))
      case Input.Cancel ⇒ point(\/.right(empty))
      case Input.Ok     ⇒ point(\/.right(things.list(bounded).just))
      case Input.Up     if bounded == lowerBound ⇒ readsInput
      case Input.Down   if bounded == upperBound ⇒ readsInput
      case Input.Up     ⇒ readList(things, bounded - 1)
      case Input.Down   ⇒ readList(things, bounded + 1)
      case _            ⇒ readsInput
    }
    printOpponents(things.list, bounded) >> readsInput
  }

  private def printOpponents[F[_]: Interact](things: List[_], current: Int): Effect[F, Unit] =
    showItems(formatItems(things, current), TeamId.Opponents)

  private def formatItems(things: List[_], current: Int): List[UiItem] = things
    .zipWithIndex
    .map(px ⇒ UiItem(px._1.toString, px._2 == current))
}
