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

import algebra.Interact
import formula.Formula
import stats._

import scalaz._
import Scalaz._

import com.nicta.rng.Rng
import shapeless.contrib.scalaz._
import spire.math.Rational

sealed trait Person {
  def name: String
  def hp: HP
  def mp: MP
  def asTarget: Target
  def hit(h: Hit): Person
}
object Person {
  implicit val equal: Equal[Person] = Equal.equalRef
}

sealed trait Attacker {
  def level: Level
  def dexterity: Dexterity
  def luck: Luck
  def power: Power
  def attack: Attack
  def attackPercent: AttackPercent
  def defensePercent: DefensePercent
  def asPerson: Person
  def chosenAttack: MonsterAttack
}

sealed trait Target {
  def level: Level
  def luck: Luck
  def defense: Defense
  def defensePercent: DefensePercent
  def asPerson: Person
}
object Target {
  implicit val equal: Equal[Target] = Equal.equalRef
}

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
  armour: Maybe[Armour]) extends Person with Target with Attacker {

  def power = weapon.foldMap(_.power)
  def attack = weapon.foldMap(_.attack) + strength.x
  def attackPercent = weapon.foldMap(_.attackPercent)
  def defense = armour.foldMap(_.defense) + vitality.x
  def defensePercent = armour.foldMap(_.defensePercent) + (dexterity / 4).x

  val asTarget: Target = this
  val asPerson: Person = this

  def chosenAttack: MonsterAttack =
    MonsterAttack.physical("Attack", attackPercent, power)

  def hit(h: Hit): Person = h match {
    case Missed      ⇒ this
    case Hits(c)     ⇒ copy(hp = HP(hp.x - c))
    case Critical(c) ⇒ copy(hp = HP(hp.x - c))
  }

  override def toString: String =
    s"$name [HP ${hp.x}/${maxHp.x}]"

  //  def magicAttack = MagicAttack(magic.x)
  //  def magicDefense = armour.foldMap(_.magicDefense) + spirit.x
  //  def magicDefensePercent = armour.foldMap(_.magicDefensePercent)

}

final case class Monster(
  name: String,
  level: Level,
  xp: XP,
  // win: Win,
  hp: HP, maxHp: HP,
  // ap: AP,
  // steal: Steal,
  mp: MP, maxMp: MP,
  // gil: Gil,
  // morph: Morph,
  attack: Attack,
  defense: Defense,
  defensePercent: DefensePercent,
  dexterity: Dexterity,
  magicAttack: MagicAttack,
  magicDefense: MagicDefense,
  luck: Luck,
  ai: AI) extends Person with Target {

  def attacks(a: MonsterAttack): MonsterAttacks =
    MonsterAttacks(this, a)

  def attacks(p: Person, a: MonsterAttack): BattleAttack =
    BattleAction(attacks(a), p.asTarget)

  def asTarget: Target = this
  def asPerson: Person = this
  def hit(h: Hit): Person = h match {
    case Missed      ⇒ this
    case Hits(c)     ⇒ copy(hp = HP(hp.x - c))
    case Critical(c) ⇒ copy(hp = HP(hp.x - c))
  }

  override def toString: String =
    s"$name [HP ${hp.x}/${maxHp.x}]"
}

final case class MonsterAttacks(
  monster: Monster,
  chosenAttack: MonsterAttack
  ) extends Attacker with Target {
  def name: String = monster.name
  def hp: HP = monster.hp
  def mp: MP = monster.mp
  def level: Level = monster.level
  def dexterity: Dexterity = monster.dexterity
  def luck: Luck = monster.luck
  def power: Power = chosenAttack.power
  def attack: Attack = monster.attack
  def defense: Defense = monster.defense
  def defensePercent: DefensePercent = monster.defensePercent
  def attackPercent: AttackPercent = chosenAttack.attackPercent
  def asPerson: Person = monster

  override def toString: String = s"$monster (${chosenAttack.name})"
}


final case class Weapon(
  name: String,
  power: Power,
  attack: Attack,
  attackPercent: AttackPercent,
  magicAttack: MagicAttack)

final case class Armour(
  name: String,
  defense: Defense,
  defensePercent: DefensePercent,
  magicDefense: MagicDefense,
  magicDefensePercent: MagicDefensePercent)

final case class MonsterAttack(
  name: String,
  cost: Maybe[MP],
  attackType: AttackType,
  formula: Formula,
  power: Power,
  attackPercent: AttackPercent)
object MonsterAttack {
  def physical(name: String, attackPercent: AttackPercent = AttackPercent(100), power: Power = Power(1), cost: Maybe[MP] = Maybe.empty): MonsterAttack =
  MonsterAttack(name, cost, Physical, formula.Physical, power, attackPercent)
}

final case class Team(persons: NonEmptyList[Person])
object Team {
  def apply(person: Person, persons: Person*): Team =
    Team(NonEmptyList(person, persons: _*))
}

//sealed trait RowPosition
//case object FrontRow extends RowPosition
//case object BackRow extends RowPosition
//case object NoRow extends RowPosition
//
//final case class Row(persons: NonEmptyList[Person])
//final case class Team(rows: NonEmptyList[Row]) {
//  def persons: NonEmptyList[Person] = rows.flatMap(_.persons)
//  def rowOf(x: Person): Option[Row] = {
//    rows.list.find(_.persons.element(x))
//  }
//  def rowPosition(x: Person): RowPosition = {
//    if(rows.head.persons.element(x)) FrontRow
//    else if (rows.tail.exists(r ⇒ r.persons.element(x))) BackRow
//    else NoRow
//  }
//}
//object Team {
//  def apply(person: Person, persons: Person*): Team =
//    Team(Row(NonEmptyList(person, persons: _*)))
//  def apply(row: Row): Team =
//    Team(row.wrapNel)
//}

final case class BattleField(heroes: Team, enemies: Team, round: Int, history: Vector[BattleResult], aborted: Boolean) {
  def isFinished: Boolean = aborted || List(heroes, enemies).exists(_.persons ∀ (_.hp.x <= 0))
  def round(br: BattleResult): BattleField = copy(round = round + 1, history = history :+ br)
}
object BattleField {
  def init(heroes: Team, enemies: Team): BattleField =
    BattleField(heroes, enemies, 0, Vector(), aborted = false)
}

sealed trait BattleAttack
object BattleAttack {
  val abort: BattleAttack = AbortAttack
  val none: BattleAttack = NoAttack
  def attack(attacker: Attacker, target: Target): BattleAttack =
    BattleAction(attacker, target)
}
case object AbortAttack extends BattleAttack
case object NoAttack extends BattleAttack
final case class BattleAction(attacker: Attacker, target: Target) extends BattleAttack

sealed trait BattleResult
case object AttackAborted extends BattleResult
case object NotAttacked extends BattleResult
final case class AttackResult(originalAttacker: Person, attacker: Attacker, target: Target, hit: Hit) extends BattleResult

trait AI extends {
  def setup(self: Monster): Interact[Monster] = Interact.unit(self)
  def apply(self: Monster, heroes: Team, targets: Team): Interact[BattleAttack]
}
object AI {
  def choose[A](num: Long, denom: Long, ifHit: ⇒ A, ifMiss: ⇒ A): Rng[A] =
    chance(num, denom).map(c ⇒ if (c) ifHit else ifMiss)

  def choose[A](r: Rational, ifHit: ⇒ A, ifMiss: ⇒ A): Rng[A] =
    chance(r).map(c ⇒ if (c) ifHit else ifMiss)

  def chance(num: Long, denom: Long): Rng[Boolean] =
    chance(Rational(num, denom))

  def chance(r: Rational): Rng[Boolean] =
    Rng.chooselong(1L, r.denominatorAsLong)
      .map(i ⇒ i <= r.numeratorAsLong)

  trait Ai extends AI {
    def attack: Rng[MonsterAttack]
    def attack(self: Monster): Rng[MonsterAttack] = attack
    def target(targets: Team): Rng[Person] = Rng.oneofL(targets.persons)
    def modify(self: Monster): Monster = self

    final def apply(self: Monster, heroes: Team, targets: Team): Interact[BattleAttack] = {
      val tar = target(targets)
      val att = attack(self)
      val mon = modify(self)
      val bat = tar.flatMap(t ⇒ att.map(a ⇒ mon.attacks(t, a)))
      Interact.random(bat)
    }
  }
}

sealed trait Hit
case object Missed extends Hit
final case class Hits(damage: Int) extends Hit
final case class Critical(damage: Int) extends Hit
object Hit {
  def apply(damage: Int): Hit = Hits(damage)
  def critical(damage: Int): Hit = Critical(damage)
  val missed: Hit = Missed
}
