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
package monsters

import algebra.Effect.point
import algebra._
import battle._
import stats._

import scalaz._
import Scalaz._

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

  def magicDefensePercent: MagicDefensePercent =
    MagicDefensePercent(0)

  def attacks(a: MonsterAttack): MonsterAttacks =
    MonsterAttacks(this, a)

  def attacks(p: Person, a: MonsterAttack): BattleAttack =
    BattleAttack(attacks(a), p.asTarget)

  val asTarget: Target = this
  val asPerson: Person = this
  val isHero: Boolean = false

  def chooseAttack[F[_]: Interact : Random](opponents: Team, allies: Team): Effect[F, Input.Special \/ BattleAttack] = {
    opponents.alives.toNel
      .some(ops ⇒ ai.apply(this, ops.toTeam))
      .none(point(BattleAttack.none))
      .map(_.right)
  }

  def hit(h: Hit): Person = h match {
    case Hit.Missed      ⇒ this
    case Hit.Hits(c)     ⇒ copy(hp = HP(hp.x - c))
    case Hit.Critical(c) ⇒ copy(hp = HP(hp.x - c))
  }

  def a: Monster = copy(name = s"$name A")
  def b: Monster = copy(name = s"$name B")
  def c: Monster = copy(name = s"$name C")
  def d: Monster = copy(name = s"$name D")
  def e: Monster = copy(name = s"$name E")

  override def toString: String =
    s"$name [HP ${hp.x}/${maxHp.x} | MP ${mp.x}/${maxMp.x}]"
}
