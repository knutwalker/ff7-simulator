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

import algebra._
import algebra.Interact._
import battle._
import stats._

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
    BattleAttack(attacks(a), p.asTarget)

  val asTarget: Target = this
  val asPerson: Person = this

  def chooseAttack(opponents: Team, allies: Team): Interact[BattleAttack] = {
    val alive = opponents.alives
    alive.headOption
      .map(a ⇒ ai(this, opponents.copy(first = a, rest = alive.tail)))
      .getOrElse(unit(BattleAttack.None))
  }

  def hit(h: Hit): Person = h match {
    case Hit.Missed      ⇒ this
    case Hit.Hits(c)     ⇒ copy(hp = HP(hp.x - c))
    case Hit.Critical(c) ⇒ copy(hp = HP(hp.x - c))
  }

  override def toString: String =
    s"$name [HP ${hp.x}/${maxHp.x}]"
}
