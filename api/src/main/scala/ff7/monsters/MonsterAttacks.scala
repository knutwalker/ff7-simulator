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

import battle._
import stats._

final case class MonsterAttacks(
  monster: Monster,
  chosenAttack: BattleAttack
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
  def magicAttack: MagicAttack = monster.magicAttack
  def magicDefense: MagicDefense = monster.magicDefense
  def magicDefensePercent: MagicDefensePercent = monster.magicDefensePercent
  def attackPercent: AttackPercent = chosenAttack.attackPercent
  def magicAttackPercent: MagicAttackPercent = chosenAttack.magicAttackPercent

  def asPerson: Person = monster

  override def toString: String = s"$monster ($chosenAttack)"
}
