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

import battle.{Person, Target, Attacker, MonsterAttack}
import stats._

final case class CharacterAttacks(
  character: Character,
  chosenAttack: MonsterAttack
  ) extends Attacker with Target {

  def name: String = character.name
  def hp: HP = character.hp
  def mp: MP = character.mp
  def level: Level = character.level
  def dexterity: Dexterity = character.dexterity
  def luck: Luck = character.luck
  def power: Power = chosenAttack.power
  def attack: Attack = character.attack
  def defense: Defense = character.defense
  def defensePercent: DefensePercent = character.defensePercent
  def magicAttack: MagicAttack = character.magicAttack
  def magicDefense: MagicDefense = character.magicDefense
  def magicDefensePercent: MagicDefensePercent = character.magicDefensePercent
  def attackPercent: AttackPercent = chosenAttack.attackPercent
  def magicAttackPercent: MagicAttackPercent = chosenAttack.magicAttackPercent

  def asPerson: Person = character

  override def toString: String = s"$character ($chosenAttack)"
}
