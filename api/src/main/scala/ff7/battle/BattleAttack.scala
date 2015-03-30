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
package battle

import stats._

sealed class BattleAttack(
  val name: String,
  val cost: Option[MP],
  val attackType: AttackType,
  val formulaType: FormulaType,
  val power: Power) {

  def availableFor(p: Person): Boolean = cost.fold(true)(_.x <= p.mp.x)

  def attackPercent: AttackPercent = AttackPercent(magicAttackPercent.x)
  def magicAttackPercent: MagicAttackPercent = MagicAttackPercent(attackPercent.x)

  override def toString: String = s"$name${cost.fold("")(c ⇒ s" (${c.x} MP)")}"
}
object BattleAttack {
  def physical(name: String, percent: AttackPercent = AttackPercent(100), power: Power = Power(1), cost: Option[MP] = None): BattleAttack =
    new BattleAttack(name, cost, AttackType.Physical, FormulaType.Physical, power) {
      override def attackPercent: AttackPercent = percent
    }
  def magical(name: String, cost: MP, percent: MagicAttackPercent = MagicAttackPercent(100), power: Power = Power(1)): BattleAttack =
    new BattleAttack(name, Some(cost), AttackType.Magical, FormulaType.Magical, power) {
      override def magicAttackPercent: MagicAttackPercent = percent
    }
}
