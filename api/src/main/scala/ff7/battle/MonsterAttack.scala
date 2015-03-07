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

final case class MonsterAttack(
  name: String,
  cost: Option[MP],
  attackType: AttackType,
  formulaType: FormulaType,
  power: Power,
  attackPercent: AttackPercent)
object MonsterAttack {
  def physical(name: String, attackPercent: AttackPercent = AttackPercent(100), power: Power = Power(1), cost: Option[MP] = None): MonsterAttack =
    MonsterAttack(name, cost, AttackType.Physical, FormulaType.Physical, power, attackPercent)
}
