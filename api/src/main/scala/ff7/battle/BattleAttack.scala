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

sealed trait BattleAttack
object BattleAttack {
  val abort: BattleAttack = Abort
  val none: BattleAttack = None
  def apply(attacker: Attacker, target: Target): BattleAttack =
    Attack(attacker, target)

  case object Abort extends BattleAttack
  case object None extends BattleAttack
  final case class Attack(attacker: Attacker, target: Target) extends BattleAttack
}
