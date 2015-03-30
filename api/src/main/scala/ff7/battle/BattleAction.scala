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

import monsters.Monster

sealed trait BattleAction
object BattleAction {
  val abort: BattleAction = Abort
  val none: BattleAction = None

  def change(monster: Monster): BattleAction =
    Change(monster)

  def apply(attacker: Attacker, target: Target): BattleAction =
    Attack(attacker, target)


  case object Abort extends BattleAction
  case object None extends BattleAction
  final case class Change(monster: Monster) extends BattleAction
  final case class Attack(attacker: Attacker, target: Target) extends BattleAction
}
