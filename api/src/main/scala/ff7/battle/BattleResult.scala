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

sealed trait BattleResult
object BattleResult {

  val aborted: BattleResult = Aborted
  val undo: BattleResult = Undo
  val none: BattleResult = None
  def apply(originalAttacker: Person, attacker: Attacker, target: Target, hit: Hit): BattleResult =
    Attack(originalAttacker, attacker, target, hit)

  case object Aborted extends BattleResult
  case object Undo extends BattleResult
  case object None extends BattleResult
  final case class Attack(originalAttacker: Person, attacker: Attacker, target: Target, hit: Hit) extends BattleResult
}
