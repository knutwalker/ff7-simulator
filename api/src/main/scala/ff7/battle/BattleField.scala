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

final case class BattleField(heroes: Team, enemies: Team, round: Int, history: List[BattleField], aborted: Boolean, result: Option[BattleResult]) {
  def isFinished: Boolean =
    aborted || (List(heroes, enemies) exists (_.persons forall (_.hp.x <= 0)))

  def round(br: BattleResult): BattleField =
    copy(round = round + 1, history = copy(result = Some(br)) :: history)

  def swap: BattleField =
    copy(heroes = enemies, enemies = heroes)

  def cycle: BattleField =
    copy(heroes = heroes.cycle)

  override def toString: String = s"Battle [$heroes] vs [$enemies]"
}
object BattleField {
  def init(heroes: Team, enemies: Team): BattleField =
    BattleField(heroes, enemies, 0, Nil, aborted = false, result = None)
}
