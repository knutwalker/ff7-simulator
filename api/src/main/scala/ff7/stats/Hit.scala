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
package stats

sealed trait Hit
object Hit {
  val missed: Hit = Missed
  def apply(damage: Int): Hit = Hits(damage)
  def critical(damage: Int): Hit = Critical(damage)

  case object Missed extends Hit
  final case class Hits(damage: Int) extends Hit
  final case class Critical(damage: Int) extends Hit
}
