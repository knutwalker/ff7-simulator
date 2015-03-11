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
package algebra


sealed trait Input
object Input {
  val up: Input = Up
  val down: Input = Down
  val right: Input = Right
  val left: Input = Left
  val ok: Input = Ok
  val cancel: Input = Cancel
  val quit: Input = Quit
  val undo: Input = Undo

  sealed trait Special
  case object Up extends Input
  case object Down extends Input
  case object Right extends Input
  case object Left extends Input
  case object Ok extends Input
  case object Cancel extends Input
  case object Quit extends Input with Special
  case object Undo extends Input with Special
}
