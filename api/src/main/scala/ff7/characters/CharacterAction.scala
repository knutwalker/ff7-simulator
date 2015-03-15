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

package ff7.characters

import scalaz.NonEmptyList

sealed trait CharacterAction
object CharacterAction {
  val attack: CharacterAction = Attack
  val magic: CharacterAction = Magic
//  val item: CharacterAction = Item
//  val defend: CharacterAction = Defend
  val skip: CharacterAction = Skip

  //  val actions = NonEmptyList(attack, magic, item, defend, skip)
  val actions = NonEmptyList(attack, magic, skip)

  case object Attack extends CharacterAction
  case object Magic extends CharacterAction
//  case object Item extends CharacterAction
//  case object Defend extends CharacterAction
  case object Skip extends CharacterAction
}
