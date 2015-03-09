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

import scalaz.NonEmptyList

import spire.math.Rational

final case class Encounter(id: String, encounterValue: Int, groups: NonEmptyList[Encounter.Group])

object Encounter {
  // TODO: rows
  final case class Group(id: String, chance: Rational, monsters: Team, kind: Kind, runDifficulty: Int)

  sealed trait Kind
  case object Normal extends Kind
  case object Back extends Kind
}
