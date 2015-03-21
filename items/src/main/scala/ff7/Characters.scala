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

import battle.Team
import characters.Character

import scalaz._
import Scalaz._

object Characters extends ConfigLoader[Character]("ff7.characters", "Character") {

  def team(first: String): Val[Team] = selectDynamic(first).map(Team(_))

  def team(first: String, second: String): Val[Team] =
    (selectDynamic(first) |@| selectDynamic(second)) { (f, s) ⇒ Team(f, s) }

  def team(first: String, second: String, third: String): Val[Team] =
    (selectDynamic(first) |@| selectDynamic(second) |@| selectDynamic(third)) {
      (f, s, t) ⇒ Team(f, s, t)
    }
}
