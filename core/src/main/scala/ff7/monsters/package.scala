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

import battle.{Team, Person}

import scalaz.Isomorphism._
import scalaz.NonEmptyList

package object monsters {

  type TeamF[x] = Team

  val teamNelIso: Team <=> NonEmptyList[Person] =
    new (Team <=> NonEmptyList[Person]) {
      val to: (Team) ⇒ NonEmptyList[Person] = t ⇒ NonEmptyList.nel(t.first, t.rest)
      val from: (NonEmptyList[Person]) ⇒ Team = nel ⇒ Team(nel.head, nel.tail)
    }

  implicit class NelTeam(val t: Team) extends AnyVal {
    def toNel: NonEmptyList[Person] = teamNelIso.to(t)
  }

  implicit class TeamNel(val ps: NonEmptyList[Person]) extends AnyVal {
    def toTeam: Team = teamNelIso.from(ps)
  }
}
