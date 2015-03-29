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

import algebra.Input.Special

import scalaz._
import Scalaz._

import scala.math._

object Interacts {

  def readList[F[_]: Interact, A](
    message: String,
    allyThings: List[_],
    allyIndex: Int,
    opponents: NonEmptyList[A])
  : Effect[F, Special \/ Option[A]] = for {
    _    ← Effect.showItems(formatItems(allyThings, allyIndex), TeamId.Allies)
    _    ← Effect.showMessage(message)
    item ← readItems(opponents, 0, TeamId.Opponents)
  } yield item

  def showItems[F[_]: Interact](things: List[_], current: Int, teamId: TeamId): Effect[F, Unit] =
    Effect.showItems(formatItems(things, current), teamId)

  def readItems[F[_]: Interact, A](things: NonEmptyList[A], current: Int, teamId: TeamId): Effect[F, Special \/ Option[A]] = {
    val lowerBound = 0
    val upperBound = things.size - 1
    val bounded = min(max(lowerBound, current), upperBound)
    def readsInput: Effect[F, Special \/ Option[A]] = Effect.readInput.flatMap {
      case Input.Quit   ⇒ Effect.point(\/.left(Input.Quit))
      case Input.Undo   ⇒ Effect.point(\/.left(Input.Undo))
      case Input.Cancel ⇒ Effect.point(\/.right(none))
      case Input.Ok     ⇒ Effect.point(\/.right(things.list(bounded).some))
      case Input.Up     if bounded == lowerBound ⇒ readsInput
      case Input.Down   if bounded == upperBound ⇒ readsInput
      case Input.Up     ⇒ readItems(things, bounded - 1, teamId)
      case Input.Down   ⇒ readItems(things, bounded + 1, teamId)
      case _            ⇒ readsInput
    }
    showItems(things.list, bounded, teamId) >> readsInput
  }

  private def formatItems(things: List[_], current: Int): List[UiItem] =
    things
      .zipWithIndex
      .map(px ⇒ UiItem(px._1.toString, px._2 == current))

}
