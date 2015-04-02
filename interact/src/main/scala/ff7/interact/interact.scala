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
package interact

import interact.Input.Special

import scalaz.Scalaz._
import scalaz.{\/, NonEmptyList, Inject}

import math._

sealed trait InteractOp[A]

object InteractOp {
  case class ShowMessage(s: String)
    extends InteractOp[Unit]

  case class ShowItems(ps: List[UiItem], id: TeamId)
    extends InteractOp[Unit]

  case object ReadInput
    extends InteractOp[Input]


  type Injects[F[_]] = Inject[InteractOp, F]

  implicit def instance[F[_]: Injects]: Interact[F] = new Interact[F]
}

class Interact[F[_]: InteractOp.Injects] {
  import InteractOp._

  def showMessage(s: String): algebras.Effect[F, Unit] =
    algebras.Effect(ShowMessage(s))

  def showItems(ps: List[UiItem], id: TeamId): algebras.Effect[F, Unit] =
    algebras.Effect(ShowItems(ps, id))

  val readInput: algebras.Effect[F, Input] =
    algebras.Effect(ReadInput)
}
object Interact {

  def readList[F[_]: Interact, A](
    message: String,
    allyThings: List[_],
    allyIndex: Int,
    opponents: NonEmptyList[A])
  : algebras.Effect[F, Special \/ Option[A]] = for {
    _    ← showItems(formatItems(allyThings, allyIndex), TeamId.Allies)
    _    ← showMessage(message)
    item ← readItems(opponents, 0, TeamId.Opponents)
  } yield item

  def showItems[F[_]: Interact](things: List[_], current: Int, teamId: TeamId): algebras.Effect[F, Unit] =
    showItems(formatItems(things, current), teamId)

  def readItems[F[_]: Interact, A](things: NonEmptyList[A], current: Int, teamId: TeamId): algebras.Effect[F, Special \/ Option[A]] = {
    val lowerBound = 0
    val upperBound = things.size - 1
    val bounded = min(max(lowerBound, current), upperBound)
    def readsInput: algebras.Effect[F, Special \/ Option[A]] = readInput.flatMap {
      case Input.Quit   ⇒ algebras.Effect.point(\/.left(Input.Quit))
      case Input.Undo   ⇒ algebras.Effect.point(\/.left(Input.Undo))
      case Input.Cancel ⇒ algebras.Effect.point(\/.right(none))
      case Input.Ok     ⇒ algebras.Effect.point(\/.right(things.list(bounded).some))
      case Input.Up     if bounded == lowerBound ⇒ readsInput
      case Input.Down   if bounded == upperBound ⇒ readsInput
      case Input.Up     ⇒ readItems(things, bounded - 1, teamId)
      case Input.Down   ⇒ readItems(things, bounded + 1, teamId)
      case _            ⇒ readsInput
    }
    showItems(things.list, bounded, teamId) >> readsInput
  }

  def showMessage[F[_]](s: String)(implicit I: Interact[F]): algebras.Effect[F, Unit] =
    I.showMessage(s)

  def showItems[F[_]](ps: List[UiItem], id: TeamId)(implicit I: Interact[F]): algebras.Effect[F, Unit] =
    I.showItems(ps, id)

  def readInput[F[_]](implicit I: Interact[F]): algebras.Effect[F, Input] =
    I.readInput

  private def formatItems(things: List[_], current: Int): List[UiItem] =
    things
      .zipWithIndex
      .map(px ⇒ UiItem(px._1.toString, px._2 == current))

}
