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
package gui

import algebra._, InteractOp._

import scalaz._
import effect.IO


object FxUI {
  val Interpreter: InteractOp ~> IO = new (InteractOp ~> IO) {
    def apply[A](fa: InteractOp[A]): IO[A] = fa match {
      case ShowItems(ps, id) ⇒ printsPersons(ps, id)
      case ShowMessage(s)    ⇒ printsString(s)
      case ReadInput         ⇒ readsInput
    }
  }

  def start(): IO[Unit] = IO(FxApp.launch())

  def stop(): IO[Unit] = IO(FxApp.stop())

  private def printsPersons(ps: List[UiItem], id: TeamId): IO[Unit] = IO {
    FxApp.items.onNext((ps, id))
  }

  private def printsString(s: String): IO[Unit] = IO {
    FxApp.labels.onNext(s)
  }

  private def readsInput: IO[Input] = IO {
    FxApp.inputs.take(1).toBlocking.first
  }
}
