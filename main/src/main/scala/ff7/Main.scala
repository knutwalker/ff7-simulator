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

import algebra.InteractOp.ShowMessage
import algebra._
import ff7.Program.{ETeam, RoundState}

import scalaz._
import effect.{IO, SafeApp}

object Main extends SafeApp {

  type F0[A] = Coproduct[InteractOp, LogOp, A]
  type App[A] = Coproduct[RandomOp, F0, A]

  override def runl(args: List[String]): IO[Unit] = for {
    o ← Args.parse[App](args)
    _ ← o.ui.start
    _ ← runRoundsIO(o.repetitions, o.ui, o.enemies)
    _ ← o.ui.stop
  } yield ()

  def runRoundsIO(repetitions: Int, ui: UI, enemies: ETeam[App]): IO[Option[RoundState]] = {
    val app: App ~> IO = appInterpreter(ui)
    Program.runRounds[App](repetitions, enemies).runM[IO](app)
  }

  def appInterpreter(ui: UI): App ~> IO = {
    val interact: InteractOp ~> IO =
      if (ui.logShowMessages) loggingUi(ui)
      else                    ui.interpreter

    val f0: F0 ~> IO = interact or log.Interpreter

    random.Interpreter or f0
  }

  def loggingUi(ui: UI): InteractOp ~> IO = {
    val delegate = ui.interpreter
    new (InteractOp ~> IO) {
      def apply[A](fa: InteractOp[A]): IO[A] = fa match {
        case ShowMessage(s) ⇒ log.info(s).flatMap(_ ⇒ delegate(fa))
        case _              ⇒ delegate(fa)
      }
    }
  }
}
