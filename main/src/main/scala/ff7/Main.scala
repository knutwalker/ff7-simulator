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
import algebra.LogOp.Log
import algebra.LogLevel.{Warn, Info, Debug, Error}
import algebra.RandomOp.ChooseInt
import algebra._
import battle.Team
import ff7.Program.RoundState

import scalaz._
import effect.{IO, SafeApp}

import com.nicta.rng.Rng
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

object Main extends SafeApp {
  private implicit val logger = Logger(LoggerFactory.getLogger(Main.getClass))

  type F0[A] = Coproduct[InteractOp, LogOp, A]
  type App[A] = Coproduct[RandomOp, F0, A]

  override def runl(args: List[String]): IO[Unit] = for {
    o ← Args.parse[App](args)
    _ ← o.ui.start
    _ ← runRoundsIO(o.repetitions, o.ui, o.enemies)
    _ ← o.ui.stop
  } yield ()

  def runRoundsIO(repetitions: Int, ui: UI, enemies: Effect[App, Team]): IO[RoundState] = {
    val uiDelegate: InteractOp ~> IO = ui.interpreter

    val interact: InteractOp ~> IO =
      if (ui.logShowMessages) {
        new (InteractOp ~> IO) {
          def apply[A](fa: InteractOp[A]): IO[A] = fa match {
            case ShowMessage(s) ⇒ IO(logger.info(s)).flatMap(_ ⇒ uiDelegate(fa))
            case _ ⇒ uiDelegate(fa)
          }
        }
      } else {
        uiDelegate
      }

    val f0: F0 ~> IO = interact or Slf4jLog
    val app: App ~> IO = RngRandom or f0
    Program.runRounds[App](repetitions, enemies).runM[IO](app)
  }

  val RngRandom: RandomOp ~> IO = new (RandomOp ~> IO) {
    def apply[A](fa: RandomOp[A]): IO[A] = fa match {
      case ChooseInt(l, u) ⇒ Rng.chooseint(l, u).run
    }
  }

  val Slf4jLog: LogOp ~> IO = new (LogOp ~> IO) {
    def apply[A](fa: LogOp[A]): IO[A] = fa match {
      case Log(x, Debug, Some(ex))     ⇒ IO(logger.debug(x, ex))
      case Log(x, Debug, None)         ⇒ IO(logger.debug(x))
      case Log(x, Info, Some(ex))      ⇒ IO(logger.info(x, ex))
      case Log(x, Info, None)          ⇒ IO(logger.info(x))
      case Log(x, Warn, Some(ex))      ⇒ IO(logger.warn(x, ex))
      case Log(x, Warn, None)          ⇒ IO(logger.warn(x))
      case Log(x, Error, Some(ex))     ⇒ IO(logger.error(x, ex))
      case Log(x, Error, None)         ⇒ IO(logger.error(x))
    }
  }
}
