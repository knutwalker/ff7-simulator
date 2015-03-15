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

import algebra.InteractOp
import algebra.InteractOp.{ShowMessage, Log}
import algebra.LogLevel.{Error, Warn, Info, Debug}

import scalaz._
import scalaz.effect.IO

import com.typesafe.scalalogging.Logger

sealed trait UI {
  def start: IO[Unit]
  def stop: IO[Unit]
  implicit def interpreter(implicit log: Logger): InteractOp ~> IO
}
object UI {
  def LoggingInterpreter(log: Logger, logPrints: Boolean, delegate: InteractOp ~> IO) = new (InteractOp ~> IO) {
    def apply[A](fa: InteractOp[A]): IO[A] = fa match {
      case Log(x, Debug, Some(ex))     ⇒ IO(log.debug(x, ex))
      case Log(x, Debug, None)         ⇒ IO(log.debug(x))
      case Log(x, Info, Some(ex))      ⇒ IO(log.info(x, ex))
      case Log(x, Info, None)          ⇒ IO(log.info(x))
      case Log(x, Warn, Some(ex))      ⇒ IO(log.warn(x, ex))
      case Log(x, Warn, None)          ⇒ IO(log.warn(x))
      case Log(x, Error, Some(ex))     ⇒ IO(log.error(x, ex))
      case Log(x, Error, None)         ⇒ IO(log.error(x))
      case ShowMessage(s) if logPrints ⇒ IO(log.info(s)).flatMap(_ ⇒ delegate(fa))
      case _                           ⇒ delegate(fa)
    }
  }
}
case object Console extends UI {
  def start: IO[Unit] = IO(())
  def stop: IO[Unit] = tui.close
  implicit def interpreter(implicit log: Logger): InteractOp ~> IO =
    UI.LoggingInterpreter(log, logPrints = false, tui.ConsoleInterpreter)
}
case object GUI extends UI {
  def start: IO[Unit] = gui.SwingUI.start()
  def stop: IO[Unit] = gui.SwingUI.stop()
  implicit def interpreter(implicit log: Logger): InteractOp ~> IO =
    UI.LoggingInterpreter(log, logPrints = true, gui.SwingUI.Interpreter)
}
case object Fx extends UI {
  def start: IO[Unit] = gui.FxUI.start()
  def stop: IO[Unit] = gui.FxUI.stop()
  implicit def interpreter(implicit log: Logger): InteractOp ~> IO =
    UI.LoggingInterpreter(log, logPrints = true, gui.FxUI.Interpreter)
}
