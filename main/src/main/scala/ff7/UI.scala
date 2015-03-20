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

import scalaz._
import scalaz.effect.IO

sealed trait UI {
  def start: IO[Unit]
  def stop: IO[Unit]
  def logShowMessages: Boolean
  implicit def interpreter: InteractOp ~> IO
}
case object Console extends UI {
  def start: IO[Unit] = IO(())
  def stop: IO[Unit] = tui.close
  def logShowMessages: Boolean = false
  implicit def interpreter: InteractOp ~> IO =
    tui.Interpreter
}
case object GUI extends UI {
  def start: IO[Unit] = gui.SwingUI.start()
  def stop: IO[Unit] = gui.SwingUI.stop()
  def logShowMessages: Boolean = true
  implicit def interpreter: InteractOp ~> IO =
    gui.SwingUI.Interpreter
}
case object Fx extends UI {
  def start: IO[Unit] = gui.FxUI.start()
  def stop: IO[Unit] = gui.FxUI.stop()
  def logShowMessages: Boolean = true
  implicit def interpreter: InteractOp ~> IO =
    gui.FxUI.Interpreter
}
