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

import algebra.Interact.printString
import algebra._
import battle.{BattleField, Team}
import characters.Characters
import monsters.midgar1.reactor1._

import scalaz._
import Scalaz._
import effect.{IO, SafeApp}

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import util.Try

object Main extends SafeApp {
  val logger = Logger(LoggerFactory.getLogger(Simulation.getClass))

  val party = Team(Characters.cloud2, Characters.barret)
  val enemies = Team(sweeper.copy(name = "Sweeper A"), sweeper.copy(name = "Sweeper B"))
  val field = BattleField.init(party, enemies)

  lazy val program = for {
    _      ← printString(s"Starting a new round with $field")
    result ← Simulation(field)
    _      ← printString(s"Finished round after ${result.history.size} turns")
  } yield result

  override def runl(args: List[String]): IO[Unit] = {
    val Options(repetitions, ui) = parseOpts(args)
    ui.start >> runRounds(repetitions, ui) >> ui.stop
  }

  def runRounds(repetitions: Int, ui: UI): IO[Unit] =
    List.fill(repetitions)(()).traverse_(_ ⇒ runRound(ui))

  def runRound(ui: UI): IO[Unit] = {
    import ui.interpreter
    log("Starting simulation") >> Interact.run(program) >> log("Simulation finished")
  }

  private def parseOpts(args: List[String]): Options =
    args.foldLeft(Options(1, GUI)) { (opts, arg) ⇒
      Try(java.lang.Integer.parseInt(arg)).toOption
        .map(r ⇒ opts.copy(repetitions = r))
        .orElse(Some(arg.toLowerCase).collect {
          case "console" ⇒ opts.copy(ui = Console)
          case "gui"     ⇒ opts.copy(ui = GUI)
        })
        .getOrElse(opts)
    }

  private def log(x: ⇒ String): IO[Unit] = {
    IO(logger.info(x))
  }

  case class Options(repetitions: Int, ui: UI)

  sealed trait UI {
    def start: IO[Unit]
    def stop: IO[Unit]
    implicit def interpreter: InteractOp ~> IO
  }
  case object Console extends UI {
    def start: IO[Unit] = IO(())
    def stop: IO[Unit] = IO(())
    implicit val interpreter: InteractOp ~> IO = console.ConsoleInterpreter
  }
  case object GUI extends UI {
    def start: IO[Unit] = IO(gui.start())
    def stop: IO[Unit] = IO(gui.stop())
    implicit val interpreter: InteractOp ~> IO = new (InteractOp ~> IO) {
      private val delegate = gui.GuiInterpreter
      def apply[A](fa: InteractOp[A]): IO[A] = fa match {
        case PrintString(s)      ⇒ log(s) >> delegate(fa)
        case _                   ⇒ delegate(fa)
      }
    }
  }
}
