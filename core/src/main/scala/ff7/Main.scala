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

import algebra._
import battle.{BattleField, Team, BattleResult, Hit}
import monsters.midgar1.reactor1._

import scalaz._
import Scalaz._
import effect.{IO, SafeApp}

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import util.Try

object Main extends SafeApp {
  import Predef.augmentString

  val logger = Logger(LoggerFactory.getLogger(Simulation.getClass))

  val party = Team(characters.cloud2, characters.barret)
  val enemies = Team(sweeper.copy(name = "Sweeper A"), sweeper.copy(name = "Sweeper B"))
  val field = BattleField.init(party, enemies)

  override def runl(args: List[String]): IO[Unit] = {
    val Options(repetitions, ui) = parseOpts(args)
    ui.start >> runRounds(repetitions, ui) >> ui.stop
  }

  def parseOpts(args: List[String]): Options =
    args.foldLeft(Options(1, GUI)) { (opts, arg) ⇒
      Try(arg.toInt).toOption
        .map(r ⇒ opts.copy(repetitions = r))
        .orElse(Some(arg.toLowerCase).collect {
          case "console" ⇒ opts.copy(ui = Console)
          case "gui"     ⇒ opts.copy(ui = GUI)
        })
        .getOrElse(opts)
    }

  def runRounds(repetitions: Int, ui: UI): IO[Unit] =
    List.fill(repetitions)(()).traverse_(_ ⇒ runRound(ui))

  def runRound(ui: UI): IO[Unit] = {
    import ui.interpreter
    val simulation = Interact.run(runSimulation)
    log(s"Starting a new round with $field") >> simulation >>= (_.traverse_(x ⇒ log(x)))
  }

  def runSimulation: Interact[Vector[String]] =
    Simulation(field).map(resultMessages)

  def resultMessages(result: BattleField): Vector[String] = result.history.map {
    case BattleResult.Attack(oa, a, t, Hit.Missed) ⇒
      s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] but missed"
    case BattleResult.Attack(oa, a, t, Hit.Hits(x)) ⇒
      s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] and hit with $x damage"
    case BattleResult.Attack(oa, a, t, Hit.Critical(x)) ⇒
      s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] and hit critically with $x damage"
    case BattleResult.None ⇒
      "No attack happened"
    case BattleResult.Aborted ⇒
      "Attack was aborted"
  } :+ s"Finished round after ${result.history.size} turns"

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
    implicit def interpreter: InteractOp ~> IO = console.ConsoleInterpreter
  }
  case object GUI extends UI {
    def start: IO[Unit] = IO(gui.start())
    def stop: IO[Unit] = IO(gui.stop())
    implicit def interpreter: InteractOp ~> IO = gui.GuiInterpreter
  }
}
