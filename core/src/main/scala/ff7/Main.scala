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

import algebra._, LogLevel._, Interact._
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

  def program(field: BattleField) = for {
    _      ← debug("Starting simulation")
    _      ← printString(s"Starting a new round with $field")
    result ← Simulation(field)
    _      ← printString(s"Finished round after ${result.history.size} turns")
    _      ← debug("Simulation finished")
  } yield result

  override def runl(args: List[String]): IO[Unit] = {
    val Options(repetitions, ui) = parseOpts(args)
    ui.start >> runRounds(repetitions, ui) >> ui.stop
  }

  def runRounds(repetitions: Int, ui: UI): IO[Unit] = {
    StateT.stateTMonadState[RoundState, IO]
      .iterateUntil(runRoundState(ui)) { rs ⇒
        rs.rounds >= repetitions || rs.heroes.alive.isEmpty
    }.eval(RoundState(party, 0, 0)) >>= { rs ⇒ IO {
      logger.info(s"Simulation finished after ${rs.rounds} rounds and ${rs.turns} turns in total.")
    }}
  }

  def runRoundState(ui: UI): StateT[IO, RoundState, RoundState] =
    StateT(rs ⇒ {
      runRound(ui, BattleField.init(rs.heroes, enemies))
        .map(f ⇒ {
          val team = if (f.enemies.isHero) f.enemies else f.heroes
          RoundState(team, rs.rounds + 1, rs.turns + f.history.size).squared
      })
    })

  def runRound(ui: UI, field: BattleField): IO[BattleField] = {
    import ui.interpreter
    Interact.run(program(field))
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

  case class Options(repetitions: Int, ui: UI)

  sealed trait UI {
    def start: IO[Unit]
    def stop: IO[Unit]
    implicit def interpreter: InteractOp ~> IO
  }
  case object Console extends UI {
    def start: IO[Unit] = IO(())
    def stop: IO[Unit] = IO(())
    implicit val interpreter: InteractOp ~> IO =
      LoggingInterpreter(logger, logPrints = false, console.ConsoleInterpreter)
  }
  case object GUI extends UI {
    def start: IO[Unit] = IO(gui.start())
    def stop: IO[Unit] = IO(gui.stop())
    implicit val interpreter: InteractOp ~> IO =
      LoggingInterpreter(logger, logPrints = true, gui.GuiInterpreter)
  }

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
      case PrintString(s) if logPrints ⇒ IO(log.info(s)) >> delegate(fa)
      case _                           ⇒ delegate(fa)
    }
  }

  case class RoundState(heroes: Team, rounds: Int, turns: Int)
}
