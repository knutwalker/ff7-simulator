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

import algebra._, LogLevel._, Interact._, InteractOp._
import battle.{BattleField, Team}

import scalaz._
import Scalaz._
import effect.{IO, SafeApp}

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import util.Try

object Main extends SafeApp {
  private val logger = Logger(LoggerFactory.getLogger(Simulation.getClass))

  override def runl(args: List[String]): IO[Unit] = {
    val Options(repetitions, ui) = parseOpts(args)
    for {
      _ ← ui.start
      _ ← runRoundsIO(repetitions, ui)
      _ ← ui.stop
    } yield ()
  }

  def party = Characters.team("cloud2", "barret").toInteract

  def enemies = Encounters.midgar1.traverse[Interact, NonEmptyList[String], Team] { es ⇒
    for {
      e ← Interact.oneOfL(es)
      g ← Interact.oneOfL(e.groups)
    } yield g.monsters
  }.flatMap(_.toInteract)

  def program(field: BattleField): Interact[BattleField] = for {
    _      ← debug("Starting simulation")
    _      ← showMessage(s"Starting a new round with $field")
    result ← Simulation(field)
    _      ← showMessage(s"Finished round after ${result.history.size} turns")
    _      ← debug("Simulation finished")
  } yield result

  def runRoundsIO(repetitions: Int, ui: UI): IO[RoundState] = {
    import ui.interpreter
    runRounds(repetitions).run[IO]
  }

  def runRounds(repetitions: Int): Interact[RoundState] = {
    for {
      p  ← party
      rs ← runAllRoundsState(repetitions).eval(RoundState(p, 0, 0))
      _  ← Interact.info(s"Simulation finished after ${rs.rounds} rounds and ${rs.turns} turns in total.")
    } yield rs
  }

  def runAllRoundsState(repetitions: Int): StateT[Interact, RoundState, RoundState] =
    StateT.stateTMonadState[RoundState, Interact]
      .iterateUntil(runRoundState) { rs ⇒
      rs.rounds >= repetitions || rs.heroes.alive.isEmpty
    }

  val runRoundState: StateT[Interact, RoundState, RoundState] =
    StateT(rs ⇒ for {
      es ← enemies
      f ← runRound(BattleField.init(rs.heroes, es))
    } yield {
      val team = if (f.enemies.isHero) f.enemies else f.heroes
      RoundState(team, rs.rounds + 1, rs.turns + f.history.size).squared
    })

  def runRound(field: BattleField): Interact[BattleField] =
    program(field)

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
      LoggingInterpreter(logger, logPrints = false, tui.ConsoleInterpreter)
  }
  case object GUI extends UI {
    def start: IO[Unit] = IO(gui.start())
    def stop: IO[Unit] = IO(gui.stop())
    implicit val interpreter: InteractOp ~> IO =
      LoggingInterpreter(logger, logPrints = true, gui.GuiInterpreter)
  }

  private def LoggingInterpreter(log: Logger, logPrints: Boolean, delegate: InteractOp ~> IO) = new (InteractOp ~> IO) {
    def apply[A](fa: InteractOp[A]): IO[A] = fa match {
      case Log(x, Debug, Some(ex))     ⇒ IO(log.debug(x, ex))
      case Log(x, Debug, None)         ⇒ IO(log.debug(x))
      case Log(x, Info, Some(ex))      ⇒ IO(log.info(x, ex))
      case Log(x, Info, None)          ⇒ IO(log.info(x))
      case Log(x, Warn, Some(ex))      ⇒ IO(log.warn(x, ex))
      case Log(x, Warn, None)          ⇒ IO(log.warn(x))
      case Log(x, Error, Some(ex))     ⇒ IO(log.error(x, ex))
      case Log(x, Error, None)         ⇒ IO(log.error(x))
      case ShowMessage(s) if logPrints ⇒ IO(log.info(s)) >> delegate(fa)
      case _                           ⇒ delegate(fa)
    }
  }

  case class RoundState(heroes: Team, rounds: Int, turns: Int)
}
