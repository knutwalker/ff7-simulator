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

import algebra._, Interact._
import battle.{BattleField, Team}
import monsters.Monster

import scalaz._
import Scalaz._
import effect.{IO, SafeApp}

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.Predef._

object Main extends SafeApp {
  private implicit val logger = Logger(LoggerFactory.getLogger(Simulation.getClass))

  override def runl(args: List[String]): IO[Unit] = for {
    o ← Args.parse(args)
    _ ← o.ui.start
    _ ← runRoundsIO(o.repetitions, o.ui, o.enemies)
    _ ← o.ui.stop
  } yield ()

  def party = Characters.team("cloud2", "barret").toInteract

  def program(field: BattleField): Interact[BattleField] = for {
    _      ← debug("Starting simulation")
    _      ← showMessage(s"Starting a new round with $field")
    result ← Simulation(field)
    _      ← showMessage(s"Finished round after ${result.history.size} turns")
    _      ← debug("Simulation finished")
  } yield result

  def runRoundsIO(repetitions: Int, ui: UI, enemies: Interact[Team]): IO[RoundState] = {
    import ui.interpreter
    runRounds(repetitions, enemies).run[IO]
  }

  def runRounds(repetitions: Int, enemies: Interact[Team]): Interact[RoundState] = {
    for {
      p  ← party
      rs ← runAllRoundsState(repetitions, enemies).eval(RoundState(p, 0, 0))
      _  ← Interact.info(s"Simulation finished after ${rs.rounds} rounds and ${rs.turns} turns in total.")
    } yield rs
  }

  def runAllRoundsState(repetitions: Int, enemies: Interact[Team]): StateT[Interact, RoundState, RoundState] =
    StateT.stateTMonadState[RoundState, Interact]
      .iterateUntil(runRoundState(enemies)) { rs ⇒
      rs.rounds >= repetitions || rs.heroes.alive.isEmpty
    }

  def runRoundState(enemies: Interact[Team]): StateT[Interact, RoundState, RoundState] =
    StateT(rs ⇒ for {
      es ← enemies
      f  ← runRound(BattleField.init(rs.heroes, es))
    } yield {
      val team = if (f.enemies.isHero) f.enemies else f.heroes
      RoundState(team, rs.rounds + 1, rs.turns + f.history.size).squared
    })

  def runRound(field: BattleField): Interact[BattleField] =
    program(field)

  case class RoundState(heroes: Team, rounds: Int, turns: Int)

  object Args {

    def parse(args: List[String]): IO[Config] =
      Options.parse(args)

    case class Config(ui: UI, enemies: Interact[Team], repetitions: Int)
    case class Options(ui: Option[String], repetitions: Int, monsters: List[(String, Int)])
    object Options {
      val empty = Options(None, 1, Nil)
      val uis = List("console", "gui", "fx")

      val parser = new scopt.OptionParser[Options]("ff7") {
        var _writer = Writer[Vector[String], Unit](Vector.empty, ())

        opt[Int]('r', "repetitions") action { (r, o) ⇒
          o.copy(repetitions = r)
        } validate { r ⇒
          if (r > 0) success
          else failure("repetitions must be > 0")
        }

        opt[String]('u', "ui") action { (u, o) ⇒
          o.copy(ui = Some(u.toLowerCase))
         } validate { x ⇒
          if (Options.uis.contains(x.toLowerCase)) success
          else failure(s"UI must be one of [${Options.uis.mkString(" | ")}]")
        }

        opt[Map[String, Int]]('m', "monster") unbounded() action { (m, o) ⇒
          o.copy(monsters = o.monsters ::: m.toList)
        } validate { case ms ⇒
          ms.toList.traverse_[({type l[x] = Either[String, x]})#l] { case (m, c) ⇒
            if (c > 0 && c <= 5) {
              val monster = Monsters.selectDynamic(m)
              monster.fold(es ⇒ failure(es.list.mkString(", ")), m ⇒ success)
            }
            else failure("count must be > 0 and <= 5")
          }
        }
      }

      def defaultEnemies =
        Encounters.midgar1.traverse[Interact, NonEmptyList[String], Team] { es ⇒
          for {
            e ← Interact.oneOfL(es)
            g ← Interact.oneOfL(e.groups)
          } yield g.monsters
        }.flatMap(_.toInteract)

      def parse(args: List[String]): IO[Config] = IO {
        parser.parse(args, empty).fold(sys.exit(-1)) { o ⇒
          val team = o.monsters.toNel match {
            case Some(monsterConfigs) ⇒
              monsterConfigs.traverse1[Val, NonEmptyList[Monster]] { case (monsterName, count) ⇒
                Monsters.selectDynamic(monsterName).map { monster ⇒
                  if (count > 1) {
                    Iterator.from(0)
                      .map(o => 'A' + o)
                      .map(_.toChar)
                      .take(count)
                      .map(suffix ⇒ monster.copy(name = s"${monster.name} $suffix"))
                      .toList.toNel.get
                  } else {
                    NonEmptyList(monster)
                  }
                }
              }.map(_.flatMap(x ⇒ x))
                .map(ms => Team(ms.head, ms.tail, None))
                .toInteract
            case None ⇒ defaultEnemies
          }
          val ui = o.ui collect {
            case "gui" ⇒ GUI
            case "fx"  ⇒ Fx
          } getOrElse Console
          Config(ui, team, o.repetitions)
        }
      }
    }
  }
}
