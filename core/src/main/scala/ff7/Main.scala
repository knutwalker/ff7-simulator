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
import battle.{BattleResult, Hit}
import monsters.midgar1.reactor1._
import simulation.{BattleField, Team}

import scalaz._
import Scalaz._
import effect.{IO, SafeApp}

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import util.Try

object Main extends SafeApp {

  val logger = Logger(LoggerFactory.getLogger(Simulation.getClass))

  val party = Team(characters.cloud2, characters.barret)
  val enemies = Team(sweeper.copy(name = "Sweeper A"), sweeper.copy(name = "Sweeper B"))
  val field = BattleField.init(party, enemies)

  override def runl(args: List[String]): IO[Unit] = {
    val repetitions = args.headOption.flatMap(x ⇒ Try(x.toInt).toOption).getOrElse(1)
    runRounds(repetitions)
  }

  def runRounds(repetitions: Int): IO[Unit] =
    List.fill(repetitions)(repetitions).traverse_(runRound)

  def runRound(ignored: Int): IO[Unit] = {
    import console.ConsoleInterpreter
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
}
