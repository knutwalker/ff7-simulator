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


import scalaz._, Scalaz._
import effect.{SafeApp, IO}

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import util.Try

object Main extends SafeApp {

  val logger = Logger(LoggerFactory.getLogger(Simulation.getClass))

  val party = Team(characters.cloud2, characters.barret)
  val enemies = Team(monsters.sweeper.copy(name = "Sweeper A"), monsters.sweeper.copy(name = "Sweeper B"))
  val field = BattleField(party, enemies, 0)

  def runRound(repetitions: Int): IO[Unit] = {
    val finalField = Simulation.playAllRounds.eval(field)
    log(s"Starting a new round with $field") >>
    finalField flatMap { f ⇒
      f.history.reverse.traverse_ {
        case AttackResult(oa, a, t, Missed) ⇒
          log(s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] but missed")
        case AttackResult(oa, a, t, Hits(x)) ⇒
          log(s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] and hit with $x damage")
        case AttackResult(oa, a, t, Critical(x)) ⇒
          log(s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] and hit critically with $x damage")
        case NotAttacked ⇒
          log(s"No attack happened")
      } >>
      log(s"Finished round after ${f.history.size} turns")
    }
  }

  override def runl(args: List[String]): IO[Unit] = {
    val repetitions = args.headOption.flatMap(x ⇒ Try(x.toInt).toOption).getOrElse(10)
    List.fill(repetitions)(repetitions).traverse_(runRound)
  }

  private def log(x: ⇒ String): IO[Unit] = {
    IO(logger.info(x))
  }
}
