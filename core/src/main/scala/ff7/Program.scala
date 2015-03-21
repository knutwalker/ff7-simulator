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
import Effect._
import battle.{BattleField, Team}

import scalaz._
import Scalaz._


object Program {

  def runRounds[F[_]: Log: Interact: Random](
    repetitions: Int,
    enemies: Effect[F, Team])
  : Effect[F, RoundState] = {
    for {
      p  ← party
      rs ← runAllRoundsState(repetitions, enemies).eval(RoundState(p, 0, 0))
      _  ← info(s"Simulation finished after ${rs.rounds} rounds and ${rs.turns} turns in total.")
    } yield rs
  }

  def party[F[_]] =
    Characters.team("cloud2", "barret").toEffect[F]

  def runAllRoundsState[F[_]: Log: Interact: Random](
    repetitions: Int,
    enemies: Effect[F, Team])
  : StateT[({type λ[α] = Effect[F, α]})#λ, RoundState, RoundState] =
    StateT.stateTMonadState[RoundState, ({type λ[α] = Effect[F, α]})#λ]
      .iterateUntil(runRoundState[F](enemies)) { rs ⇒
      rs.rounds >= repetitions || rs.heroes.alive.isEmpty
    }

  def runRoundState[F[_]: Log: Interact: Random](enemies: Effect[F, Team])
  : StateT[({type λ[α] = Effect[F, α]})#λ, RoundState, RoundState] =
    StateT[({type λ[α] = Effect[F, α]})#λ, RoundState, RoundState](rs ⇒ for {
      es ← enemies
      f  ← runRound(BattleField.init(rs.heroes, es))
    } yield {
        val team = if (f.enemies.isHero) f.enemies else f.heroes
        RoundState(team, rs.rounds + 1, rs.turns + f.history.size).squared
      })

  def runRound[F[_]: Log: Interact: Random](field: BattleField): Effect[F, BattleField] =
    program(field)

  def program[F[_]: Log: Interact: Random](field: BattleField): Effect[F, BattleField] = for {
    _      ← debug("Starting simulation")
    _      ← showMessage(s"Starting a new round with $field")
    result ← Simulation(field)
    _      ← showMessage(s"Finished round after ${result.history.size} turns")
    _      ← debug("Simulation finished")
  } yield result

  case class RoundState(heroes: Team, rounds: Int, turns: Int)
}
