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

  type ETeam[F[_]] = Effect[F, Val[Team]]

  def runRounds[F[_]: Log: Interact: Random](
    repetitions: Int,
    enemies: ETeam[F])
  : Effect[F, Option[RoundState]] = {
    party.flatMap { pVal ⇒
      pVal.fold(
        fail = errors ⇒ errors.traverse_[({type λ[α] = Effect[F, α]})#λ](s ⇒ Effect.warn[F](s)) as none,
        succ = { p ⇒
          runAllRoundsState(repetitions, enemies).eval(RoundState(p, 0, 0)).flatMap { rs ⇒
            info(s"Simulation finished after ${rs.rounds} rounds and ${rs.turns} turns in total.") as rs.some
          }
        }
      )
    }
  }

  def party[F[_]]: ETeam[F] =
    Characters.team("cloud2", "barret").effect[F]

  def runAllRoundsState[F[_]: Log: Interact: Random](
    repetitions: Int,
    enemies: ETeam[F])
  : StateT[({type λ[α] = Effect[F, α]})#λ, RoundState, RoundState] =
    StateT.stateTMonadState[RoundState, ({type λ[α] = Effect[F, α]})#λ]
      .iterateUntil(runRoundState[F](enemies)) { rs ⇒
      rs.rounds >= repetitions || rs.heroes.alive.isEmpty
    }

  def runRoundState[F[_]: Log: Interact: Random](enemies: ETeam[F])
  : StateT[({type λ[α] = Effect[F, α]})#λ, RoundState, RoundState] =
    StateT[({type λ[α] = Effect[F, α]})#λ, RoundState, RoundState] { rs ⇒
      enemies.flatMap { esVal ⇒
        esVal.fold(
          fail = errors ⇒ errors.traverse_[({type λ[α] = Effect[F, α]})#λ](s ⇒ Effect.warn[F](s)) as rs.squared,
          succ = { es ⇒
          runRound(BattleField.init(rs.heroes, es)).map { f =>
            val team = if (f.enemies.isHero) f.enemies else f.heroes
            RoundState(team, rs.rounds + 1, rs.turns + f.history.size).squared
          }
        })
      }
    }

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
