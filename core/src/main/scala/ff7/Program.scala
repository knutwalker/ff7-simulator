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

import interact.Interact
import battle.BattleField

import algebras._, Algebras._

import scalaz._
import Scalaz._


class Program[F[_]: Log: Interact: Random] {
  import Program.{RoundState, RoundsConfig}

  type X[A] = Effect[F, A]
  type M[A] = StateT[X, RoundState, A]
  type Team = X[Val[battle.Team]]

  val party: Team =
    Characters.team("cloud2", "barret").effect[F]

  val state: StateT[X, RoundsConfig[F], Option[RoundState]] =
    StateT[X, RoundsConfig[F], Option[RoundState]](cfg ⇒ runRounds(cfg.repetitions, cfg.enemies)
      .map(rs ⇒ (cfg, rs)))

  def runRounds(repetitions: Int,enemies: Team): Effect[F, Option[RoundState]] = party.flatMap { pVal ⇒
    pVal.fold(
      fail = errors ⇒ errors.traverse_[X](Log.warn[F]) as none,
      succ = { p ⇒
        runAllRoundsState(repetitions, enemies).eval(RoundState(p, 0, 0)).flatMap { rs ⇒
          Log.info(s"Simulation finished after ${rs.rounds} rounds and ${rs.turns} turns in total.") as rs.some
        }
      }
    )
  }

  def runAllRoundsState(repetitions: Int, enemies: Team): StateT[X, RoundState, RoundState] =
    Monad[M].iterateUntil(runRoundState(enemies)) { rs ⇒
      rs.rounds >= repetitions || rs.heroes.alive.isEmpty
    }

  def runRoundState(enemies: Team): StateT[X, RoundState, RoundState] =
    StateT[X, RoundState, RoundState](rs ⇒ runRound(enemies)(rs).map(_.squared))

  def runRound(enemies: Team)(rs: RoundState): Effect[F, RoundState] =
    enemies.flatMap { esVal ⇒
      esVal.fold(
        fail = errors ⇒ errors.traverse_[X](Log.warn[F]) as rs,
        succ = { es ⇒
          program(BattleField.init(rs.heroes, es)).map { f =>
          val team = if (f.enemies.isHero) f.enemies else f.heroes
          RoundState(team, rs.rounds + 1, rs.turns + f.history.size)
        }
      })
    }

  def program(field: BattleField): Effect[F, BattleField] = for {
    _      ← Log.debug("Starting simulation")
    _      ← Interact.showMessage(s"Starting a new round with $field")
    result ← Simulation(field)
    _      ← Interact.showMessage(s"Finished round after ${result.history.size} turns")
    _      ← Log.debug("Simulation finished")
  } yield result
}

object Program {

  type Team[F[_]] = Effect[F, Val[battle.Team]]

  case class RoundsConfig[F[_]](repetitions: Int, enemies: Team[F])
  case class RoundState(heroes: battle.Team, rounds: Int, turns: Int)

  def runRounds[F[_]: Log: Interact: Random](repetitions: Int, enemies: Team[F]): Effect[F, Option[RoundState]] =
    new Program[F].state.eval(RoundsConfig[F](repetitions, enemies))
}
