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
package simulation

import algebra.{Effect, Interact, Random}
import battle.{BattleField, Person}
import monsters.Monster

import scalaz._
import Scalaz._


class Battle[F[_]: Interact: Random] {

  type X[A] = Effect[F, A]
  type M[A] = StateT[X, BattleField, A]
  type S[A, B] = StateT[X, A, B]

  val monad: Monad[M] = Monad[M]

  def state(turn: M[BattleField]): StateT[X, BattleField, BattleField] =
    StateT[X, BattleField, BattleField](bf ⇒ initiateRound(bf).map(_.squared))
      .flatMap(_ ⇒ iterate(turn))

  def initiateRound(bf: BattleField): Effect[F, BattleField] = {
    val hsi = bf.heroes.toNel.traverse[X, Person](setupPerson)
    val esi = bf.enemies.toNel.traverse[X, Person](setupPerson)
    for {
      hs ← hsi
      es ← esi
    } yield {
      val h = hs.toTeam.copy(originalStart = bf.heroes.originalStart)
      val e = es.toTeam.copy(originalStart = bf.enemies.originalStart)
      bf.copy(heroes = h, enemies = e)
    }
  }

  def setupPerson(p: Person): Effect[F, Person] = p match {
    case m: Monster ⇒ m.ai.setup(m).map(_.asPerson)
    case x          ⇒ x.effect[F]
  }

  def iterate(turn: M[BattleField]): M[BattleField] =
    monad.iterateUntil(turn)(_.isFinished)
}

object Battle {
  def apply[F[_]: Interact: Random]: Battle[F] = new Battle[F]
}
