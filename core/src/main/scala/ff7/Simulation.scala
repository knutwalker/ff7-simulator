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
import simulation.{Battle, Turn}

import algebras._

class Simulation[F[_]: Interact: Random] {
  val state = Battle[F].state(Turn[F].state)
}

object Simulation {
  def apply[F[_]: Interact: Random](field: BattleField): Effect[F, BattleField] =
    new Simulation[F].state.eval(field)
}
