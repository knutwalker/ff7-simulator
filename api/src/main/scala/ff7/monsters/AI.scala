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
package monsters

import battle.{BattleAction, Team}

import algebras._, Algebras._


trait Ai extends {
  def setup[F[_]: Random](self: Monster): Effect[F, Monster]
  def apply[F[_]: Random](self: Monster, targets: Team): Effect[F, BattleAction]
}
object Ai {
  trait NoSetup { self: Ai ⇒
    def setup[F[_] : Random](self: Monster): Effect[F, Monster] = self.effect
  }

  trait Setup { self: Ai ⇒
    def apply[F[_] : Random](self: Monster, targets: Team): Effect[F, BattleAction] =
      BattleAction.none.effect
  }
}


