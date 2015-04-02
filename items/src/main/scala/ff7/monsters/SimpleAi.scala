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

import battle.{BattleAction, BattleAttack, Person, Team}
import monsters.Ai.NoSetup

import algebras._

trait SimpleAi extends Ai with NoSetup {
  def attack[F[_] : Random]: Effect[F, BattleAttack]

  def target[F[_] : Random](targets: Team): Effect[F, Person]

  def modify(self: Monster): Monster

  final def apply[F[_] : Random](self: Monster, targets: Team): Effect[F, BattleAction] = {
    val tar = target(targets)
    val att = attack
    val mon = modify(self)
    tar.flatMap(t ⇒ att.map(a ⇒ mon.attacks(t, a)))
  }

  implicit protected def liftInteract[F[_], A](a: A): Effect[F, A] =
    Effect.point(a)
}

trait RandomTarget { self: SimpleAi ⇒
  def target[F[_] : Random](targets: Team): Effect[F, Person] =
    Random.oneOfL(targets.toNel)
}

trait StatelessAi { self: SimpleAi ⇒
  def modify(self: Monster): Monster =
    self
}
