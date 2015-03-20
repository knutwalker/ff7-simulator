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

import algebra._
import battle.{Team, Person, MonsterAttack, BattleAttack}

trait SimpleAi extends AI {
  def attack[F[_] : Random]: Effect[F, MonsterAttack]

  def attack[F[_] : Random](self: Monster): Effect[F, MonsterAttack] =
    attack

  def target[F[_] : Random](targets: Team): Effect[F, Person] =
    Effect.oneOfL(targets.toNel)

  def modify(self: Monster): Monster =
    self

  final def apply[F[_] : Random](self: Monster, targets: Team): Effect[F, BattleAttack] = {
    val tar = target(targets)
    val att = attack(self)
    val mon = modify(self)
    tar.flatMap(t ⇒ att.map(a ⇒ mon.attacks(t, a)))
  }

  def setup[F[_] : Random](self: Monster): Effect[F, Monster] =
    Effect.point(self)

  implicit protected def liftInteract[F[_], A](a: A): Effect[F, A] =
    Effect.point(a)
}
