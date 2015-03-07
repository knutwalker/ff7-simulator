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

import algebra.Interact
import stats.BattleAttack

import com.nicta.rng.Rng
import spire.math.Rational

trait AI extends {
  def setup(self: Monster): Interact[Monster]
  def apply(self: Monster, heroes: Team, targets: Team): Interact[BattleAttack]
}
object AI {
  def choose[A](num: Long, denom: Long, ifHit: ⇒ A, ifMiss: ⇒ A): Rng[A] =
    chance(num, denom).map(c ⇒ if (c) ifHit else ifMiss)

  def choose[A](r: Rational, ifHit: ⇒ A, ifMiss: ⇒ A): Rng[A] =
    chance(r).map(c ⇒ if (c) ifHit else ifMiss)

  def chance(num: Long, denom: Long): Rng[Boolean] =
    chance(Rational(num, denom))

  def chance(r: Rational): Rng[Boolean] =
    Rng.chooselong(1L, r.denominatorAsLong)
      .map(i ⇒ i <= r.numeratorAsLong)
}
