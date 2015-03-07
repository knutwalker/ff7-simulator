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
import simulation._
import stats.{BattleAttack, MonsterAttack, Person}

import com.nicta.rng.Rng

trait SimpleAi extends AI {
  def attack: Rng[MonsterAttack]
  def attack(self: Monster): Rng[MonsterAttack] = attack
  def target(targets: Team): Rng[Person] = Rng.oneofL(targets.persons)
  def modify(self: Monster): Monster = self

  final def apply(self: Monster, heroes: Team, targets: Team): Interact[BattleAttack] = {
    val tar = target(targets)
    val att = attack(self)
    val mon = modify(self)
    val bat = tar.flatMap(t ⇒ att.map(a ⇒ mon.attacks(t, a)))
    Interact.random(bat)
  }

  def setup(self: Monster): Interact[Monster] = Interact.unit(self)

  implicit protected def liftRng[A](a: A): Rng[A] = Rng.insert(a)
}
