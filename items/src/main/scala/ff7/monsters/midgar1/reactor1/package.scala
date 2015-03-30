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
package midgar1

import algebra._
import battle.{BattleAction, BattleAttack, Person, Team}
import stats.MP

import scalaz._
import Scalaz._

import shapeless.contrib.scalaz._

package reactor1 {
  import Attacks._

  object Mp extends SimpleAi with RandomTarget with StatelessAi {
    def attack[F[_] : Random]: Effect[F, BattleAttack] =
      if (true) // if (heroes.rowPosition(self) == FrontRow)
        Effect.choose(1, 2, tonfa, machineGun)
      else
        Effect.choose(1, 6, tonfa, machineGun)
  }

  object GuardHound extends SimpleAi with StatelessAi {
    def attack[F[_] : Random]: Effect[F, BattleAttack] =
      Effect.choose(1, 3, tentacle, bite)

   def target[F[_] : Random](targets: Team): Effect[F, Person] =
      targets.toNel.minimumBy1(_.hp)
  }

  object MonoDrive extends Ai with NoSetup {

    def apply[F[_] : Random](self: Monster, targets: Team): Effect[F, BattleAction] =
      Effect.chance(1, 3).map { roll ⇒
        if (roll && fire.availableFor(self)) {
          val attack = fire
          val target = targets.toNel.minimumBy1(_.asTarget.magicDefense)
          self
            .copy(mp = MP(self.mp.x - fire.cost.fold(0)(_.x)))
            .attacks(target, attack)
        } else {
          val attack = drillDrive
          val target = targets.toNel.minimumBy1(_.asTarget.defense)
          self.attacks(target, attack)
        }
      }
  }

  object Grunt extends SimpleAi with RandomTarget with StatelessAi {
    def attack[F[_] : Random]: Effect[F, BattleAttack] = if (true) { // if (heroes.rowPosition(self) == FrontRow)
      Effect.choose(1, 2, beamGun, handClaw)
    } else {
      Effect.choose(1, 12, handClaw, beamGun)
    }
  }

  class FirstRay(count: Int) extends Ai with NoSetup {
    def apply[F[_] : Random](self: Monster, targets: Team): Effect[F, BattleAction] = {
      if (count == 0) {
        val tar = targets.toNel.maximumBy1(_.hp)
        val att = laserCannon
        self.copy(ai = new FirstRay(1)).attacks(tar, att).effect
      } else {
        BattleAction.change(self.copy(ai = new FirstRay(0))).effect
      }
    }
  }
  object FirstRay extends Ai with Setup {
    def setup[F[_] : Random](self: Monster): Effect[F, Monster] =
      self.copy(ai = new FirstRay(0)).effect
  }

  object Sweeper extends Ai with Setup {

    lazy val state1: Ai = new SimpleAi with RandomTarget {
      def attack[F[_] : Random]: Effect[F, BattleAttack] = smokeShot
      def modify(self: Monster): Monster = self.copy(ai = state2)
    }

    lazy val state2: Ai = new SimpleAi {
      def attack[F[_] : Random]: Effect[F, BattleAttack] = machineGun
      def target[F[_] : Random](targets: Team): Effect[F, Person] =
        targets.toNel.minimumBy1(_.hp)
      def modify(self: Monster): Monster = self.copy(ai = state3)
    }

    lazy val state3: Ai = new SimpleAi {
      def attack[F[_] : Random]: Effect[F, BattleAttack] = doubleMachineGun
      def target[F[_] : Random](targets: Team): Effect[F, Person] =
        targets.toNel.minimumBy1(_.hp)
      def modify(self: Monster): Monster = self.copy(ai = state1)
    }

    def setup[F[_] : Random](self: Monster): Effect[F, Monster] =
      Effect.chooseInt(0, 3).map {
        case 0 ⇒ self.copy(ai = state1)
        case 1 ⇒ self.copy(ai = state2)
        case _ ⇒ self.copy(ai = state3)
      }
  }
}
