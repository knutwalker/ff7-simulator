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
import battle.{BattleAttack, MonsterAttack, Team}
import stats.{AttackPercent, MP, Power}

import scalaz._
import Scalaz._

import shapeless.contrib.scalaz._
import spire.math.Rational

package reactor1 {

  object Mp extends SimpleAi {
    val machineGun = MonsterAttack.physical("Machine Gun")
    val tonfa = MonsterAttack.physical("Tonfa",
      AttackPercent(85), Power(Rational(3, 2)))
    def attack[F[_] : Random] =
      if (true) // if (heroes.rowPosition(self) == FrontRow)
        Effect.choose(1, 2, tonfa, machineGun)
      else
        Effect.choose(1, 6, tonfa, machineGun)
  }

  object GuardHound extends SimpleAi {
    val bite = MonsterAttack.physical("Bite")
    val tentacle = MonsterAttack.physical("Tentacle",
      AttackPercent(90), Power(Rational(3, 2)))
    def attack[F[_] : Random] = Effect.choose(1, 3, tentacle, bite)
    override def target[F[_] : Random](targets: Team) = targets.toNel.minimumBy1(_.hp)
  }


  object MonoDrive extends AI {
    val drillDrive = MonsterAttack.physical("Drilldrive")
    val fire = MonsterAttack.magical("Fire",
      MP(4), power = Power(Rational(1, 2)))

    def setup[F[_] : Random](self: Monster): Effect[F, Monster] =
      Effect.point(self)

    def apply[F[_] : Random](self: Monster, targets: Team): Effect[F, BattleAttack] =
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

  object Grunt extends SimpleAi {
    val handClaw = MonsterAttack.physical("Handclaw")
    val beamGun = MonsterAttack.physical("Beam Gun",
      power = Power(Rational(9, 8)))
    def attack[F[_] : Random] = if (true) { // if (heroes.rowPosition(self) == FrontRow)
      Effect.choose(1, 2, beamGun, handClaw)
    } else {
      Effect.choose(1, 12, handClaw, beamGun)
    }
  }

  object FirstRay extends SimpleAi {
    val laserCannon = MonsterAttack.physical("Laser Cannon")
    private val count = 0
    override def target[F[_] : Random](targets: Team) = {
      if (count == 0) {
        targets.toNel.maximumBy1(_.hp)
      } else {
        // TODO: no attack
        targets.toNel.maximumBy1(_.hp)
      }
    }
    def attack[F[_] : Random] = laserCannon
  }

  object Sweeper extends AI {
    val machineGun = MonsterAttack.physical("Machine Gun")

    val doubleMachineGun = MonsterAttack.physical("W Machine Gun",
      power = Power(Rational(7, 4)))
    val smokeShot = MonsterAttack.physical("Smoke Shot",
      AttackPercent(75), Power(Rational(3, 2)))

    lazy val state1: AI = new SimpleAi {
      def attack[F[_] : Random] = smokeShot
      override def modify(self: Monster): Monster = self.copy(ai = state2)
    }

    lazy val state2: AI = new SimpleAi {
      def attack[F[_] : Random] = machineGun
      override def target[F[_] : Random](targets: Team) = targets.toNel.minimumBy1(_.hp)
      override def modify(self: Monster): Monster = self.copy(ai = state3)
    }

    lazy val state3: AI = new SimpleAi {
      def attack[F[_] : Random] = doubleMachineGun
      override def target[F[_] : Random](targets: Team) = targets.toNel.minimumBy1(_.hp)
      override def modify(self: Monster): Monster = self.copy(ai = state1)
    }

    override def setup[F[_] : Random](self: Monster) =
      Effect.chooseInt(0, 3).map {
        case 0 ⇒ self.copy(ai = state1)
        case 1 ⇒ self.copy(ai = state2)
        case _ ⇒ self.copy(ai = state3)
      }

    def apply[F[_] : Random](self: Monster, targets: Team): Effect[F, BattleAttack] =
      BattleAttack.none.effect
  }
}