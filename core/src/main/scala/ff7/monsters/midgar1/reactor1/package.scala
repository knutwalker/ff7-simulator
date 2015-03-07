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
import simulation.{AI, BattleAttack, Team, Monster, MonsterAttack}
import stats._

import scalaz._
import Maybe._
import Scalaz._

import com.nicta.rng.Rng
import shapeless.contrib.scalaz._
import spire.math.Rational

package object reactor1 {

  val mp = {
    val machineGun = MonsterAttack.physical("Machine Gun")
    val tonfa = MonsterAttack.physical("Tonfa",
      AttackPercent(85), Power(Rational(3, 2)))
    object ai extends SimpleAi {
      def attack =
        if (true) // if (heroes.rowPosition(self) == FrontRow)
          AI.choose(1, 2, tonfa, machineGun)
        else
          AI.choose(1, 6, tonfa, machineGun)
    }
    Monster("MP",
      Level(2),
      XP(16),
      HP(30), HP(30),
      MP(0), MP(0),
      Attack(6),
      Defense(4),
      DefensePercent(0),
      Dexterity(50),
      MagicAttack(0),
      MagicDefense(0),
      Luck(4),
      ai
    )
  }

  val guardHound = {
    val bite = MonsterAttack.physical("Bite")
    val tentacle = MonsterAttack.physical("Tentacle",
      AttackPercent(90), Power(Rational(3, 2)))
    object ai extends SimpleAi {
      def attack = AI.choose(1, 3, tentacle, bite)
      override def target(targets: Team) =
        targets.persons.minimumBy1(_.hp)
    }
    Monster("Guard Hound",
      Level(3),
      XP(20),
      HP(42), HP(42),
      MP(0), MP(0),
      Attack(8),
      Defense(4),
      DefensePercent(6),
      Dexterity(64),
      MagicAttack(2),
      MagicDefense(2),
      Luck(6),
      ai
    )
  }

  val monoDrive = {
    val drillDrive = MonsterAttack.physical("Drilldrive")
    val fire = MonsterAttack.physical("Fire", // Magical
      power = Power(Rational(1, 2)), cost = just(MP(4)))
    object ai extends AI {
      def setup(self: Monster): Interact[Monster] = ???
      def apply(self: Monster, heroes: Team, targets: Team): Interact[BattleAttack] = {
        ???
      }
    }
    Monster("Mono Drive",
      Level(2),
      XP(18),
      HP(28), HP(28),
      MP(28), MP(28),
      Attack(3),
      Defense(6),
      DefensePercent(6),
      Dexterity(49),
      MagicAttack(3),
      MagicDefense(4),
      Luck(2),
      ai
    )
  }

  val grunt = {
    val handClaw = MonsterAttack.physical("Handclaw")
    val beamGun = MonsterAttack.physical("Beam Gun",
      power = Power(Rational(9, 8)))
    object ai extends SimpleAi {
      def attack = if (true) { // if (heroes.rowPosition(self) == FrontRow)
        AI.choose(1, 2, beamGun, handClaw)
      } else {
        AI.choose(1, 12, handClaw, beamGun)
      }
    }
    Monster("Grunt",
      Level(7),
      XP(22),
      HP(40), HP(40),
      MP(0), MP(0),
      Attack(12),
      Defense(10),
      DefensePercent(4),
      Dexterity(58),
      MagicAttack(2),
      MagicDefense(2),
      Luck(8),
      ai
    )
  }

  val firstRay = {
    val laserCannon = MonsterAttack.physical("Laser Cannon")
    object ai extends SimpleAi {
      private val count = 0
      override def target(targets: Team) = {
        if (count == 0) {
          targets.persons.maximumBy1(_.hp)
        } else {
          // TODO: no attack
          targets.persons.maximumBy1(_.hp)
        }
      }
      def attack = laserCannon
    }
    Monster("1st Ray",
      Level(4),
      XP(12),
      HP(18), HP(18),
      MP(0), MP(0),
      Attack(10),
      Defense(2),
      DefensePercent(0),
      Dexterity(40),
      MagicAttack(0),
      MagicDefense(0),
      Luck(0),
      ai
    )
  }

  val sweeper = {
    val machineGun = MonsterAttack.physical("Machine Gun")
    val doubleMachineGun = MonsterAttack.physical("W Machine Gun",
      power = Power(Rational(7, 4)))
    val smokeShot = MonsterAttack.physical("Smoke Shot",
      AttackPercent(75), Power(Rational(3, 2)))

    lazy val state1: AI = new SimpleAi {
      def attack = smokeShot
      override def modify(self: Monster): Monster = self.copy(ai = state2)
    }

    lazy val state2: AI = new SimpleAi {
      def attack = machineGun
      override def target(targets: Team) = targets.persons.minimumBy1(_.hp)
      override def modify(self: Monster): Monster = self.copy(ai = state3)
    }

    lazy val state3: AI = new SimpleAi {
      def attack = doubleMachineGun
      override def target(targets: Team) = targets.persons.minimumBy1(_.hp)
      override def modify(self: Monster): Monster = self.copy(ai = state1)
    }

    object ai extends AI {
      override def setup(self: Monster) =
        Interact.random(Rng.chooseint(0, 3).map {
          case 0 ⇒ self.copy(ai = state1)
          case 1 ⇒ self.copy(ai = state2)
          case _ ⇒ self.copy(ai = state3)
        })
      def apply(self: Monster, heroes: Team, targets: Team): Interact[BattleAttack] =
        throw new IllegalStateException("setup routine did not run")
    }
    Monster("Sweeper",
      Level(8),
      XP(27),
      HP(140), HP(140),
      MP(0), MP(0),
      Attack(18),
      Defense(20),
      DefensePercent(0),
      Dexterity(48),
      MagicAttack(0),
      MagicDefense(4),
      Luck(1),
      ai
    )
  }
}
