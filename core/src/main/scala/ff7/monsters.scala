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

import scalaz._, Scalaz._
import shapeless.contrib.scalaz._
import Maybe._
import effect.IO

import com.nicta.rng.Rng
import spire.math.Rational

object monsters {
  val mp = {
    val machineGun = MonsterAttack("Machine Gun",
      empty, Physical,
      Power(Rational(1)),
      AttackPercent(100))
    val tonfa = MonsterAttack("Tonfa",
      empty, Physical,
      Power(Rational(3, 2)),
      AttackPercent(85))
    object ai extends AI {
      def apply(self: Monster, heroes: Team, targets: Team) = {
        val attack =
//          if (heroes.rowPosition(self) == FrontRow)
          if (true)
            AI.choose(1, 2, tonfa, machineGun)
          else
            AI.choose(1, 6, tonfa, machineGun)
        val target = Rng.oneofL(targets.persons)
        attack.flatMap(a ⇒ target.map(t ⇒ self.attacks(t, a))).run
      }
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
    val bite = MonsterAttack("Bite",
      empty, Physical,
      Power(Rational(1)),
      AttackPercent(100)
    )
    val tentacle = MonsterAttack("Tentacle",
      empty, Physical,
      Power(Rational(3, 2)),
      AttackPercent(90)
    )
    object ai extends AI {
      def apply(self: Monster, heroes: Team, targets: Team) = {
        val attack = AI.choose(1, 3, tentacle, bite)
        val target = targets.persons.minimumBy1(_.hp)
        attack.map(self.attacks(target, _)).run
      }
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
  val firstRay = {
    val laserCannon = MonsterAttack("Laser Cannon",
      empty, Physical,
      Power(Rational(1)),
      AttackPercent(100)
    )
    object ai extends AI {
      private val count = 0
      def apply(self: Monster, heroes: Team, targets: Team) = {
        val target = if (count == 0) {
          targets.persons.maximumBy1(_.hp)
        } else {
          // TODO: no attack
          targets.persons.maximumBy1(_.hp)
        }
        IO(self.attacks(target, laserCannon))
      }
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
  val grunt = {
    val handClaw = MonsterAttack("Handclaw",
      empty, Physical,
      Power(Rational(1)),
      AttackPercent(100)
    )
    val beamGun = MonsterAttack("Beam Gun",
      empty, Physical,
      Power(Rational(9, 8)),
      AttackPercent(100)
    )
    object ai extends AI {
      def apply(self: Monster, heroes: Team, targets: Team) = {
        // if (heroes.rowPosition(self) == FrontRow)
        val attack = if (true) {
          AI.choose(1, 2, beamGun, handClaw)
        } else {
          AI.choose(1, 12, handClaw, beamGun)
        }
        val target = Rng.oneofL(targets.persons)
        attack.flatMap(a ⇒ target.map(t ⇒ self.attacks(t, a))).run
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
  val monoDrive = {
    val drillDrive = BattleAttack("Drilldrive",
      empty, Physical,
      Power(Rational(1)),
      AttackPercent(100)
    )
    val fire = BattleAttack("Fire",
      just(4), Physical, // Magical
      Power(Rational(1, 2)),
      AttackPercent(100)
    )
    object ai extends AI {
      def apply(self: Monster, heroes: Team, targets: Team): IO[BattleAction] = {
        IO(???)
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
  val sweeper = {
    val machineGun = MonsterAttack("Machine Gun",
      empty, Physical,
      Power(Rational(1)),
      AttackPercent(100)
    )
    val doubleMachineGun = MonsterAttack("W Machine Gun",
      empty, Physical,
      Power(Rational(7, 4)),
      AttackPercent(100)
    )
    val smokeShot = MonsterAttack("Smoke Shot",
      empty, Physical,
      Power(Rational(3, 2)),
      AttackPercent(75)
    )

    lazy val state1: AI = new AI {
      def apply(self: Monster, heroes: Team, targets: Team): IO[BattleAttack] = {
        val target = Rng.oneofL(targets.persons)
        target.run.map(t ⇒ self.copy(ai = state2).attacks(t, smokeShot))
      }
    }

    lazy val state2: AI = new AI {
      def apply(self: Monster, heroes: Team, targets: Team): IO[BattleAttack] = {
        val target = targets.persons.minimumBy1(_.hp)
        IO(self.copy(ai = state3).attacks(target, machineGun))
      }
    }

    lazy val state3: AI = new AI {
      def apply(self: Monster, heroes: Team, targets: Team): IO[BattleAttack] = {
        val target = targets.persons.minimumBy1(_.hp)
        IO(self.copy(ai = state1).attacks(target, doubleMachineGun))
      }
    }

    object ai extends AI {
      override def setup(self: Monster) = {
        Rng.chooseint(0, 3).map {
          case 0 ⇒ self.copy(ai = state1)
          case 1 ⇒ self.copy(ai = state2)
          case _ ⇒ self.copy(ai = state3)
        }.run
      }
      def apply(self: Monster, heroes: Team, targets: Team): IO[BattleAttack] = {
        IO.throwIO(new IllegalStateException("setup routine did not run"))
      }
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
