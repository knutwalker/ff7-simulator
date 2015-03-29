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
package reactor1

import battle.MonsterAttack
import stats.{MP, Power, AttackPercent}

import spire.math.Rational

object Attacks {
  val machineGun =
    MonsterAttack.physical("Machine Gun")

  val tonfa =
    MonsterAttack.physical("Tonfa", AttackPercent(85), Power(Rational(3, 2)))

  val bite =
    MonsterAttack.physical("Bite")

  val tentacle =
    MonsterAttack.physical("Tentacle", AttackPercent(90), Power(Rational(3, 2)))

  val drillDrive =
    MonsterAttack.physical("Drilldrive")

  val fire =
    MonsterAttack.magical("Fire", MP(4), power = Power(Rational(1, 2)))

  val handClaw =
    MonsterAttack.physical("Handclaw")

  val beamGun =
    MonsterAttack.physical("Beam Gun", power = Power(Rational(9, 8)))

  val laserCannon =
    MonsterAttack.physical("Laser Cannon")

  val doubleMachineGun =
    MonsterAttack.physical("W Machine Gun", power = Power(Rational(7, 4)))

  val smokeShot =
    MonsterAttack.physical("Smoke Shot", AttackPercent(75), Power(Rational(3, 2)))
}
