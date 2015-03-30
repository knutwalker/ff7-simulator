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

import battle.BattleAttack
import stats.{MP, Power, AttackPercent}

import spire.math.Rational

object Attacks {
  val machineGun =
    BattleAttack.physical("Machine Gun")

  val tonfa =
    BattleAttack.physical("Tonfa", AttackPercent(85), Power(Rational(3, 2)))

  val bite =
    BattleAttack.physical("Bite")

  val tentacle =
    BattleAttack.physical("Tentacle", AttackPercent(90), Power(Rational(3, 2)))

  val drillDrive =
    BattleAttack.physical("Drilldrive")

  val fire =
    BattleAttack.magical("Fire", MP(4), power = Power(Rational(1, 2)))

  val handClaw =
    BattleAttack.physical("Handclaw")

  val beamGun =
    BattleAttack.physical("Beam Gun", power = Power(Rational(9, 8)))

  val laserCannon =
    BattleAttack.physical("Laser Cannon")

  val doubleMachineGun =
    BattleAttack.physical("W Machine Gun", power = Power(Rational(7, 4)))

  val smokeShot =
    BattleAttack.physical("Smoke Shot", AttackPercent(75), Power(Rational(3, 2)))
}
