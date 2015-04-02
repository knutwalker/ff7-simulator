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

package ff7.formulas

import ff7.battle.{Attacker, Target}
import ff7.{TestState, Arbitraries}

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import spire.math.Rational

object PhysicalSpec extends Specification with ScalaCheck with Arbitraries with TestState {

  "Physical formula" should {
    "damage variation" >> {
      val (x, y) = runR(Physical.damageVariation)
      x ==== 3841
      y ==== 4096
    }

    "percentage calculation" >> {
      val (x, y) = runR(Physical.percent)
      x ==== 1
      y ==== 100
    }

    "apply bound" in prop { (d: Int) ⇒
      val y = Physical.applyBounds(d)

      (y >= 1) ==== true
      (y <= 9999) ==== true
    }

    "apply variance" in prop { (d: Int) ⇒
      val (x, y) = runR(Physical.applyVariance(d))

      x ==== (Rational(3841, 4096) * d).toInt
      d ==== y
    }

    "apply critical" in prop { (d: Int, c: Boolean) ⇒
      val x = Physical.applyCritical(c, d)

      x ==== (if (c) 2*d else d)
    }

    "critical percent" in prop { (a: Attacker, t: Target) ⇒
      Physical.criticalPercent(a, t) ==== ((a.luck.x + a.level.x - t.level.x) / 4)
    }

    "calculate critical" in prop { (a: Attacker, t: Target) ⇒
      val c = Physical.criticalPercent(a, t)
      val (x, y) = runR(Physical.calculateCritical(a, t))
      x ==== (c >= 1)
      y ==== (c >= 100)
    }

    /* def calculateBaseDamage(attacker: Attacker, target: Target): Int */
    "calculate base damage" in prop { (a: Attacker, t: Target) ⇒
    }

    /* def calculateDamage[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Hit] */
    "calculate damage" in prop { (a: Attacker, t: Target) ⇒
    }

    /* def calculateIfHitsPercent[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Int] */
    "calculate if hits percent" in prop { (a: Attacker, t: Target) ⇒
    }

    /* def calculateIfHits[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Boolean] */
    "calculate if hits" in prop { (a: Attacker, t: Target) ⇒
    }

    /* def apply[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Hit] */
    "apply physical damage formula" in prop { (a: Attacker, t: Target) ⇒
    }
  }
}
