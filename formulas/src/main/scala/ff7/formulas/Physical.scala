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
package formulas

import battle.{Attacker, Hit, Target}

import com.nicta.rng.Rng
import spire.math.Rational

import math._

object Physical extends Formula {

  def apply(attacker: Attacker, target: Target): Rng[Hit] =
    calculateIfHits(attacker, target) flatMap { h ⇒
      if (h) calculateDamage(attacker, target)
      else Rng.insert(Hit.missed)
    }

  private def calculateIfHits(attacker: Attacker, target: Target): Rng[Boolean] =
    calculateIfHitsPercent(attacker, target)
      .flatMap(h ⇒ percent.map(_ < h))

  private def calculateIfHitsPercent(attacker: Attacker, target: Target): Rng[Int] = {
    // Lucky Hit:
    //   The Attacker has a [Lck / 4]% chance of landing a Lucky Hit.  If this is
    //   successful, then Hit% is immediately increased to 255%.

    // Lucky Evade:
    //   The Target has a [Lck / 4]% chance of pulling off a Lucky Evade, but
    //   *ONLY* if a Lucky Hit was not pulled off.  Furthermore, it uses the same
    //   random number that was checked for Lucky Hit, so the Target's Lck must be
    //   *greater* than the Attacker's Lck to even have a chance at this.  The
    //   adjusted chance should therefore be expressed as:
    //
    //       Evade Chance = ([Target's Lck / 4] - [Attacker's Lck / 4])%.

    //   Note that only party members may obtain a Lucky Evade and only if a
    //   non-party member is attacking them - enemies will never get a Lucky Evade.
    //   If a Lucky Evade is pulled off, then the Hit% is immediately decreased
    //   to 0%.

    val hitPercent = ((attacker.dexterity / 4).x + attacker.attackPercent.x) + attacker.defensePercent.x - target.defensePercent.x
    Rng.insert(hitPercent)
  }

  private def calculateDamage(attacker: Attacker, target: Target): Rng[Hit] = {
    val damage = calculateBaseDamage(attacker, target)
    val criticalHits = calculateCritical(attacker, target)

    for {
      c ← criticalHits
      d1 ← applyCritical(c, damage)
      // TODO: rows
      d3 ← applyVariance(d1)
      d4 ← applyBounds(d3)
    } yield {
      if (c) Hit.critical(d4)
      else Hit(d4)
    }
  }

  private def calculateBaseDamage(attacker: Attacker, target: Target): Int = {
    val base = attacker.attack.x + Rational(attacker.attack.x + attacker.level.x, 32).toInt * Rational(attacker.attack.x * attacker.level.x, 32).toInt
    //    val power = attacker.power.x * base
    //    val power = attacker.power.x.toDouble
    val power = attacker.power.x * 16
    ((power * (512 - target.defense.x) * base) / (16 * 512)).toInt
  }

  private def calculateCritical(attacker: Attacker, target: Target): Rng[Boolean] = {
    val criticalPercent = Rational(attacker.luck.x + attacker.level.x - target.level.x, 4).toInt
    percent.map(_ <= criticalPercent)
  }

  private def applyCritical(critical: Boolean, damage: Int): Rng[Int] =
    Rng.insert(if (critical) damage * 2 else damage)

  private def applyVariance(d: Int): Rng[Int] =
    damageVariation.map(m ⇒ Rational(d * m, 4096).toInt)

  private def applyBounds(damage: Int): Rng[Int] =
    Rng.insert(min(9999, max(1, damage)))

  private val percent = Rng
    .chooseint(0, 65535)
    .map(i ⇒ Rational(i * 99, 65535).toInt + 1)

  private val damageVariation: Rng[Int] = Rng
    .chooseint(0, 255)
    .map(_ + 3841)
}