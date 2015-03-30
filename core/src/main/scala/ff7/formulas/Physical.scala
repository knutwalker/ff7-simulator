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

import algebra.{Effect, Random}
import battle.{Attacker, Hit, Target}

import spire.math.Rational

import math._

object Physical extends Formula {

  def apply[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Hit] =
    calculateIfHits(attacker, target) flatMap { h ⇒
      if (h) calculateDamage(attacker, target)
      else   Effect.point(Hit.missed)
    }

  def calculateIfHits[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Boolean] =
    calculateIfHitsPercent(attacker, target)
      .flatMap(h ⇒ percent.map(_ < h))

  def calculateIfHitsPercent[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Int] = {
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
    Effect.point(hitPercent)
  }

  def calculateDamage[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Hit] = {
    val damage = calculateBaseDamage(attacker, target)
    val criticalHits = calculateCritical(attacker, target)

    for {
      c ← criticalHits
      d1 ← Effect.point(applyCritical(c, damage))
      // TODO: rows
      d3 ← applyVariance(d1)
      d4 ← Effect.point(applyBounds(d3))
    } yield {
      if (c) Hit.critical(d4)
      else Hit(d4)
    }
  }

  def calculateBaseDamage(attacker: Attacker, target: Target): Int = {
    val base = attacker.attack.x + Rational(attacker.attack.x + attacker.level.x, 32).toInt * Rational(attacker.attack.x * attacker.level.x, 32).toInt
    val power = attacker.power.x * 16
    ((power * (512 - target.defense.x) * base) / (16 * 512)).toInt
  }

  def calculateCritical[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Boolean] =
    percent.map(_ <= criticalPercent(attacker, target))

  def criticalPercent(attacker: Attacker, target: Target): Int =
    Rational(attacker.luck.x + attacker.level.x - target.level.x, 4).toInt

  def applyCritical(critical: Boolean, damage: Int): Int =
    if (critical) damage * 2 else damage

  def applyVariance[F[_]: Random](d: Int): Effect[F, Int] =
    damageVariation.map(m ⇒ Rational(d.toLong * m.toLong, 4096L).toInt)

  def applyBounds(damage: Int): Int =
    min(9999, max(1, damage))

  def percent[F[_]: Random]: Effect[F, Int] =
    Effect
    .chooseInt(0, 65535)
    .map(i ⇒ Rational(i * 99, 65535).toInt + 1)

  def damageVariation[F[_]: Random]: Effect[F, Int] =
    Effect
    .chooseInt(0, 255)
    .map(_ + 3841)
}
