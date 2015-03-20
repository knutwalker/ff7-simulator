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

object Magical extends Formula {

  def apply[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Hit] = {
    checkIfHits(attacker, target) flatMap { h ⇒
      if (h) calculateDamage(attacker, target)
      else   Effect.point(Hit.missed)
    }
  }

  def checkIfHits[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Boolean] = for {
    instantMiss ← checkMagicDefense(target)
    hits ← if (instantMiss) Effect.point[F, Boolean](false)
           else             checkHitsPercentage(attacker, target)
  } yield hits

  def checkMagicDefense[F[_]: Random](target: Target): Effect[F, Boolean] =
    Effect.percent(target.magicDefensePercent.x)

  def checkHitsPercentage[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Boolean] = {
    val hitp = attacker.magicAttackPercent.x + attacker.level.x - (target.level.x / 2) - 1
    Effect.chooseInt(0, 99).map(_ < hitp)
  }

  def calculateDamage[F[_]: Random](attacker: Attacker, target: Target): Effect[F, Hit] = {
    val damage = calculateBaseDamage(attacker, target)
    for {
      d1 ← applyVariance(damage)
      d2 ← Effect.point(applyBounds(d1))
    } yield Hit(d2)
  }

  def calculateBaseDamage(attacker: Attacker, target: Target): Int = {
    val base = 6 * (attacker.magicAttack.x + attacker.level.x)
    val power = attacker.power.x * 16
    ((power * (512 - target.magicDefense.x) * base) / (16 * 512)).toInt
  }

  def applyVariance[F[_]: Random](d: Int): Effect[F, Int] =
    damageVariation.map(m ⇒ Rational(d.toLong * m.toLong, 4096L).toInt)

  def applyBounds(damage: Int): Int =
    min(9999, max(1, damage))

  def damageVariation[F[_]: Random]: Effect[F, Int] =
    Effect.chooseInt(0, 255)
    .map(_ + 3841)
}
