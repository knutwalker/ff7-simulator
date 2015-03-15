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

import algebra.Interact
import battle.{Attacker, Hit, Target}

import spire.math.Rational

import math._

object Magical extends Formula {

  def apply(attacker: Attacker, target: Target): Interact[Hit] = {
    checkIfHits(attacker, target) flatMap { h ⇒
      if (h) calculateDamage(attacker, target)
      else Interact.point(Hit.missed)
    }
  }

  def checkIfHits(attacker: Attacker, target: Target): Interact[Boolean] = for {
    instantMiss ← checkMagicDefense(target)
    hits ← if (instantMiss) Interact.point(false) else checkHitsPercentage(attacker, target)
  } yield hits

  def checkMagicDefense(target: Target): Interact[Boolean] =
    Interact.percent(target.magicDefensePercent.x)

  def checkHitsPercentage(attacker: Attacker, target: Target): Interact[Boolean] = {
    val hitp = attacker.magicAttackPercent.x + attacker.level.x - (target.level.x / 2) - 1
    Interact.chooseInt(0, 99).map(_ < hitp)
  }

  def calculateDamage(attacker: Attacker, target: Target): Interact[Hit] = {
    val damage = calculateBaseDamage(attacker, target)
    for {
      d1 ← applyVariance(damage)
      d2 ← Interact.point(applyBounds(d1))
    } yield Hit(d2)
  }

  def calculateBaseDamage(attacker: Attacker, target: Target): Int = {
    val base = 6 * (attacker.magicAttack.x + attacker.level.x)
    val power = attacker.power.x * 16
    ((power * (512 - target.magicDefense.x) * base) / (16 * 512)).toInt
  }

  def applyVariance(d: Int): Interact[Int] =
    damageVariation.map(m ⇒ Rational(d.toLong * m.toLong, 4096L).toInt)

  def applyBounds(damage: Int): Int =
    min(9999, max(1, damage))

  val damageVariation: Interact[Int] = Interact
    .chooseInt(0, 255)
    .map(_ + 3841)
}
