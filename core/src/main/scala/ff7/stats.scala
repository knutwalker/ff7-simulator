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

import scalaz.Monoid

import spire.math.Rational

// ========= Characters =================

final case class Level(x: Int) extends AnyVal
final case class HP(x: Int) extends AnyVal
final case class MP(x: Int) extends AnyVal
final case class Strength(x: Int) extends AnyVal
final case class Dexterity(x: Int) extends AnyVal {
  def /(y: Int) = Dexterity(Rational(x, y).toInt)
}
final case class Vitality(x: Int) extends AnyVal
final case class Magic(x: Int) extends AnyVal
final case class Spirit(x: Int) extends AnyVal
final case class Luck(x: Int) extends AnyVal
final case class XP(x: Int) extends AnyVal


// ========= Battle =================

final case class Attack(x: Int) extends AnyVal {
  def +(y: Int): Attack = Attack(x + y)
}
final case class AttackPercent(x: Int) extends AnyVal
final case class Defense(x: Int) extends AnyVal {
  def +(y: Int): Defense = Defense(x + y)
}
final case class DefensePercent(x: Int) extends AnyVal {
  def +(y: Int): DefensePercent = DefensePercent(x + y)
}
final case class MagicAttack(x: Int) extends AnyVal
final case class MagicDefense(x: Int) extends AnyVal {
  def +(y: Int): MagicDefense = MagicDefense(x + y)
}
final case class MagicDefensePercent(x: Int) extends AnyVal
final case class HitPercent(x: Int) extends AnyVal

// ========= Attacks =====================
final case class Power(x: Rational) extends AnyVal
object Power {
  implicit val monoid: Monoid[Rational] = new Monoid[Rational] {
    val zero: Rational = Rational.zero
    def append(f1: Rational, f2: ⇒ Rational): Rational = f1 + f2
  }
}

sealed trait AttackType
case object Physical extends AttackType
