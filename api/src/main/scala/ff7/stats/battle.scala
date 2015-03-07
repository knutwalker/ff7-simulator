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
package stats

import spire.math.Rational

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

final case class Power(x: Rational) extends AnyVal
