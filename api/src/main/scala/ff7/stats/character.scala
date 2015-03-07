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
