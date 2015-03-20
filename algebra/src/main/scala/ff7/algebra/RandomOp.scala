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
package algebra

import scalaz.{NonEmptyList, Inject}

sealed trait RandomOp[A]

object RandomOp {
  case class ChooseInt(lowerInclusive: Int, upperInclusive: Int)
    extends RandomOp[Int]

  type Injects[F[_]] = Inject[RandomOp, F]

  implicit def instance[F[_]: Injects]: Random[F] = new Random[F]
}

class Random[F[_]: RandomOp.Injects] {
  import RandomOp._

  def chooseInt(lowerInclusive: Int, upperInclusive: Int): Effect[F, Int] =
    Effect(ChooseInt(lowerInclusive, upperInclusive))

  def oneOf[A](xs: A*): Effect[F, A] =
    chooseInt(0, xs.size - 1) map xs

  def oneOfL[A](x: NonEmptyList[A]): Effect[F, A] =
    chooseInt(0, x.size - 1) map x.list

  def choose[A](parts: Int, outOf: Int, whenHit: ⇒ A, whenMiss: ⇒ A): Effect[F, A] =
    chance(parts, outOf).map(c ⇒ if (c) whenHit else whenMiss)

  def percent(p: Int): Effect[F, Boolean] =
    chance(p, 100)

  def chance(parts: Int, outOf: Int): Effect[F, Boolean] =
    chooseInt(1, outOf).map(_ <= parts)
}
