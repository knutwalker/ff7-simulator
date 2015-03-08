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

import spire.math.Rational

import Predef.augmentString
import util.{Failure, Success, Try}

trait Caster[A] {
  def safeCast(x: AnyRef): Try[A]
  def unsafeCast(x: AnyRef): A = safeCast(x).get
}
object Caster extends LowerPriorityImplicits {
  implicit val string: Caster[String] = new Caster[String] {
    def safeCast(x: AnyRef): Try[String] = Success(String.valueOf(x))
  }
  implicit val int: Caster[Int] = new Caster[Int] {
    def safeCast(x: AnyRef): Try[Int] =
    Try(x.asInstanceOf[Int]).orElse(Try(string.unsafeCast(x).toInt))
  }
  implicit val long: Caster[Long] = new Caster[Long] {
    def safeCast(x: AnyRef): Try[Long] =
    Try(x.asInstanceOf[Long]).orElse(Try(string.unsafeCast(x).toLong))
  }
  implicit val rational: Caster[Rational] = new Caster[Rational] {
    def safeCast(x: AnyRef): Try[Rational] = {
      int.safeCast(x).map(Rational(_)).orElse(parseString(string.unsafeCast(x)))
    }
    private def parseString(x: String): Try[Rational] = {
      x.split("/") match {
        case Array(num, den) ⇒ for {
          n ← Try(num.trim.toLong)
          d ← Try(den.trim.toLong)
        } yield Rational(n, d)
        case Array(f) ⇒ Try(f.trim.toLong).map(Rational(_))
        case _        ⇒ Failure(new IllegalArgumentException(s"could not parse string $x to a Rational"))
      }
    }
  }
}
trait LowerPriorityImplicits {
  implicit def anyRef[A]: Caster[A] = new Caster[A] {
    def safeCast(x: AnyRef): Try[A] = Try(x.asInstanceOf[A])
  }
}
