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

import ff7.battle.{Person, Team}

import scalaz.Isomorphism._
import scalaz._

import com.typesafe.config.{ConfigObject, ConfigValue}

import util.Try


package object ff7 {
  type TeamF[x] = Team

  val teamNelIso: Team <=> NonEmptyList[Person] =
    new (Team <=> NonEmptyList[Person]) {
      val to: (Team) ⇒ NonEmptyList[Person] = t ⇒ {
        val (first, rest) = t.inOrder
        NonEmptyList.nel(first, rest)
      }
      val from: (NonEmptyList[Person]) ⇒ Team = nel ⇒ Team(nel.head, nel.tail, None)
    }

  implicit class NelTeam(val t: Team) extends AnyVal {
    def toNel: NonEmptyList[Person] = teamNelIso.to(t)
  }

  implicit class TeamNel(val ps: NonEmptyList[Person]) extends AnyVal {
    def toTeam: Team = teamNelIso.from(ps)
  }

  implicit class CastConfigValue(val v: ConfigValue) extends AnyVal {
    def apply[T](implicit T: Caster[T]): Try[T] = T.cast(v)
  }

  implicit class CastConfigObject(val v: ConfigObject) extends AnyVal {
    def apply(key: String): ConfigValue = v.get(key)
  }

  private[this] def justMessage[A, F[_, _]: Bifunctor](f: F[Throwable, A]): F[String, A] =
    Bifunctor[F].leftMap(f)(_.getMessage)

  private[ff7] def TryE[A](f: ⇒ A): String \/ A =
    justMessage(\/.fromTryCatchNonFatal(f))

  implicit final class DisjunctionOps[E, A](val e: E \/ A) extends AnyVal {
    def nel: ValidationNel[E, A] = e.validation.toValidationNel
  }

}
