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

import scalaz._
import Validation._

package object ff7 {

  type TeamF[x] = Team

  type Val[+A] = Validation[NonEmptyList[String], A]

  private[this] def justMessage[A, F[_, _]: Bifunctor](f: F[Throwable, A]): F[String, A] =
    Bifunctor[F].leftMap(f)(_.getMessage)

  private[ff7] def TryE[A](f: ⇒ A): String \/ A =
    justMessage(\/.fromTryCatchNonFatal(f))

  private[ff7] def TryV[A](f: ⇒ A): String \?/ A =
    TryE(f).validation

  private[ff7] def TryVN[A](f: ⇒ A): Val[A] =
    TryE(f).validation.toValidationNel

  implicit class TeamNel(val ps: NonEmptyList[Person]) extends AnyVal {
    def toTeam: Team = Team(ps.head, ps.tail, None)
  }
}
