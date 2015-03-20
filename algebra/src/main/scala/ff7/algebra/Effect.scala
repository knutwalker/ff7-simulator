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

package ff7.algebra

import scalaz._, Free._, Leibniz._

sealed trait Effect[F[_], A] {

  val free: FreeC[F, A]

  def map[B](f: A ⇒ B): Effect[F, B] =
    Effect(free map f)

  def as[B](f: ⇒ B): Effect[F, B] =
    >|(f)

  def >|[B](f: ⇒ B): Effect[F, B] =
    map(_ ⇒ f)

  def flatMap[B](f: A ⇒ Effect[F, B]): Effect[F, B] =
    Effect(free flatMap (f(_).free))

  def >>=[B](f: A ⇒ Effect[F, B]): Effect[F, B] =
    flatMap(f)

  def andThen[B](f: ⇒ Effect[F, B]): Effect[F, B] =
    >>(f)

  def >>[B](f: ⇒ Effect[F, B]): Effect[F, B] =
    flatMap(_ ⇒ f)

  def ap[X](f: Effect[F, A ⇒ X]): Effect[F, X] =
    f.flatMap(ff ⇒ map(aa ⇒ ff(aa)))

  def zip[X](q: Effect[F, X]): Effect[F, (A, X)] =
    zipWith(q)(a ⇒ (a, _))

  def zipWith[B, C](r: Effect[F, B])(f: A ⇒ B ⇒ C): Effect[F, C] =
    r.ap(map(f))

  def |+|[AA >: A](x: Effect[F, AA])(implicit S: Semigroup[AA]): Effect[F, AA] =
    flatMap(a ⇒ x.map(b ⇒ S.append(a, b)))

  def ***[X](x: Effect[F, X]): Effect[F, (A, X)] =
    zip(x)

  def flatten[AA >: A, B](implicit f: AA === Effect[F, B]): Effect[F, B] =
    flatMap(f)

  def runM[M[_]](f: F ~> M)(implicit M: Monad[M]): M[A] =
    run[M](f, M)

  def run[M[_]](implicit f: F ~> M, M: Monad[M]): M[A] =
    Free.runFC(free)(f)(M)
}
object Effect {
  private[algebra] def apply[F[_], A](f: FreeC[F, A]): Effect[F, A] =
    new Effect[F, A] { val free = f }

  def point[F[_], A](a: ⇒ A): Effect[F, A] =
    apply(Free.point[({type λ[α] = Coyoneda[F, α]})#λ, A](a))

  // Booooo!!!
  def fail[F[_], A](reason: String): Effect[F, A] =
    apply(Free.point[({type λ[α] = Coyoneda[F, α]})#λ, A](throw new RuntimeException(reason)))

  // Interact

  def showMessage[F[_]](s: String)(implicit I: Interact[F]): Effect[F, Unit] =
    I.showMessage(s)

  def showItems[F[_]](ps: List[UiItem], id: TeamId)(implicit I: Interact[F]): Effect[F, Unit] =
    I.showItems(ps, id)

  def readInput[F[_]](implicit I: Interact[F]): Effect[F, Input] =
    I.readInput

  // Random

  def chooseInt[F[_]](lowerInclusive: Int, upperInclusive: Int)(implicit R: Random[F]): Effect[F, Int] =
    R.chooseInt(lowerInclusive, upperInclusive)

  def oneOf[F[_], A](xs: A*)(implicit R: Random[F]): Effect[F, A] =
    R.oneOf(xs: _*)

  def oneOfL[F[_], A](x: NonEmptyList[A])(implicit R: Random[F]): Effect[F, A] =
    R.oneOfL(x)

  def choose[F[_], A](parts: Int, outOf: Int, whenHit: ⇒ A, whenMiss: ⇒ A)(implicit R: Random[F]): Effect[F, A] =
    R.choose(parts, outOf, whenHit, whenMiss)

  def percent[F[_]](p: Int)(implicit R: Random[F]): Effect[F, Boolean] =
    R.percent(p)

  def chance[F[_]](parts: Int, outOf: Int)(implicit R: Random[F]): Effect[F, Boolean] =
    R.chance(parts, outOf)

  // Log

  def log[F[_]](s: String, l: LogLevel, ex: Option[Throwable])(implicit L: Log[F]): Effect[F, Unit] =
    L.log(s, l, ex)

  def debug[F[_]](s: String)(implicit L: Log[F]): Effect[F, Unit] =
    log(s, LogLevel.Debug, None)

  def info[F[_]](s: String)(implicit L: Log[F]): Effect[F, Unit] =
    log(s, LogLevel.Info, None)

  def warn[F[_]](s: String)(implicit L: Log[F]): Effect[F, Unit] =
    log(s, LogLevel.Warn, None)

  def error[F[_]](s: String, ex: Throwable)(implicit L: Log[F]): Effect[F, Unit] =
    log(s, LogLevel.Error, Some(ex))

  // =====================

  implicit def AlgebraMonad[F[_]]: Monad[({type λ[α] = Effect[F, α]})#λ] =
    new Monad[({type λ[α] = Effect[F, α]})#λ] {

      def point[A](a: ⇒ A): Effect[F, A] =
        Effect.point(a)

      def bind[A, B](fa: Effect[F, A])(f: A ⇒ Effect[F, B]): Effect[F, B] =
        fa flatMap f
    }

  implicit def AlgebraSemigroup[F[_], A: Semigroup]: Semigroup[Effect[F, A]] =
    new Semigroup[Effect[F, A]] {

      def append(f1: Effect[F, A], f2: ⇒ Effect[F, A]): Effect[F, A] =
        f1 |+| f2
    }

  implicit def AlgebraMonoid[F[_], A: Monoid]: Monoid[Effect[F, A]] =
    new Monoid[Effect[F, A]] {

      def zero: Effect[F, A] =
        point(Monoid[A].zero)

      def append(f1: Effect[F, A], f2: ⇒ Effect[F, A]): Effect[F, A] =
        f1 |+| f2
    }
}
