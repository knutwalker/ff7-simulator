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

import InteractOp._

import scalaz._, Leibniz._

sealed trait Interact[A] {
  import Interact.InteractFree

  val free: InteractFree[A]

  def map[B](f: A ⇒ B): Interact[B] =
    Interact(free map f)

  def as[B](f: ⇒ B): Interact[B] =
    >|(f)

  def >|[B](f: ⇒ B): Interact[B] =
    map(_ ⇒ f)

  def flatMap[B](f: A ⇒ Interact[B]): Interact[B] =
    Interact(free flatMap (f(_).free))

  def >>=[B](f: A ⇒ Interact[B]): Interact[B] =
    flatMap(f)

  def andThen[B](f: ⇒ Interact[B]): Interact[B] =
    >>(f)

  def >>[B](f: ⇒ Interact[B]): Interact[B] =
    flatMap(_ ⇒ f)

  def ap[X](f: Interact[A ⇒ X]): Interact[X] =
    f.flatMap(ff ⇒ map(aa ⇒ ff(aa)))

  def zip[X](q: Interact[X]): Interact[(A, X)] =
    zipWith(q)(a ⇒ (a, _))

  def zipWith[B, C](r: Interact[B])(f: A ⇒ B ⇒ C): Interact[C] =
    r.ap(map(f))

  def |+|[AA >: A](x: Interact[AA])(implicit S: Semigroup[AA]): Interact[AA] =
    flatMap(a ⇒ x.map(b ⇒ S.append(a, b)))

  def ***[X](x: Interact[X]): Interact[(A, X)] =
    zip(x)

  def flatten[AA >: A, B](implicit f: AA === Interact[B]): Interact[B] =
    flatMap(f)

  def run[M[_]](implicit f: InteractOp ~> M, M: Monad[M]): M[A] =
    Free.runFC(free)(f)(M)
}

object Interact {

  type InteractC[A] = Coyoneda[InteractOp, A]

  type InteractFree[A] = Free[InteractC, A]

  private[algebra] def apply[A](f:InteractFree[A]): Interact[A] =
    new Interact[A] { val free = f }

  def showItems(ps: List[UiItem], id: TeamId): Interact[Unit] =
    apply(Free.liftFC(ShowItems(ps, id)))

  def showMessage(s: String): Interact[Unit] =
    apply(Free.liftFC(ShowMessage(s)))

  def readInput: Interact[Input] =
    apply(Free.liftFC(ReadInput))

  def log(s: String, l: LogLevel, ex: Option[Throwable]): Interact[Unit] =
    apply(Free.liftFC(Log(s, l, ex)))

  def chooseInt(lowerInclusive: Int, upperInclusive: Int): Interact[Int] =
    apply(Free.liftFC(ChooseInt(lowerInclusive, upperInclusive)))

  def fail[A](reason: String): Interact[A] =
    apply(Free.liftFC(Fail[A](reason)))

  def point[A](a: ⇒ A): Interact[A] =
    apply(Free.point[InteractC, A](a))

  def oneOf[A](xs: A*): Interact[A] =
    chooseInt(0, xs.size - 1) map xs

  def oneOfL[A](x: NonEmptyList[A]): Interact[A] =
    chooseInt(0, x.size - 1) map x.list

  def choose[A](parts: Int, outOf: Int, whenHit: ⇒ A, whenMiss: ⇒ A): Interact[A] =
    chance(parts, outOf).map(c ⇒ if (c) whenHit else whenMiss)

  def percent(p: Int): Interact[Boolean] =
    chance(p, 100)

  def chance(parts: Int, outOf: Int): Interact[Boolean] =
    chooseInt(1, outOf).map(_ <= parts)

  def debug(s: String): Interact[Unit] =
    log(s, LogLevel.Debug, None)

  def info(s: String): Interact[Unit] =
    log(s, LogLevel.Info, None)

  def warn(s: String): Interact[Unit] =
    log(s, LogLevel.Warn, None)

  def error(s: String, ex: Throwable): Interact[Unit] =
    log(s, LogLevel.Error, Some(ex))

  def run[A, M[_]](prog: Interact[A])(implicit f: InteractOp ~> M, M: Monad[M]): M[A] =
    prog.run[M]

  implicit val InteractMonad: Monad[Interact] =
    new Monad[Interact] {
      def bind[A, B](a: Interact[A])(f: A ⇒ Interact[B]) =
        a flatMap f
      def point[A](a: ⇒ A) =
        Interact.point(a)
    }

  implicit def InteractSemigroup[A](implicit S: Semigroup[A]): Semigroup[Interact[A]] =
    new Semigroup[Interact[A]] {
      def append(i1: Interact[A], i2: ⇒ Interact[A]) =
        i1 |+| i2
    }

  implicit def InteractMonoid[A](implicit M: Monoid[A]): Monoid[Interact[A]] =
    new Monoid[Interact[A]] {
      def append(i1: Interact[A], i2: ⇒ Interact[A]) =
        i1 |+| i2

      def zero =
        point(M.zero)
    }
}
