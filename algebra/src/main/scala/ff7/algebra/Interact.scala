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

import scalaz._, Scalaz._, Leibniz._, effect._

import com.nicta.rng.Rng
import spire.math.Rational

sealed trait Interact[A] {
  val free: Free.FreeC[InteractOp, A]

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

  def runIO: IO[A] =
    run[IO](Interact.defaultInterpreter, Monad[IO])

  def unsafeRun(): A =
    runIO.unsafePerformIO()
}

object Interact {
  private[algebra] def apply[A](f: Free.FreeC[InteractOp, A]): Interact[A] =
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

  def oneOf[A](xs: A*): Interact[A] =
    chooseInt(0, xs.size - 1) map xs

  def oneOfL[A](x: NonEmptyList[A]): Interact[A] =
    chooseInt(0, x.size - 1) map x.list

  def unit[A](a: ⇒ A): Interact[A] =
    apply(Free.point[({type l[a] = Coyoneda[InteractOp, a]})#l, A](a))

  def choose[A](num: Int, denom: Int, whenHit: ⇒ A, whenMiss: ⇒ A): Interact[A] =
    choose(Rational(num.toLong, denom.toLong), whenHit, whenMiss)

  def choose[A](r: Rational, whenHit: ⇒ A, whenMiss: ⇒ A): Interact[A] =
    chance(r).map(c ⇒ if (c) whenHit else whenMiss)

  def chance(num: Int, denom: Int): Interact[Boolean] =
    chance(Rational(num.toLong, denom.toLong))

  def chance(r: Rational): Interact[Boolean] =
    chooseInt(1, r.denominatorAsLong.toInt)
      .map(_ <= r.numeratorAsLong)

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
        unit(a)
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
        unit(M.zero)
    }

  private val defaultInterpreter: InteractOp ~> IO = new (InteractOp ~> IO) {
    def apply[X](fa: InteractOp[X]): IO[X] = fa match {
      case ShowItems(ps, _) ⇒ ps.traverse_(p ⇒ IO.putStrLn(p.text))
      case ShowMessage(s)   ⇒ IO.putStrLn(s)
      case ChooseInt(l, u)  ⇒ Rng.chooseint(l, u).run
      case ReadInput        ⇒ IO(Input.Ok)
      case Log(_, _, _)     ⇒ IO(())
    }
  }
}
