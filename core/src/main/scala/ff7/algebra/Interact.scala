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

import scalaz._

import com.nicta.rng.Rng


object Interact {

  implicit val monad: Monad[Interact] =
    Free.freeMonad[InteractMonad]

  def printPersons(ps: List[OutPerson]): Interact[Unit] =
    Free.liftFC(PrintPersons(ps))

  def printString(s: String): Interact[Unit] =
    Free.liftFC(PrintString(s))

  def random[A](rng: Rng[A]): Interact[A] =
    Free.liftFC(Random(rng))

  def readInput: Interact[Input] =
    Free.liftFC(ReadInput)

  def unit[A](a: A): Interact[A] =
    Free.point[InteractMonad, A](a)

  def run[A, M[_]](prog: Interact[A])(implicit f: InteractOp ~> M, M: Monad[M]): M[A] =
    Free.runFC(prog)(f)
}
