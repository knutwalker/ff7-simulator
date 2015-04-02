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

import interact.InteractOp.{ShowItems, ShowMessage}
import interact.Input

import scalaz.Monoid

import algebras.LogOp.Logs

case class TestWorld(inputs: Stream[Input], printed: Vector[ShowMessage], listed: Vector[ShowItems], logged: Vector[Logs]) {
  def input: (TestWorld, Input) =
    (copy(inputs = inputs.tail), inputs.head)

  def print(s: ShowMessage): (TestWorld, Unit) =
    (copy(printed = printed :+ s), ())

  def list(s: ShowItems): (TestWorld, Unit) =
    (copy(listed = listed :+ s), ())

  def log(l: Logs): (TestWorld, Unit) =
    (copy(logged = logged :+ l), ())
}
object TestWorld {
  val empty: TestWorld = init(Stream.empty)
  def init(inputs: Stream[Input]): TestWorld =
    TestWorld(inputs, Vector.empty, Vector.empty, Vector.empty)

  implicit val testWorldMonoid: Monoid[TestWorld] = new Monoid[TestWorld] {
    val zero: TestWorld = TestWorld.empty
    def append(f1: TestWorld, f2: ⇒ TestWorld): TestWorld =
      TestWorld(f1.inputs ++ f2.inputs, f1.printed ++ f2.printed, f1.listed ++ f2.listed, f1.logged ++ f2.logged)
  }
}
