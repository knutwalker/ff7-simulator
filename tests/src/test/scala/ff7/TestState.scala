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

import interact.InteractOp

import scalaz._

import algebras._, Algebras._

trait TestState {

  type TestState[A] = State[TestWorld, A]
  type IR[A] = Coproduct[InteractOp, RandomOp, A]

  val testMonad = Monad[TestState]

  def runR[A](i: Effect[RandomOp, A]): (A, A) = {
    val x = i.run(ChooseTestInterpreter((x, y) ⇒ x), testMonad).evalZero
    val y = i.run(ChooseTestInterpreter((x, y) ⇒ y), testMonad).evalZero
    (x, y)
  }

  def runRR[A](i: Effect[RandomOp, A])(f: (Int, Int) => Int): TestState[A] =
    i.run(ChooseTestInterpreter(f), testMonad)

  def runIR[A](i: Effect[IR, A])(f: (Int, Int) => Int): TestState[A] =
    i.run(TestInterpreter or ChooseTestInterpreter(f), testMonad)

  implicit val TestInterpreter: InteractOp ~> TestState = new (InteractOp ~> TestState) {
    def apply[A](fa: InteractOp[A]): TestState[A] = fa match {
      case InteractOp.ReadInput         ⇒ State(_.input)
      case p@InteractOp.ShowMessage(_)  ⇒ State(_.print(p))
      case p@InteractOp.ShowItems(_, _) ⇒ State(_.list(p))
    }
  }

  implicit val RandomInterpreter: RandomOp ~> TestState = new (RandomOp ~> TestState) {
    def apply[A](fa: RandomOp[A]): TestState[A] = fa match {
      case RandomOp.NextInt(u) ⇒ State.state(0)
    }
  }

  def ChooseTestInterpreter(f: (Int, Int) => Int): RandomOp ~> TestState = new (RandomOp ~> TestState) {
    def apply[A](fa: RandomOp[A]): TestState[A] = fa match {
      case RandomOp.NextInt(u) ⇒ State.state(f(0, u - 1))
    }
  }

  implicit def irInterpreter(implicit I: InteractOp ~> TestState, R: RandomOp ~> TestState): IR ~> TestState =
    I or R
}

object TestState extends TestState
