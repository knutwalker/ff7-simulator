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

import battle._
import interact.InteractOp.ShowMessage
import interact._
import simulation.Turn

import scalaz._

import algebras.Effect
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification


object SimulationSpec extends Specification with ScalaCheck with Arbitraries with TestState {

  val effectMonad: Monad[({type λ[α] = Effect[InteractOp, α]})#λ] =
    Effect.AlgebraMonad[InteractOp]

  "ReadInput" should {
    "read the input" in prop { (in: Input) ⇒
      val ia = Interact.readInput[InteractOp]
      val testState = ia.run[TestState]
      val (finalState, input) = testState(TestWorld.init(Stream(in)))
      finalState.inputs.toList ==== List()
      input ==== in
    }

    "read all inputs" in prop { (ins: List[Input]) ⇒
      val ia = effectMonad.replicateM(ins.length, Interact.readInput)
      val testState = ia.run[TestState]
      val (finalState, inputs) = testState(TestWorld.init(ins.toStream))
      finalState.inputs.toList ==== List()
      inputs ==== ins
    }

    "read all inputs 2" in prop { (in: Input, n: Int @@ Sized) ⇒
      val ia = effectMonad.replicateM(n, Interact.readInput)
      val testState = ia.run[TestState]
      val inputs = testState.eval(TestWorld.init(Stream.continually(in)))
      inputs ==== List.fill(n)(in)
    }
  }

  "Simulation" should {
    "evaluate abort" >> {
      val bf = BattleField.init(Team(characterMonoid.zero), Team(characterMonoid.zero))
      val testState = Turn[IR].evaluateAbort(bf).run[TestState]
      val (finalState, newBf) = testState(TestWorld.init(Stream.empty))
      newBf ==== BattleField(
         heroes = bf.heroes,
        enemies = bf.enemies,
          round = 1,
        history = List(bf.copy(result = Some(BattleResult.aborted))),
        aborted = true,
         result = None)
      finalState.printed ==== Vector(ShowMessage("Attack was aborted"))
    }

    "evaluate no attack" >> {
      val bf = BattleField.init(Team(characterMonoid.zero), Team(characterMonoid.zero))
      val testState = Turn[IR].evaluateNoAttack(bf).run[TestState]
      val (finalState, newBf) = testState(TestWorld.init(Stream.empty))
      newBf ==== BattleField(
         heroes = bf.enemies,
        enemies = bf.heroes,
          round = 1,
        history = List(bf.copy(result = Some(BattleResult.none))),
        aborted = false,
         result = None)
      finalState.printed ==== Vector(ShowMessage("No attack happened"))
    }
  }
}
