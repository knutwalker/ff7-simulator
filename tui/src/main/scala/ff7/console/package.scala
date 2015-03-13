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

import algebra._
import algebra.InteractOp._

import scalaz._, Scalaz._
import effect.IO

import com.nicta.rng.Rng
import jline.console.ConsoleReader
import jline.internal.NonBlockingInputStream

package object tui {

  implicit val ConsoleInterpreter: InteractOp ~> IO = new (InteractOp ~> IO) {
    def apply[A](fa: InteractOp[A]): IO[A] = fa match {
      case ShowItems(ps, _) ⇒ printPersons(ps)
      case ShowMessage(s)   ⇒ IO.putStrLn(s"  ~~ ${Console.GREEN}$s${Console.RESET}")
      case ChooseInt(l, u)  ⇒ Rng.chooseint(l, u).run
      case ReadInput        ⇒ readInput
      case Log(_, _, _)     ⇒ IO(())
    }
  }

  private val reader = new ConsoleReader

  private def printPersons(persons: List[UiItem]): IO[Unit] =
    persons.map {
      case UiItem(p, true)  ⇒ s"${Console.BOLD}> $p${Console.RESET}"
      case UiItem(p, false) ⇒ s"  $p"
    }.traverse_(IO.putStrLn)

  private def readInput: IO[Input] =
    readsDefinitiveDirectionS.eval(())

  private val readDirectionS: StateT[IO, Unit, Option[Input]] =
    StateT((_: Unit) ⇒ readsDirection.map(x ⇒ ((), x)))

  private val readsDefinitiveDirectionS: StateT[IO, Unit, Input] =
    IndexedStateT.stateTMonadState[Unit, IO]
      .iterateUntil(readDirectionS)(_.isDefined).map(_.get)

  private def readsDirection: IO[Option[Input]] = readChar map {
    case 10 | 13 | 32   ⇒ Input.ok.some     // 10,13 = Enter, 32 = Space
    case 113            ⇒ Input.quit.some   // 113 = 1
    case 127            ⇒ Input.undo.some   // 127 = Backspace
    case 27             ⇒ Input.cancel.some // 27 = Escape
    case 68 | 104 | 97  ⇒ Input.left.some   // 68 = Left, 104 = h, 97 = a
    case 66 | 106 | 115 ⇒ Input.down.some   // 66 = Down, 106 = j, 115 = s
    case 65 | 107 | 119 ⇒ Input.up.some     // 65 = Up, 107 = k, 119 = w
    case 67 | 108 | 100 ⇒ Input.right.some  // 67 = Right, 108 = l, 100 = d
    case _              ⇒ none[Input]
  }

  private def readChar = IO {
    val c = reader.readCharacter()
    if (c == 27) {  // escape
      val nbStream = reader.getInput.asInstanceOf[NonBlockingInputStream]
      val n = nbStream.peek(15)
      if (n == 91) {
        nbStream.read()
        val m = nbStream.peek(15)
        if (m != -2) { // -2 == timeout
          nbStream.read()
        } else c
      } else c
    } else c
  }
}
