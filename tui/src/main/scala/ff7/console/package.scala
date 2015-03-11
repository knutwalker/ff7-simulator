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

import scalaz._, Scalaz._
import effect.IO

import algebra._
import jline.console.ConsoleReader

package object tui {

  implicit val ConsoleInterpreter: InteractOp ~> IO = new (InteractOp ~> IO) {
    def apply[A](fa: InteractOp[A]): IO[A] = fa match {
      case PrintPersons(ps, _) ⇒ printPersons(ps)
      case PrintString(s)      ⇒ IO.putStrLn(s"  ~~ ${Console.GREEN}$s${Console.RESET}")
      case Random(rng)         ⇒ rng.run
      case ReadInput           ⇒ readInput
      case Log(_, _, _)        ⇒ IO(())
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

  private def readsDirection: IO[Option[Input]] = readChar flatMap {
    case 10 | 13   ⇒ IO(some(Input.Ok))
    case 113       ⇒ IO(some(Input.Quit))
    case 127       ⇒ IO(some(Input.Undo))
    case 27        ⇒ readEscapedCursor
    case 104 | 97  ⇒ IO(some(Input.Left))
    case 106 | 115 ⇒ IO(some(Input.Down))
    case 107 | 119 ⇒ IO(some(Input.Up))
    case 108 | 100 ⇒ IO(some(Input.Right))
    case _         ⇒ IO(none)
  }

  private def readEscapedCursor: IO[Option[Input]] = readChar flatMap {
    case 91 ⇒ readChar map {
      case 65 ⇒ some(Input.Up)
      case 66 ⇒ some(Input.Down)
      case 67 ⇒ some(Input.Right)
      case 68 ⇒ some(Input.Left)
      case _  ⇒ none
    }
    case _  ⇒ IO(none)
  }

  private def readChar = IO(reader.readCharacter())
}
