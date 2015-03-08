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

import algebra.{Input, Interact, OutPerson}
import Interact._
import battle._
import characters.Character
import monsters._
import simulation._

import scalaz._
import Maybe._
import Scalaz._
import std.list

import math.{max, min}

object Simulation {
  type IOState[s, a] = StateT[Interact, s, a]

  def apply(field: BattleField): Interact[BattleField] =
    playAllRoundsSSS.eval(field)

  private val chooseAttackerSSS: State[BattleField, Option[Person]] = State { (bf: BattleField) ⇒
    (bf.copy(heroes = bf.heroes.cycle), bf.heroes.alive)
  }

  private val runAttackSSS: StateT[Interact, BattleField, BattleResult] =
    chooseAttackerSSS.mapK[Interact, BattleResult, BattleField] {
      case (bf, Some(a)) ⇒ attack(a, bf.enemies).map(b ⇒ (bf, b))
      case (bf, None)    ⇒ unit(BattleResult.none).map(b ⇒ (bf, b))
    }

  private val playRoundSSS: StateT[Interact, BattleField, BattleField] = {
    runAttackSSS.mapK[Interact, BattleField, BattleField] { _.map {
      case (bf, m@BattleResult.Attack(originalAttacker, attacker, target, hit)) ⇒
        val enemies = bf.enemies.updated(target.asPerson, target.asPerson.hit(hit))
        val heroes = bf.heroes.updated(originalAttacker, attacker.asPerson)
        bf.round(m).copy(heroes = enemies, enemies = heroes)
      case (bf, m@BattleResult.None)                                            ⇒
        bf.round(m)
      case (bf, m@BattleResult.Aborted)                                         ⇒
        bf.round(m).copy(aborted = true)
    }.map(_.squared) }
  }

  private val setupPerson: Person ⇒ Interact[Person] = {
    case m: Monster ⇒ m.ai.setup(m).map(_.asPerson)
    case x          ⇒ unit(x)
  }

  private val initiateRound =
    StateT[Interact, BattleField, BattleField] { b ⇒
      val hsi = b.heroes.toNel.traverse[Interact, Person](setupPerson)
      val esi = b.enemies.toNel.traverse[Interact, Person](setupPerson)
      hsi.flatMap { hs ⇒
        esi.map { es ⇒
          val h = hs.toTeam.copy(originalStart = b.heroes.originalStart)
          val e = es.toTeam.copy(originalStart = b.enemies.originalStart)
          b.copy(heroes = h, enemies = e).squared
        }
      }
    }

  private val battleMonad = MonadState[IOState, BattleField]

  private val playAllRoundsSSS: StateT[Interact, BattleField, BattleField] = {
    initiateRound >> battleMonad.iterateUntil(playRoundSSS)(_.isFinished)
  }

  private def attack(attacker: Person, opponents: Team): Interact[BattleResult] = {
    chooseAttack(attacker, opponents).flatMap {
      case BattleAttack.Attack(x, t) ⇒ executeAttack(attacker, x, t)
      case BattleAttack.None         ⇒ unit(BattleResult.none)
      case BattleAttack.Abort        ⇒ unit(BattleResult.aborted)
    }
  }

  private def chooseAttack(attacker: Person, opponents: Team): Interact[BattleAttack] = {
    attacker match {
      case c: Character ⇒
        list.toNel(opponents.alivesInOrder)
          .fold(unit(BattleAttack.none))(selectPerson(c))

      case m: Monster ⇒
        val alive = opponents.alives
        alive.headOption
          .map(a ⇒ m.ai(m, opponents.copy(first = a, rest = alive.tail)))
          .getOrElse(unit(BattleAttack.None))
    }
  }

  private def selectPerson(a: Attacker)(persons: NonEmptyList[Person]): Interact[BattleAttack] =
    printString(s"$a: Choose your enemy") >>= { _ ⇒
      readEnemy(persons).map { mp ⇒
        mp.cata(t ⇒ BattleAttack(a, t.asTarget), BattleAttack.abort)
      }
    }

  private def readEnemy(persons: NonEmptyList[Person], current: Int = 0): Interact[Maybe[Person]] = {
    val bounded = min(max(0, current), persons.size - 1)
    printEnemies(persons.list, bounded).flatMap {
      case Input.Quit ⇒ unit(empty[Person])
      case Input.Ok   ⇒ unit(just(persons.list(bounded)))
      case Input.Up   ⇒ readEnemy(persons, bounded - 1)
      case Input.Down ⇒ readEnemy(persons, bounded + 1)
      case _          ⇒ readEnemy(persons, bounded)
    }
  }

  private def printEnemies(persons: List[Person], current: Int): Interact[Input] = for {
    _ ← printPersons(formatEnemies(persons, current))
    i ← readInput
  } yield i

  private def formatEnemies(persons: List[Person], current: Int): List[OutPerson] = persons
      .zipWithIndex
      .map(px ⇒ OutPerson(px._1.toString, px._2 == current))

  private def executeAttack(originalAttacker: Person, attacker: Attacker, target: Target): Interact[BattleResult] = {
    attacker.chosenAttack.formulaType match {
      case FormulaType.Physical ⇒
        random(formulas.Physical(attacker, target))
          .map(BattleResult(originalAttacker, attacker, target, _))
    }
  }
}
