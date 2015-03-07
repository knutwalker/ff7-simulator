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

import com.nicta.rng.Rng

import math.{max, min}

object Simulation {
  type IOState[s, a] = StateT[Interact, s, a]

  def apply(field: BattleField): Interact[BattleField] =
    playAllRounds.eval(field)

  private val battleMonad = MonadState[IOState, BattleField]

  private val setupPerson: Person ⇒ Interact[Person] = {
    case m: Monster ⇒ m.ai.setup(m).map(_.asPerson)
    case x          ⇒ unit(x)
  }

  private val initiateRound =
    StateT[Interact, BattleField, BattleField] { b ⇒ for {
      hs ← b.heroes.toNel.traverse(setupPerson)
      es ← b.enemies.toNel.traverse(setupPerson)
    } yield b.copy(heroes = hs.toTeam, enemies = es.toTeam).squared }

  private val playRoundS =
    StateT[Interact, BattleField, BattleField](b ⇒
      playRound(b).map(_.squared))

  private val playAllRounds =
    initiateRound >> battleMonad.iterateUntil(playRoundS)(_.isFinished)

  private def playRound(battle: BattleField): Interact[BattleField] =
    runAttack(battle.heroes, battle.enemies) map {
      case m@BattleResult.Attack(originalAttacker, attacker, target, hit) ⇒
        val enemies = update(target.asPerson, _.hit(hit), battle.enemies)
        val heroes = update(originalAttacker, _ ⇒ attacker.asPerson, battle.heroes)
        battle.round(m).copy(enemies, heroes)
      case m@BattleResult.None ⇒
        battle.round(m)
      case m@BattleResult.Aborted ⇒
        battle.round(m).copy(aborted = true)
    }

  private def runAttack(attackers: Team, opponents: Team): Interact[BattleResult] =
    random(chooseAttacker(attackers, opponents)).flatMap {
      case Just(a) ⇒ chooseAttackings(a, attackers, opponents)
      case Empty() ⇒ unit(BattleResult.None)
    }

  private def chooseAttackings(attacker: Person, attackers: Team, opponents: Team): Interact[BattleResult] = {
    chooseAttack(attacker, attackers, opponents).flatMap {
      case BattleAttack.Attack(x, t) ⇒
        x.chosenAttack.formulaType match {
          case FormulaType.Physical ⇒
            random(formulas.Physical(x, t))
              .map(BattleResult(attacker, x, t, _))
        }
      case BattleAttack.None ⇒
        unit(BattleResult.none)
      case BattleAttack.Abort ⇒
        unit(BattleResult.aborted)
    }
  }

  private def chooseAttacker(attackers: Team, opponents: Team): Rng[Maybe[Person]] =
    chooseAlivePerson(attackers)

  private def chooseAlivePerson(team: Team): Rng[Maybe[Person]] = {
    val alive = alivePersons(team)
    if (alive.isEmpty) Rng.insert(empty)
    else if (alive.tail.isEmpty)
      Rng.insert(just(alive.head))
    else
      Rng.oneofL(NonEmptyList.nel(alive.head, alive.tail)).map(just)
  }

  private def chooseAttack(attacker: Person, attackers: Team, opponents: Team): Interact[BattleAttack] = attacker match {
    case c: Character ⇒
      list.toNel(alivePersons(opponents))
        .fold(unit(BattleAttack.none))(selectPerson(c))

    case m: Monster ⇒
      val alive = opponents.persons.filter(_.hp.x > 0)
      alive.headOption
        .map(a ⇒ m.ai(m, attackers, Team(a, alive.tail)))
        .getOrElse(unit(BattleAttack.None))
  }

  private def selectPerson(a: Attacker)(persons: NonEmptyList[Person]): Interact[BattleAttack] =
    printString(s"$a: Choose your enemy") >>= { _ ⇒
      readEnemy(persons).map { mp ⇒
        mp.cata(t ⇒ BattleAttack(a, t.asTarget), BattleAttack.abort)
      }
    }

  private def update(p: Person, f: Person ⇒ Person, team: Team): Team = {
    val persons = team.persons
    val idx = {
      val i = persons.indexOf(p)
      if (i == -1) None else Some(i)
    }
    val newPersons = idx.fold(persons)(i ⇒ persons.updated(i, f(p)))
    Team(newPersons.head, newPersons.tail)
  }


  private def alivePersons(team: Team): List[Person] =
    team.persons.filter(_.hp.x > 0)

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

  private def formatEnemies(persons: List[Person], current: Int): List[OutPerson] =
    persons
      .zipWithIndex
      .map(px ⇒ OutPerson(px._1.toString, px._2 == current))

  private def printEnemies(persons: List[Person], current: Int): Interact[Input] = for {
    _ ← printPersons(formatEnemies(persons, current))
    i ← readInput
  } yield i
}
