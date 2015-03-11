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

import algebra.{Input, Interact}, Interact._
import battle._
import monsters._

import scalaz._
import Scalaz._

object Simulation {
  type IOState[s, a] = StateT[Interact, s, a]

  def apply(field: BattleField): Interact[BattleField] =
    playAllRounds.eval(field)

  val chooseAttacker: State[BattleField, Option[Person]] = State { (bf: BattleField) ⇒
    (bf, bf.heroes.alive)
  }

  val runAttack: StateT[Interact, BattleField, BattleResult] =
    chooseAttacker.mapK[Interact, BattleResult, BattleField] {
      case (bf, Some(a)) ⇒ attack(a, bf.enemies, bf.heroes).map(b ⇒ (bf, b))
      case (bf, None)    ⇒ unit(BattleResult.none).map(b ⇒ (bf, b))
    }

  private val playRound: StateT[Interact, BattleField, BattleField] = {
    runAttack.mapK[Interact, BattleField, BattleField](_
      .flatMap((evaluateResult _).tupled).map(_.squared))
  }

  private val setupPerson: Person ⇒ Interact[Person] = {
    case m: Monster ⇒ m.ai.setup(m).map(_.asPerson)
    case x          ⇒ unit(x)
  }

  private val initiateRound =
    StateT[Interact, BattleField, BattleField] { b ⇒
      val hsi = b.heroes.toNel.traverse[Interact, Person](setupPerson)
      val esi = b.enemies.toNel.traverse[Interact, Person](setupPerson)
      for {
        hs ← hsi
        es ← esi
      } yield {
        val h = hs.toTeam.copy(originalStart = b.heroes.originalStart)
        val e = es.toTeam.copy(originalStart = b.enemies.originalStart)
        b.copy(heroes = h, enemies = e).squared
      }
    }

  private val battleMonad = MonadState[IOState, BattleField]

  private val playAllRounds: StateT[Interact, BattleField, BattleField] = {
    initiateRound >> battleMonad.iterateUntil(playRound)(_.isFinished)
  }

  def attack(attacker: Person, opponents: Team, allies: Team): Interact[BattleResult] = {
    attacker.chooseAttack(opponents, allies).flatMap {
      case \/-(BattleAttack.Attack(x, t)) ⇒ executeAttack(attacker, x, t)
      case \/-(BattleAttack.None)         ⇒ unit(BattleResult.none)
      case \/-(BattleAttack.Abort)        ⇒ unit(BattleResult.aborted)
      case -\/(Input.Quit)                ⇒ unit(BattleResult.aborted)
      case -\/(Input.Undo)                ⇒ unit(BattleResult.undo)
    }
  }

  def executeAttack(originalAttacker: Person, attacker: Attacker, target: Target): Interact[BattleResult] = {
    attacker.chosenAttack.formulaType match {
      case FormulaType.Physical ⇒
        random(formulas.Physical(attacker, target))
          .map(BattleResult(originalAttacker, attacker, target, _))
    }
  }

  private def evaluateResult(bf: BattleField, br: BattleResult): Interact[BattleField] = br match {
    case BattleResult.Attack(oa, a, t, h) ⇒ evaluateAttack(bf, br, oa, a, t, h)
    case BattleResult.None                ⇒ evaluateNoAttack(bf, br)
    case BattleResult.Aborted             ⇒ evaluateAbort(bf, br)
    case BattleResult.Undo                ⇒ evaluateUndo(bf, br)
  }

  private def evaluateAttack(bf: BattleField, br: BattleResult, oa: Person, a: Attacker, t: Target, h: Hit): Interact[BattleField] = {
    val target = t.asPerson
    val enemies = bf.enemies.updated(target, target.hit(h))
    val heroes = bf.heroes.updated(oa, a.asPerson)
    val b = bf.round(br).copy(heroes = enemies, enemies = heroes).cycle
    val msg = h match {
      case Hit.Missed ⇒
        s"$oa attacked $target using [${a.chosenAttack.name}] but missed"
      case Hit.Hits(x) ⇒
        s"$oa attacked $target using [${a.chosenAttack.name}] and hit with $x damage"
      case Hit.Critical(x) ⇒
        s"$oa attacked $target using [${a.chosenAttack.name}] and hit critically with $x damage"
    }
    Interact.printString(msg) >| b
  }

  private def evaluateNoAttack(bf: BattleField, br: BattleResult): Interact[BattleField] = {
    val b = bf.round(br).swap.cycle
    val msg = "No attack happened"
    Interact.printString(msg) >| b
  }

  private def evaluateAbort(bf: BattleField, br: BattleResult): Interact[BattleField] = {
    val b = bf.round(br).copy(aborted = true)
    val msg = "Attack was aborted"
    Interact.printString(msg) >| b
  }

  private def evaluateUndo(bf: BattleField, br: BattleResult): Interact[BattleField] = bf match {
    case BattleField(_, _, _, _ :: prev :: _, _, _) ⇒
      val msg = "The last two attacks were undone"
      Interact.printString(msg) >| prev
    case BattleField(_, _, _, prev :: _, _, _) ⇒
      val msg = "The last attack was undone"
      Interact.printString(msg) >| prev
    case _ ⇒
      val msg = "An attack should have been undone, but there was no history yet"
      Interact.printString(msg) >| bf
  }
}
