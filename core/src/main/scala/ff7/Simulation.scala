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

import algebra.Interact, Interact._
import battle._
import monsters._

import scalaz._
import Scalaz._

object Simulation {
  type IOState[s, a] = StateT[Interact, s, a]

  def apply(field: BattleField): Interact[BattleField] =
    playAllRoundsSSS.eval(field)

  private val chooseAttackerSSS: State[BattleField, Option[Person]] = State { (bf: BattleField) ⇒
    (bf.copy(heroes = bf.heroes.cycle), bf.heroes.alive)
  }

  private val runAttackSSS: StateT[Interact, BattleField, BattleResult] =
    chooseAttackerSSS.mapK[Interact, BattleResult, BattleField] {
      case (bf, Some(a)) ⇒ attack(a, bf.enemies, bf.heroes).map(b ⇒ (bf, b))
      case (bf, None)    ⇒ unit(BattleResult.none).map(b ⇒ (bf, b))
    }

  private val playRoundSSS: StateT[Interact, BattleField, BattleField] = {
    runAttackSSS.mapK[Interact, BattleField, BattleField] { _.flatMap {
      case (bf, m@BattleResult.Attack(oa, a, t, h)) ⇒
        val enemies = bf.enemies.updated(t.asPerson, t.asPerson.hit(h))
        val heroes = bf.heroes.updated(oa, a.asPerson)
        val b = bf.round(m).copy(heroes = enemies, enemies = heroes)
        val msg = h match {
          case Hit.Missed ⇒
            s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] but missed"
          case Hit.Hits(x) ⇒
            s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] and hit with $x damage"
          case Hit.Critical(x) ⇒
            s"$oa attacked ${t.asPerson} using [${a.chosenAttack.name}] and hit critically with $x damage"
        }
        Interact.printString(msg) map (_ ⇒ (b, b))
      case (bf, m@BattleResult.None)                                            ⇒
        val b = bf.round(m)
        val msg = "No attack happened"
        Interact.printString(msg) map (_ ⇒ (b, b))
      case (bf, m@BattleResult.Aborted)                                         ⇒
        val b = bf.round(m).copy(aborted = true)
        val msg = "Attack was aborted"
        Interact.printString(msg) map (_ ⇒ (b, b))
    }}
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

  private def attack(attacker: Person, opponents: Team, allies: Team): Interact[BattleResult] = {
    attacker.chooseAttack(opponents, allies).flatMap {
      case BattleAttack.Attack(x, t) ⇒ executeAttack(attacker, x, t)
      case BattleAttack.None         ⇒ unit(BattleResult.none)
      case BattleAttack.Abort        ⇒ unit(BattleResult.aborted)
    }
  }

  private def executeAttack(originalAttacker: Person, attacker: Attacker, target: Target): Interact[BattleResult] = {
    attacker.chosenAttack.formulaType match {
      case FormulaType.Physical ⇒
        random(formulas.Physical(attacker, target))
          .map(BattleResult(originalAttacker, attacker, target, _))
    }
  }
}
