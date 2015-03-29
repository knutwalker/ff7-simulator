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
package simulation

import algebra.{Effect, Input, Interact, Random}
import battle._

import scalaz._
import Scalaz._


final class Turn[F[_]: Interact: Random] {

  type X[A] = Effect[F, A]
  type S = StateT[X, BattleField, BattleField]

  val state: StateT[X, BattleField, BattleField] =
    StateT[X, BattleField, BattleField](bf ⇒ applyAttack(bf).map(_.squared))

  def applyAttack(bf: BattleField): Effect[F, BattleField] = for {
    result ← runsAttack(bf, chooseAttacker(bf))
    field ← evaluateResult(bf, result)
  } yield field

  def evaluateResult(bf: BattleField, br: BattleResult): Effect[F, BattleField] = br match {
    case r@BattleResult.Attack(_,_,_,_) ⇒ evaluateAttack(bf, r)
    case r@BattleResult.Change(_,_)     ⇒ evaluateChange(bf, r)
    case BattleResult.None              ⇒ evaluateNoAttack(bf)
    case BattleResult.Aborted           ⇒ evaluateAbort(bf)
    case BattleResult.Undo              ⇒ evaluateUndo(bf)
  }

  def runsAttack(bf: BattleField, attacker: Option[Person]): Effect[F, BattleResult] =
    attacker.fold(BattleResult.none.effect[F])(a ⇒ attack(a, bf.enemies, bf.heroes))

  def attack(attacker: Person, opponents: Team, allies: Team): Effect[F, BattleResult] = {
    attacker.chooseAttack(opponents, allies).flatMap {
      case \/-(BattleAttack.Attack(x, t)) ⇒ executeAttack(attacker, x, t)
      case \/-(BattleAttack.None)         ⇒ BattleResult.none.effect[F]
      case \/-(BattleAttack.Abort)        ⇒ BattleResult.aborted.effect[F]
      case -\/(Input.Quit)                ⇒ BattleResult.aborted.effect[F]
      case -\/(Input.Undo)                ⇒ BattleResult.undo.effect[F]
    }
  }

  def executeAttack(originalAttacker: Person, attacker: Attacker, target: Target): Effect[F, BattleResult] = {
    val formula = attacker.chosenAttack.formulaType match {
      case FormulaType.Physical ⇒ formulas.Physical
      case FormulaType.Magical  ⇒ formulas.Magical
    }
    formula(attacker, target)
      .map(BattleResult(originalAttacker, attacker, target, _))
  }

  def chooseAttacker(bf: BattleField): Option[Person] = bf.heroes.alive

  def evaluateAttack(bf: BattleField, br: BattleResult.Attack): Effect[F, BattleField] = {
    val oa = br.originalAttacker
    val a = br.attacker
    val t = br.target
    val h = br.hit
    val target = t.asPerson
    val enemies = bf.enemies.updated(target, target.hit(h))
    val heroes = bf.heroes.updated(oa, a.asPerson)
    val b = bf.round(br).copy(heroes = enemies, enemies = heroes).cycle
    val msg = h match {
      case Hit.Missed ⇒
        s"$oa attacked $target using [${a.chosenAttack}] but missed"
      case Hit.Hits(x) ⇒
        s"$oa attacked $target using [${a.chosenAttack}] and hit with $x damage"
      case Hit.Critical(x) ⇒
        s"$oa attacked $target using [${a.chosenAttack}] and hit critically with $x damage"
    }
    Effect.showMessage(msg) >| b
  }

  def evaluateChange(bf: BattleField, br: BattleResult.Change): Effect[F, BattleField] = {
    val oa = br.originalAttacker
    val a = br.attacker
    val heroes = bf.heroes.updated(oa, a)
    val b = bf.round(br).copy(heroes = bf.enemies, enemies = heroes).cycle
    b.effect
  }

  def evaluateNoAttack(bf: BattleField): Effect[F, BattleField] = {
    val b = bf.round(BattleResult.none).swap.cycle
    val msg = "No attack happened"
    Effect.showMessage(msg) >| b
  }

  def evaluateAbort(bf: BattleField): Effect[F, BattleField] = {
    val b = bf.round(BattleResult.aborted).copy(aborted = true)
    val msg = "Attack was aborted"
    Effect.showMessage(msg) >| b
  }

  def evaluateUndo(bf: BattleField): Effect[F, BattleField] = bf match {
    case BattleField(_, _, _, _ :: prev :: _, _, _) ⇒
      val msg = "The last two attacks were undone"
      Effect.showMessage(msg) >| prev
    case BattleField(_, _, _, prev :: _, _, _) ⇒
      val msg = "The last attack was undone"
      Effect.showMessage(msg) >| prev
    case _ ⇒
      val msg = "An attack should have been undone, but there was no history yet"
      Effect.showMessage(msg) >| bf
  }
}

object Turn {
  def apply[F[_]: Interact: Random]: Turn[F] = new Turn[F]
}
