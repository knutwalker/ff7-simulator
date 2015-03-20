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

import algebra.{Random, Effect, Interact, Input}, Effect.point
import battle._
import monsters._

import scalaz._
import Scalaz._

object Simulation {

  sealed trait Types[F[_]] {
    type Effected[A] = Effect[F, A]
    type Stated[A, B] = StateT[Effected, A, B]
    val battleMonad: MonadState[Stated, BattleField] =
      MonadState[Stated, BattleField]
  }

  private def battleMonad[F[_]] =
    new Types[F] {}.battleMonad
//    MonadState[({type λ[α, β] = StateT[({type μ[γ] = Effect[F, γ]})#μ, α, β]})#λ, BattleField]

  def apply[F[_]: Interact: Random](field: BattleField): Effect[F, BattleField] =
    playAllRounds[F].eval(field)

  val chooseAttacker: State[BattleField, Option[Person]] = State { (bf: BattleField) ⇒
    (bf, bf.heroes.alive)
  }

  def runAttack[F[_]: Interact: Random]: StateT[({type λ[α] = Effect[F, α]})#λ, BattleField, BattleResult] =
    chooseAttacker.mapK[({type λ[α] = Effect[F, α]})#λ, BattleResult, BattleField] {
      case (bf, Some(a)) ⇒ attack(a, bf.enemies, bf.heroes).map(b ⇒ (bf, b))
      case (bf, None)    ⇒ point(BattleResult.none).map(b ⇒ (bf, b))
    }

  def playRound[F[_]: Interact: Random]: StateT[({type λ[α] = Effect[F, α]})#λ, BattleField, BattleField] = {
    runAttack.mapK[({type λ[α] = Effect[F, α]})#λ, BattleField, BattleField](_
      .flatMap((evaluateResult[F] _).tupled).map(_.squared))
  }

  def setupPerson[F[_]: Random]: Kleisli[({type λ[α] = Effect[F, α]})#λ, Person, Person] =
    Kleisli.kleisli[({type λ[α] = Effect[F, α]})#λ, Person, Person] {
      case m: Monster ⇒ m.ai.setup(m).map(_.asPerson)
      case x          ⇒ point(x)
    }

  def initiateRound[F[_]: Random]: StateT[({type λ[α] = Effect[F, α]})#λ, BattleField, BattleField] =
    StateT[({type λ[α] = Effect[F, α]})#λ, BattleField, BattleField] { b ⇒
      val hsi = b.heroes.toNel.traverse[({type λ[α] = Effect[F, α]})#λ, Person](setupPerson.run)
      val esi = b.enemies.toNel.traverse[({type λ[α] = Effect[F, α]})#λ, Person](setupPerson.run)
      for {
        hs ← hsi
        es ← esi
      } yield {
        val h = hs.toTeam.copy(originalStart = b.heroes.originalStart)
        val e = es.toTeam.copy(originalStart = b.enemies.originalStart)
        b.copy(heroes = h, enemies = e).squared
      }
    }

  def playAllRounds[F[_]: Interact: Random]: StateT[({type λ[α] = Effect[F, α]})#λ, BattleField, BattleField] =
    initiateRound[F].flatMap(_ ⇒ battleMonad[F].iterateUntil(playRound)(_.isFinished))

  def attack[F[_]: Interact: Random](attacker: Person, opponents: Team, allies: Team): Effect[F, BattleResult] = {
    attacker.chooseAttack(opponents, allies).flatMap {
      case \/-(BattleAttack.Attack(x, t)) ⇒ executeAttack(attacker, x, t)
      case \/-(BattleAttack.None)         ⇒ point(BattleResult.none)
      case \/-(BattleAttack.Abort)        ⇒ point(BattleResult.aborted)
      case -\/(Input.Quit)                ⇒ point(BattleResult.aborted)
      case -\/(Input.Undo)                ⇒ point(BattleResult.undo)
    }
  }

  def executeAttack[F[_]: Random](originalAttacker: Person, attacker: Attacker, target: Target): Effect[F, BattleResult] = {
    val formula = attacker.chosenAttack.formulaType match {
      case FormulaType.Physical ⇒ formulas.Physical
      case FormulaType.Magical  ⇒ formulas.Magical
    }
    formula(attacker, target)
      .map(BattleResult(originalAttacker, attacker, target, _))
  }

  def evaluateResult[F[_]: Interact](bf: BattleField, br: BattleResult): Effect[F, BattleField] = br match {
    case r@BattleResult.Attack(_,_,_,_) ⇒ evaluateAttack(bf, r)
    case BattleResult.None              ⇒ evaluateNoAttack(bf)
    case BattleResult.Aborted           ⇒ evaluateAbort(bf)
    case BattleResult.Undo              ⇒ evaluateUndo(bf)
  }

  def evaluateAttack[F[_]: Interact](bf: BattleField, br: BattleResult.Attack): Effect[F, BattleField] = {
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

  def evaluateNoAttack[F[_]: Interact](bf: BattleField): Effect[F, BattleField] = {
    val b = bf.round(BattleResult.none).swap.cycle
    val msg = "No attack happened"
    Effect.showMessage(msg) >| b
  }

  def evaluateAbort[F[_]: Interact](bf: BattleField): Effect[F, BattleField] = {
    val b = bf.round(BattleResult.aborted).copy(aborted = true)
    val msg = "Attack was aborted"
    Effect.showMessage(msg) >| b
  }

  def evaluateUndo[F[_]: Interact](bf: BattleField): Effect[F, BattleField] = bf match {
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
