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

import scalaz._, Scalaz._, Maybe._
import scalaz.effect.IO

import com.nicta.rng.Rng
import spire.math.Rational

import math.{max, min}


object Simulation {

  val damageRandom = Rng
    .chooseint(0, 65535)
    .map(i ⇒ Rational(i * 99, 65535).toInt + 1)

  val variation = Rng
    .chooseint(0, 255)
    .map(_ + 3841)

  def executeAttack(attacker: Attacker, target: Target): Rng[Hit] = {
    val hits = calculateIfHits(attacker, target)
    hits flatMap { h ⇒
      if (h) calculateDamage(attacker, target)
      else Rng.insert(Hit.missed)
    }
  }

  def calculateIfHits(attacker: Attacker, target: Target): Rng[Boolean] = {
    calculateIfHitsPercent(attacker, target)
      .flatMap(h ⇒ damageRandom.map(_ < h))
  }

  def calculateIfHitsPercent(attacker: Attacker, target: Target): Rng[Int] = {
    // Lucky Hit:
    //   The Attacker has a [Lck / 4]% chance of landing a Lucky Hit.  If this is
    //   successful, then Hit% is immediately increased to 255%.

    // Lucky Evade:
    //   The Target has a [Lck / 4]% chance of pulling off a Lucky Evade, but
    //   *ONLY* if a Lucky Hit was not pulled off.  Furthermore, it uses the same
    //   random number that was checked for Lucky Hit, so the Target's Lck must be
    //   *greater* than the Attacker's Lck to even have a chance at this.  The
    //   adjusted chance should therefore be expressed as:
    //
    //       Evade Chance = ([Target's Lck / 4] - [Attacker's Lck / 4])%.

    //   Note that only party members may obtain a Lucky Evade and only if a
    //   non-party member is attacking them - enemies will never get a Lucky Evade.
    //   If a Lucky Evade is pulled off, then the Hit% is immediately decreased
    //   to 0%.

    val hitPercent = ((attacker.dexterity / 4).x + attacker.attackPercent.x) + attacker.defensePercent.x - target.defensePercent.x
    Rng.insert(hitPercent)
  }

  def calculateDamage(attacker: Attacker, target: Target): Rng[Hit] = {
    val damage = calculateBaseDamage(attacker, target)
    val criticalHits = calculateCritical(attacker, target)

    for {
      c ← criticalHits
      d1 ← applyCritical(c, damage)
      // TODO: rows
      d3 ← applyVariance(d1)
      d4 ← applyBounds(d3)
    } yield {
      if (c) Hit.critical(d4)
      else Hit(d4)
    }
  }

  def calculateBaseDamage(attacker: Attacker, target: Target): Int = {
    val base = attacker.attack.x + Rational(attacker.attack.x + attacker.level.x, 32).toInt * Rational(attacker.attack.x * attacker.level.x, 32).toInt
//    val power = attacker.power.x * base
//    val power = attacker.power.x.toDouble
    val power = attacker.power.x * 16
    ((power * (512 - target.defense.x) * base) / (16 * 512)).toInt
  }

  def calculateCritical(attacker: Attacker, target: Target): Rng[Boolean] = {
    val criticalPercent = Rational(attacker.luck.x + attacker.level.x - target.level.x, 4).toInt
    damageRandom.map(_ <= criticalPercent)
  }

  def applyCritical(critical: Boolean, damage: Int): Rng[Int] = {
    Rng.insert(if (critical) damage * 2 else damage)
  }

  def applyVariance(d: Int): Rng[Int] = {
    variation.map(m ⇒ Rational(d * m, 4096).toInt)
  }

  def applyBounds(damage: Int): Rng[Int] = {
    Rng.insert(min(9999, max(1, damage)))
  }

  val setupPerson: Kleisli[IO, Person, Person] = Kleisli.kleisli {
    case m: Monster ⇒
      m.ai.setup(m).map(_.asPerson)
    case x ⇒ IO(x)
  }

  val initiateRound: StateT[IO, BattleField, BattleField] =
    StateT[IO, BattleField, BattleField] { b ⇒
      val hsio = b.heroes.persons.traverse(setupPerson.run)
      val esio = b.enemies.persons.traverse(setupPerson.run)
      hsio.flatMap(hs ⇒ esio
        .map(es ⇒ b.copy(heroes = Team(hs), enemies = Team(es))))
        .map(_.squared)
    }

  val playRoundS =
    StateT[IO, BattleField, BattleField](b ⇒ playRound(b).map(_.squared))

  val playAllRounds: StateT[IO, BattleField, BattleField] =
    initiateRound >> IndexedStateT.stateTMonadState[BattleField, IO]
      .iterateUntil(playRoundS)(f ⇒ f.aPartyIsEnded)

  def playRound(battle: BattleField): IO[BattleField] = {
    runAttack(battle.heroes, battle.enemies) map {
      case m@AttackResult(originalAttacker, attacker, target, hit) ⇒
        // update enemies
        val tp = target.asPerson
        val ntp = tp.hit(hit)
        val enemies = battle.enemies.persons.list
        val idx = enemies.indexOf(tp)
        val newEnemies = enemies.updated(idx, ntp)
        val newEnemyTeam = Team(NonEmptyList.nel(newEnemies.head, newEnemies.tail))
        // update heroes
        val heroes = battle.heroes.persons.list
        val idx2 = heroes.indexOf(originalAttacker)
        val newHeroes = heroes.updated(idx2, attacker.asPerson)
        val newHeroesTeam = Team(NonEmptyList.nel(newHeroes.head, newHeroes.tail))

        BattleField(
          newEnemyTeam,
          newHeroesTeam,
          battle.round + 1,
          m :: battle.history)
      case m@NotAttacked ⇒
        BattleField(
          battle.enemies,
          battle.heroes,
          battle.round + 1,
          m :: battle.history)
    }
  }

  def runAttack(attackers: Team, opponents: Team): IO[BattleResult] = {
    chooseAttacker(attackers, opponents).run.flatMap {
      case Just(a) ⇒
        chooseAttack(a, attackers, opponents).flatMap {
          case BattleAction(x, t) ⇒
            executeAttack(x, t).run.map(AttackResult(a, x, t, _))
          case NoAttack ⇒
            IO(NotAttacked)
        }
      case Empty() ⇒
        IO(NotAttacked)
    }
  }

  def chooseAttacker(attackers: Team, opponents: Team): Rng[Maybe[Person]] = {
    chooseAlivePerson(attackers)
  }

  def chooseAttack(attacker: Person, attackers: Team, opponents: Team): IO[BattleAttack] = attacker match {
    case c: Character ⇒
      chooseAlivePerson(opponents).run
        .map(_.cata(t ⇒ BattleAction(c, t.asTarget), NoAttack))

    case m: Monster ⇒
      val alive = opponents.persons.list.filter(_.hp.x > 0)
      alive.headOption
        .map(a ⇒ Team(NonEmptyList.nel(a, alive.tail)))
        .map(os ⇒ m.ai(m, attackers, os))
        .getOrElse(IO(NoAttack))
  }

  def chooseAlivePerson(team: Team): Rng[Maybe[Person]] = {
    val alive = team.persons.list.filter(_.hp.x > 0)
    if (alive.isEmpty) Rng.insert(empty)
    else if (alive.tail.isEmpty)
      Rng.insert(just(alive.head))
    else
      Rng.oneofL(NonEmptyList.nel(alive.head, alive.tail)).map(just)
  }
}
