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
package characters

import algebra.{OutPerson, Input, Interact}
import algebra.Interact._
import battle._
import stats._

import scalaz._
import Maybe._
import Scalaz._
import std.list

import shapeless.contrib.scalaz._

import math._

final case class Character(
  name: String,
  level: Level,
  hp: HP, maxHp: HP,
  mp: MP, maxMp: MP,
  strength: Strength,
  dexterity: Dexterity,
  vitality: Vitality,
  magic: Magic,
  spirit: Spirit,
  luck: Luck,
  xp: XP,
  weapon: Maybe[Weapon],
  armour: Maybe[Armour]) extends Person with Target with Attacker {

  def power = weapon.foldMap(_.power)
  def attack = weapon.foldMap(_.attack) + strength.x
  def attackPercent = weapon.foldMap(_.attackPercent)
  def defense = armour.foldMap(_.defense) + vitality.x
  def defensePercent = armour.foldMap(_.defensePercent) + (dexterity / 4).x

  val asTarget: Target = this
  val asPerson: Person = this

  val chosenAttack: MonsterAttack =
    MonsterAttack.physical("Attack", attackPercent, power)

  def chooseAttack(opponents: Team): Interact[BattleAttack] = {
    list.toNel(opponents.alivesInOrder)
      .fold(unit(BattleAttack.none))(Character.selectPerson(this))
  }

  def hit(h: Hit): Person = h match {
    case Hit.Missed      ⇒ this
    case Hit.Hits(c)     ⇒ copy(hp = HP(hp.x - c))
    case Hit.Critical(c) ⇒ copy(hp = HP(hp.x - c))
  }

  override def toString: String =
    s"$name [HP ${hp.x}/${maxHp.x}]"

  //  def magicAttack = MagicAttack(magic.x)
  //  def magicDefense = armour.foldMap(_.magicDefense) + spirit.x
  //  def magicDefensePercent = armour.foldMap(_.magicDefensePercent)

}
object Character {
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
}
