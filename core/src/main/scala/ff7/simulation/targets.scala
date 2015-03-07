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

import battle._
import stats._

import scalaz._
import Scalaz._

import shapeless.contrib.scalaz._

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

  def chosenAttack: MonsterAttack =
    MonsterAttack.physical("Attack", attackPercent, power)

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

final case class Monster(
  name: String,
  level: Level,
  xp: XP,
  // win: Win,
  hp: HP, maxHp: HP,
  // ap: AP,
  // steal: Steal,
  mp: MP, maxMp: MP,
  // gil: Gil,
  // morph: Morph,
  attack: Attack,
  defense: Defense,
  defensePercent: DefensePercent,
  dexterity: Dexterity,
  magicAttack: MagicAttack,
  magicDefense: MagicDefense,
  luck: Luck,
  ai: AI) extends Person with Target {

  def attacks(a: MonsterAttack): MonsterAttacks =
    MonsterAttacks(this, a)

  def attacks(p: Person, a: MonsterAttack): BattleAttack =
    BattleAttack(attacks(a), p.asTarget)

  def asTarget: Target = this
  def asPerson: Person = this
  def hit(h: Hit): Person = h match {
    case Hit.Missed      ⇒ this
    case Hit.Hits(c)     ⇒ copy(hp = HP(hp.x - c))
    case Hit.Critical(c) ⇒ copy(hp = HP(hp.x - c))
  }

  override def toString: String =
    s"$name [HP ${hp.x}/${maxHp.x}]"
}

final case class MonsterAttacks(
  monster: Monster,
  chosenAttack: MonsterAttack
  ) extends Attacker with Target {
  def name: String = monster.name
  def hp: HP = monster.hp
  def mp: MP = monster.mp
  def level: Level = monster.level
  def dexterity: Dexterity = monster.dexterity
  def luck: Luck = monster.luck
  def power: Power = chosenAttack.power
  def attack: Attack = monster.attack
  def defense: Defense = monster.defense
  def defensePercent: DefensePercent = monster.defensePercent
  def attackPercent: AttackPercent = chosenAttack.attackPercent
  def asPerson: Person = monster

  override def toString: String = s"$monster (${chosenAttack.name})"
}
