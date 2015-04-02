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

import battle._
import characters.Character
import interact.Input.Special
import interact.{Interact, Input}
import stats._

import scalaz._, Scalaz._

import algebras._, Algebras._
import shapeless.contrib.scalaz._
import shapeless.contrib.scalacheck._

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.Choose
import org.scalacheck.{Gen, Arbitrary}
import spire.math.Rational

trait Arbitraries {

  implicit val characterMonoid = Monoid[Character]

  implicit val arbitraryInputs: Arbitrary[List[Input]] =
    Arbitrary(Gen.listOf(arbitrary[Input]))

  implicit val arbitraryInput: Arbitrary[Input] =
    Arbitrary(Gen.oneOf(Input.cancel, Input.down, Input.left, Input.ok, Input.quit, Input.right, Input.undo, Input.up))

  implicit val arbitraryRational: Arbitrary[Rational] =
    Arbitrary(for {
      n ← arbitrary[Long]
      d ← arbitrary[Long] suchThat (_ != 0)
    } yield Rational(n, d))

  val genDummy: Gen[Dummy] = for {
    hp ← arbitrary[HP]
    mp ← arbitrary[MP]
    level ← arbitrary[Level]
    luck ← arbitrary[Luck]
    defense ← arbitrary[Defense]
    dexterity ← arbitrary[Dexterity]
    defensePercent ← arbitrary[DefensePercent]
    power ← arbitrary[Power]
    attack ← arbitrary[Attack]
    attackPercent ← arbitrary[AttackPercent]
    magicAttack ← arbitrary[MagicAttack]
    magicAttackPercent ← arbitrary[MagicAttackPercent]
    magicDefense ← arbitrary[MagicDefense]
    magicDefensePercent ← arbitrary[MagicDefensePercent]
  } yield Dummy(hp, mp, level, luck, dexterity, defense, defensePercent, power,
      attack, attackPercent, magicAttack, magicAttackPercent, magicDefense, magicDefensePercent)

  implicit val arbitraryPerson: Arbitrary[Person] =
    Arbitrary(genDummy.map(_.asPerson))

  implicit val arbitraryTarget: Arbitrary[Target] =
    Arbitrary(genDummy.map(_.asTarget))

  implicit val arbitraryAttacker: Arbitrary[Attacker] =
    Arbitrary(genDummy.map(_.asAttacker))

  sealed trait Sized
  implicit def unwrapSized(sz: Int @@ Sized): Int = Tag.unwrap(sz)

  private val sizedGen: Gen[Int @@ Sized] =
    Gen.sized(n ⇒ Choose.chooseInt.choose(0, n * 2).map(Tag(_)))
  implicit val sized = Arbitrary(sizedGen)

}

case class Dummy(
  hp: HP,
  mp: MP,
  level: Level,
  luck: Luck,
  dexterity: Dexterity,
  defense: Defense,
  defensePercent: DefensePercent,
  power: Power,
  attack: Attack,
  attackPercent: AttackPercent,
  magicAttack: MagicAttack,
  magicAttackPercent: MagicAttackPercent,
  magicDefense: MagicDefense,
  magicDefensePercent: MagicDefensePercent) extends Person with Target with Attacker {

  val asTarget: Target = this
  val asPerson: Person = this
  val asAttacker: Attacker = this
  val isHero: Boolean = false
  val name: String = "Dummy"
  val chosenAttack: BattleAttack = BattleAttack.physical("Attack")

  def hit(h: Hit): Person = this
  def chooseAttack[F[_] : Interact : Random](opponents: Team, allies: Team): Effect[F, Special \/ BattleAction] = BattleAction.none.right[Special].effect[F]
}

object Arbitraries extends Arbitraries
