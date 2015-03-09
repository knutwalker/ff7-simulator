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

import battle.{BattleAttack, Team}
import characters.Character
import monsters.{AI, AiLoader, Monster, Monsters}
import stats._
import weapons.Weapons

import scalaz.Maybe._

import com.typesafe.config.{ConfigObject, ConfigValue}
import spire.math.Rational

import Predef.augmentString
import util.{Failure, Success, Try}

trait Caster[A] {
  def cast(v: ConfigValue): Try[A] = safeCast(v.unwrapped())
  protected def safeCast(x: AnyRef): Try[A] = Try(x.asInstanceOf[A])
  protected def unsafeCast(x: AnyRef): A = safeCast(x).get
}
object Caster extends LowerPriorityImplicits {
  implicit val string: Caster[String] = new Caster[String] {
    def safeCast(x: AnyRef): Try[String] = Success(String.valueOf(x))
  }
  implicit val int: Caster[Int] = new Caster[Int] {
    def safeCast(x: AnyRef): Try[Int] =
    Try(x.asInstanceOf[Int]).orElse(Try(string.unsafeCast(x).toInt))
  }
  implicit val long: Caster[Long] = new Caster[Long] {
    def safeCast(x: AnyRef): Try[Long] =
    Try(x.asInstanceOf[Long]).orElse(Try(string.unsafeCast(x).toLong))
  }
  implicit val rational: Caster[Rational] = new Caster[Rational] {
    def safeCast(x: AnyRef): Try[Rational] = {
      int.safeCast(x).map(Rational(_)).orElse(parseString(string.unsafeCast(x)))
    }
    private def parseString(x: String): Try[Rational] = {
      x.split("/") match {
        case Array(num, den) ⇒ for {
          n ← Try(num.trim.toLong)
          d ← Try(den.trim.toLong)
        } yield Rational(n, d)
        case Array(f) ⇒ Try(f.trim.toLong).map(Rational(_))
        case _        ⇒ Failure(new IllegalArgumentException(s"could not parse string $x to a Rational"))
      }
    }
  }
  implicit val weapon: Caster[Weapon] = new Caster[Weapon] {
    def safeCast(x: AnyRef): Try[Weapon] = Failure(new IllegalArgumentException("need to use cast"))
    override def cast(ov: ConfigValue): Try[Weapon] = ov match {
      case v: ConfigObject ⇒ for {
        name ← v("name")[String]
        power ← v("power")[Rational]
        attack ← v("attack")[Int]
        attackPercent ← v("attackpercent")[Int]
        magicAttack ← v("magicattack")[Int]
      } yield {
        Weapon(
          name,
          Power(power),
          Attack(attack),
          AttackPercent(attackPercent),
          MagicAttack(magicAttack)
        )
      }
      case _ ⇒
        Failure(new IllegalArgumentException("config value is no object"))
    }
  }

  implicit val character: Caster[Character] = new Caster[Character] {
    def safeCast(x: AnyRef): Try[Character] = Failure(new IllegalArgumentException("need to use cast"))
    override def cast(ov: ConfigValue): Try[Character] = ov match {
      case v: ConfigObject ⇒ for {
        name ← v("name")[String]
        level ← v("level")[Int]
        hp ← v("hp")[Int]
        mp ← v("mp")[Int]
        strength ← v("strength")[Int]
        dexterity <- v("dexterity")[Int]
        vitality ← v("vitality")[Int]
        magic <- v("magic")[Int]
        spirit ← v("spirit")[Int]
        luck <- v("luck")[Int]
        xp ← v("xp").apply[Int]
      } yield {
        val weapon = for {
          w1 ← Option(v("weapon"))
          w2 ← w1[String].toOption
          w3 ← Weapons.selectDynamic(w2)
        } yield w3
        Character(
          name,
          Level(level),
          HP(hp),
          HP(hp),
          MP(mp),
          MP(mp),
          Strength(strength),
          Dexterity(dexterity),
          Vitality(vitality),
          Magic(magic),
          Spirit(spirit),
          Luck(luck),
          XP(xp),
          fromOption(weapon),
          empty
        )
      }
      case _ ⇒
        Failure(new IllegalArgumentException("config value is no object"))
    }
  }

  implicit val monster: Caster[Monster] = new Caster[Monster] {
    def safeCast(x: AnyRef): Try[Monster] = Failure(new IllegalArgumentException("need to use cast"))
    override def cast(ov: ConfigValue): Try[Monster] = ov match {
      case v: ConfigObject ⇒ for {
        name ← v("name")[String]
        level ← v("level")[Int]
        hp ← v("hp")[Int]
        mp ← v("mp")[Int]
        dexterity <- v("dexterity")[Int]
        luck <- v("luck")[Int]
        attack ← v("attack")[Int]
        defense ← v("defense")[Int]
        defensePercent ← v("defensepercent")[Int]
        magicAttack ← v("magicattack")[Int]
        magicDefense ← v("magicdefense")[Int]
        xp ← v("xp")[Int]
        ai ← v("ai")[AI]
      } yield {
        Monster(
          name,
          Level(level),
          XP(xp),
          HP(hp),
          HP(hp),
          MP(mp),
          MP(mp),
          Attack(attack),
          Defense(defense),
          DefensePercent(defensePercent),
          Dexterity(dexterity),
          MagicAttack(magicAttack),
          MagicDefense(magicDefense),
          Luck(luck),
          ai
        )
      }
      case _ ⇒
        Failure(new IllegalArgumentException("config value is no object"))
    }
  }

  implicit val ai: Caster[AI] = new Caster[AI] {
    override protected def safeCast(x: AnyRef): Try[AI] = {
      AiLoader(string.unsafeCast(x)).fold(
        x ⇒ Failure(new IllegalArgumentException(x)), Success(_))
    }
  }
}
trait LowerPriorityImplicits {
  implicit def anyRef[A]: Caster[A] = new Caster[A] {
    override protected def safeCast(x: AnyRef): Try[A] = Try(x.asInstanceOf[A])
  }
}
