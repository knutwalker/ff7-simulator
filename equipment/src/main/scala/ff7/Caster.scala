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

import battle.{Team, Encounter}
import characters.Character
import monsters.{AI, AiLoader, Monster, Monsters}
import stats._
import weapons.Weapons

import scalaz.Maybe._
import scalaz.NonEmptyList

import com.typesafe.config.{ConfigList, ConfigObject, ConfigValue}
import spire.math.Rational

import collection.JavaConverters._
import Predef.{augmentString, genericWrapArray}
import collection.mutable.ListBuffer
import util.{Failure, Success, Try}

trait Caster[A] {
  def cast(v: ConfigValue): Try[A] = safeCast(v.unwrapped())
  protected def safeCast(x: AnyRef): Try[A] = Try(x.asInstanceOf[A])
  protected def unsafeCast(x: AnyRef): A = safeCast(x).get
}
object Caster {
  def apply[A](implicit A: Caster[A]): Caster[A] = A

  implicit def list[A: Caster]: Caster[List[A]] = new Caster[List[A]] {
    override def cast(ov: ConfigValue): Try[List[A]] = ov match {
      case v: ConfigList ⇒
        val ac = Caster[A]
        val b = ListBuffer.empty[A]
        v.asScala.toList.foldLeft(Success(b): Try[ListBuffer[A]]) { (ta, v) ⇒
          for (a ← ta; b ← ac.cast(v)) yield a += b
        }.map(_.result())
      case _ ⇒
        Failure(new IllegalArgumentException("config value is no list"))
    }
  }

  implicit def option[A: Caster]: Caster[Option[A]] = new Caster[Option[A]] {
    override def cast(ov: ConfigValue): Try[Option[A]] =
      Option(ov).fold(Success(None): Try[Option[A]])(v ⇒
        Caster[A].cast(v).map(Some(_)))
  }

  implicit val string: Caster[String] = new Caster[String] {
    override protected def safeCast(x: AnyRef): Try[String] =
      Success(String.valueOf(x))
  }
  implicit val int: Caster[Int] = new Caster[Int] {
    override protected def safeCast(x: AnyRef): Try[Int] =
      Try(x.asInstanceOf[Int]).orElse(Try(string.unsafeCast(x).toInt))
  }
  implicit val long: Caster[Long] = new Caster[Long] {
    override protected def safeCast(x: AnyRef): Try[Long] =
      Try(x.asInstanceOf[Long]).orElse(Try(string.unsafeCast(x).toLong))
  }
  implicit val rational: Caster[Rational] = new Caster[Rational] {
    override protected def safeCast(x: AnyRef): Try[Rational] = {
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
        xp ← v("xp")[Int]
        weaponName ← v("weapon").apply[Option[String]]
      } yield {
        val weapon = weaponName.flatMap(Weapons.selectDynamic)
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

  implicit val encounter: Caster[Encounter] = new Caster[Encounter] {
    override def cast(ov: ConfigValue): Try[Encounter] = ov match {
      case v: ConfigObject ⇒ for {
        id ← v("id")[String]
        encounterValue ← v("encounterValue")[Int]
        battles ← v("battles").apply[List[Encounter.Group]]
      } yield {
        Encounter(id, encounterValue, NonEmptyList.nel(battles.head, battles.tail))
      }
      case _ ⇒
        Failure(new IllegalArgumentException("config value is no object"))
    }
  }
  implicit val encounterGroup: Caster[Encounter.Group] = new Caster[Encounter.Group] {
    override def cast(ov: ConfigValue): Try[Encounter.Group] = ov match {
      case v: ConfigObject ⇒
        val maybeGroup: Try[Option[Encounter.Group]] = for {
          id ← v("id")[String]
          kind ← v("type")[Encounter.Kind]
          chance ← v("chance")[Rational]
          monsters ← v("monsters").apply[List[List[String]]]
          runDifficulty ← v("runDifficulty").apply[Option[Int]]
        } yield {
          val ms = for {
            row ← monsters
            name ← row
            parts = name.split(" ", 2).toList
            mon ← Monsters.selectDynamic(parts.head).toList
          } yield {
            if (parts.size == 2)
              mon.copy(name = s"${mon.name} ${parts(1)}")
            else mon
          }
          val run = runDifficulty.getOrElse(1)
          ms.headOption.map { m ⇒
            Encounter.Group(id, chance, Team(m, ms.tail: _*), kind, run)
          }
        }
        maybeGroup.flatMap(_.fold(Failure[Encounter.Group](new IllegalArgumentException("not enough monsters in group")): Try[Encounter.Group])(Success(_)))

      case _ ⇒
        Failure(new IllegalArgumentException("config value is no object"))
    }
  }

  implicit val encounterKind: Caster[Encounter.Kind] = new Caster[Encounter.Kind] {
    override protected def safeCast(x: AnyRef): Try[Encounter.Kind] = {
      string.safeCast(x).flatMap {
        case "normal" ⇒ Success(Encounter.Normal)
        case "back"   ⇒ Success(Encounter.Back)
        case _        ⇒ Failure(new IllegalArgumentException("must be normal | back"))
      }
    }
  }
}
