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
import characters.{Weapon, Character}
import monsters.{AI, AiLoader, Monster}
import stats._

import scalaz._, Scalaz._
import Maybe._

import com.typesafe.config.{ConfigList, ConfigObject, ConfigValue}
import spire.math.Rational

import collection.JavaConverters._
import Predef.{augmentString, genericWrapArray}

trait ConfigReader[A] {
  def cast(v: ConfigValue): Val[A]
}
object ConfigReader {
  def apply[A](implicit A: ConfigReader[A]): ConfigReader[A] = A

  implicit def list[A](implicit A: ConfigReader[A]): ConfigReader[List[A]] = new ConfigReader[List[A]] {
    def cast(ov: ConfigValue): Val[List[A]] = ov match {
      case v: ConfigList ⇒
        v.asScala.toList.traverse[Val, A](A.cast)
    case x ⇒
        s"[$x] is not a list".failureNel
    }
  }

  implicit def nonemptylist[A](implicit A: ConfigReader[A]): ConfigReader[NonEmptyList[A]] = new ConfigReader[NonEmptyList[A]] {
    import Validation.FlatMap._
    def cast(ov: ConfigValue): Val[NonEmptyList[A]] =
      list[A].cast(ov).flatMap(_.toNel.toSuccess(s"The list was empty".wrapNel))
  }

  implicit val string: ConfigReader[String] = new ConfigReader[String] {
    def cast(v: ConfigValue): Val[String] =
      get(v).successNel
  }

  implicit val int: ConfigReader[Int] = new ConfigReader[Int] {
    def cast(v: ConfigValue): Val[Int] =
      TryVN(v.unwrapped().asInstanceOf[Int])
        .orElse(TryVN(get(v).toInt))
  }

  implicit val long: ConfigReader[Long] = new ConfigReader[Long] {
    def cast(v: ConfigValue): Val[Long] =
      TryVN(v.unwrapped().asInstanceOf[Long])
        .orElse(TryVN(get(v).toLong))
  }

  implicit val rational: ConfigReader[Rational] = new ConfigReader[Rational] {
    def cast(v: ConfigValue): Val[Rational] = {
      int.cast(v).map(Rational(_))
        .orElse(parseString(get(v)))
    }
    private def parseString(x: String): Val[Rational] = {
      x.split("/") match {
        case Array(num, den) ⇒
          (TryVN(num.trim.toLong) |@| TryVN(den.trim.toLong)) {
            (n, d) ⇒ Rational(n, d)
          }
        case Array(f) ⇒ TryVN(f.trim.toLong).map(Rational(_))
        case _        ⇒ s"could not parse string $x to a Rational".failureNel
      }
    }
  }

  implicit val weapon: ConfigReader[Weapon] = new ConfigReader[Weapon] {
    def cast(ov: ConfigValue): Val[Weapon] = ov match {
      case v: ConfigObject ⇒
        val name = v.nel[String]("name")
        val power = v.nel[Rational]("power")
        val attack = v.nel[Int]("attack")
        val attackPercent = v.nel[Int]("attackpercent")
        val magicAttack = v.nel[Int]("magicattack")
        (name |@| power |@| attack |@| attackPercent |@| magicAttack) {
          (n, p, a, ap, m) ⇒
            Weapon(
              n,
              Power(p),
              Attack(a),
              AttackPercent(ap),
              MagicAttack(m)
            )
        }
      case x ⇒
        s"[$x] is not an object".failureNel
    }
  }

  implicit val character: ConfigReader[Character] = new ConfigReader[Character] {
    def cast(ov: ConfigValue): Val[Character] = ov match {
      case v: ConfigObject ⇒
        val name = v.nel[String]("name")
        val level = v.nel[Int]("level")
        val hp = v.nel[Int]("hp")
        val mp = v.nel[Int]("mp")
        val strength = v.nel[Int]("strength")
        val dexterity = v.nel[Int]("dexterity")
        val vitality = v.nel[Int]("vitality")
        val magic = v.nel[Int]("magic")
        val spirit = v.nel[Int]("spirit")
        val luck = v.nel[Int]("luck")
        val xp = v.nel[Int]("xp")
        val weaponName = v.nel_?[String]("weapon")
        val weapon = {
          import Validation.FlatMap._
          weaponName.flatMap(ow ⇒ ow.traverse[Val, Weapon](w ⇒ Weapons.selectDynamic(w)))
        }


        (name |@| level |@| hp |@| mp |@| strength |@| dexterity |@| vitality |@| magic |@| spirit |@| luck |@| xp |@| weapon) {
        (n, l, hp, mp, s, d, v, m, i, c, xp, w) ⇒
          Character(
            n,
            Level(l),
            HP(hp),
            HP(hp),
            MP(mp),
            MP(mp),
            Strength(s),
            Dexterity(d),
            Vitality(v),
            Magic(m),
            Spirit(i),
            Luck(c),
            XP(xp),
            fromOption(w),
            empty
          )
        }
      case x ⇒
        s"[$x] is not an object".failureNel
    }
  }

  implicit val monster: ConfigReader[Monster] = new ConfigReader[Monster] {
    def cast(ov: ConfigValue): Val[Monster] = ov match {
      case v: ConfigObject ⇒
        val name = v.nel[String]("name")
        val level = v.nel[Int]("level")
        val hp = v.nel[Int]("hp")
        val mp = v.nel[Int]("mp")
        val dexterity = v.nel[Int]("dexterity")
        val luck = v.nel[Int]("luck")
        val attack = v.nel[Int]("attack")
        val defense = v.nel[Int]("defense")
        val defensePercent = v.nel[Int]("defensepercent")
        val magicAttack = v.nel[Int]("magicattack")
        val magicDefense = v.nel[Int]("magicdefense")
        val xp = v.nel[Int]("xp")
        val ai = v.nel[AI]("ai")

        val make = (name |@| level |@| hp |@| mp |@| dexterity |@| luck |@| attack |@| defense |@| defensePercent |@| magicAttack |@| magicDefense |@| xp) {
          (n, l, hp, mp, x, c, a, d, dp, ma, md, xp) ⇒ (ai: AI) ⇒
            Monster(
              n,
              Level(l),
              XP(xp),
              HP(hp),
              HP(hp),
              MP(mp),
              MP(mp),
              Attack(a),
              Defense(d),
              DefensePercent(dp),
              Dexterity(x),
              MagicAttack(ma),
              MagicDefense(md),
              Luck(c),
              ai
            )
        }

        (ai |@| make) { (a, m) ⇒ m(a) }
      case x ⇒
        s"[$x] is not an object".failureNel
    }
  }

  implicit val ai: ConfigReader[AI] = new ConfigReader[AI] {
    def cast(v: ConfigValue): Val[AI] = {
      AiLoader(get(v)).nel
    }
  }

  implicit val encounter: ConfigReader[Encounter] = new ConfigReader[Encounter] {
    def cast(ov: ConfigValue): Val[Encounter] = ov match {
      case v: ConfigObject ⇒
        val id = v.nel[String]("id")
        val encounterValue = v.nel[Int]("encounterValue")
        val battles = v.nel[NonEmptyList[Encounter.Group]]("battles")
        (id |@| encounterValue |@| battles) { (i, ev, bs) ⇒
          Encounter(i, ev, bs)
        }
      case x ⇒
        s"[$x] is not an object".failureNel
    }
  }

  implicit val encounterGroup: ConfigReader[Encounter.Group] = new ConfigReader[Encounter.Group] {
    def cast(ov: ConfigValue): Val[Encounter.Group] = ov match {
      case v: ConfigObject ⇒
        val id = v.nel[String]("id")
        val kind = v.nel[Encounter.Kind]("type")
        val chance = v.nel[Rational]("chance")
        val monsters = v.nel[NonEmptyList[NonEmptyList[Monster @@ Ref]]]("monsters")
        val runDifficulty = v.nel_?[Int]("runDifficulty")

        (id |@| kind |@| chance |@| monsters |@| runDifficulty) {
          (i, k, c, ms, rd) ⇒
            val run = rd.getOrElse(1)
            val mons = for {
              row ← ms
              mon ← row
            } yield Tag.unwrap(mon)

            Encounter.Group(i, c, Team(mons.head, mons.tail, None), k, run)
        }

      case x ⇒
        s"[$x] is not an object".failureNel
    }
  }

  implicit val monsterRef: ConfigReader[Monster @@ Ref] = new ConfigReader[Monster @@ Ref] {
    def cast(ov: ConfigValue): Val[Monster @@ Ref] = {
      val name = get(ov)
      val parts = name.split(" ", 2).toList
      Monsters.selectDynamic(parts.head)
        .map { m ⇒ if (parts.size == 2) m.copy(name = s"${m.name} ${parts(1)}") else m }
        .map(scalaz.Tag(_))
    }
  }

  implicit val encounterKind: ConfigReader[Encounter.Kind] = new ConfigReader[Encounter.Kind] {
    def cast(v: ConfigValue): Val[Encounter.Kind] = get(v).toLowerCase match {
      case "normal" ⇒ Encounter.Normal.successNel
      case "back"   ⇒ Encounter.Back.successNel
      case _        ⇒ "encounter kind must be normal | back".failureNel
    }
  }

  sealed trait Ref

  private def get(v: ConfigValue): String = String.valueOf(v.unwrapped())
}
