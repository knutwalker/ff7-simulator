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

import battle.Team
import monsters.Monster

import algebras._, Algebras._

import scalaz._
import Scalaz._
import effect.IO

import scala.collection.immutable.Map

object Args {

  def parse[F[_] : Random](args: List[String]): IO[Config[F]] =
    Options.parse(args)

  case class Config[F[_]](ui: UI, enemies: Effect[F, Val[Team]], repetitions: Int)

  case class Options(ui: Option[String], repetitions: Int, monsters: List[(String, Int)])
  object Options {
    val empty = Options(None, 1, Nil)
    val uis = List("console", "gui", "fx")

    val parser = new scopt.OptionParser[Options]("ff7") {
      var _writer = Writer[Vector[String], Unit](Vector.empty, ())

      opt[Int]('r', "repetitions") action { (r, o) ⇒
        o.copy(repetitions = r)
      } validate { r ⇒
        if (r > 0) success
        else failure("repetitions must be > 0")
      }

      opt[String]('u', "ui") action { (u, o) ⇒
        o.copy(ui = Some(u.toLowerCase))
      } validate { x ⇒
        if (Options.uis.contains(x.toLowerCase)) success
        else failure(s"UI must be one of [${Options.uis.mkString(" | ")}]")
      }

      opt[Map[String, Int]]('m', "monster") unbounded() action { (m, o) ⇒
        o.copy(monsters = o.monsters ::: m.toList)
      } validate { case ms ⇒
        ms.toList.traverse_[({type λ[α] = Either[String, α]})#λ] { case (m, c) ⇒
          if (c > 0 && c <= 5) {
            val monster = Monsters.selectDynamic(m)
            monster.fold(es ⇒ failure(es.list.mkString(", ")), m ⇒ success)
          }
          else failure("count must be > 0 and <= 5")
        }
      }
    }

    def defaultEnemies[F[_]: Random]: Effect[F, Val[Team]] =
      Encounters.midgar1
        .traverse[({type λ[α] = Effect[F, α]})#λ, NonEmptyList[String], Team] { es ⇒
          for {
            e ← Random.oneOfL(es)
            g ← Random.oneOfL(e.groups)
          } yield g.monsters
        }

    def parse[F[_]: Random](args: List[String]): IO[Config[F]] = IO {
      parser.parse(args, empty).fold(sys.exit(-1)) { o ⇒
        val team: Effect[F, Val[Team]] = o.monsters.toNel match {
          case Some(monsterConfigs) ⇒
            val monsterss = monsterConfigs.traverse1[Val, NonEmptyList[Monster]] {
              case (monsterName, count) ⇒
                Monsters.selectDynamic(monsterName).map { monster ⇒
                  if (count > 1) {
                    Iterator.from(0)
                      .map(o => 'A' + o)
                      .map(_.toChar)
                      .take(count)
                      .map(suffix ⇒ monster.copy(name = s"${monster.name} $suffix"))
                      .toList.toNel.get
                  } else {
                    NonEmptyList(monster)
                  }
                }
            }
            val monsters = monsterss.map(_.flatMap(x ⇒ x))
            val teams = monsters.map(ms ⇒ Team(ms.head, ms.tail, None))
            teams.effect[F]
          case None ⇒ defaultEnemies[F]
        }
        val ui = o.ui collect {
          case "gui" ⇒ GUI
          case "fx"  ⇒ Fx
        } getOrElse Console
        Config[F](ui, team, o.repetitions)
      }
    }
  }
}
