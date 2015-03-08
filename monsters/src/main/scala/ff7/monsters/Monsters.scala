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
package monsters

import algebra.Interact
import battle.{BattleAttack, Team}
import stats._

import com.typesafe.config.{ConfigObject, ConfigFactory}

import collection.JavaConverters._
import collection.immutable
import scala.language.dynamics


object Monsters extends Dynamic {
  private val config = ConfigFactory.load()

  private val loaded = {
    val c = config.getObject("ff7.monsters.midgar1")
    val chars = c.entrySet.asScala

    chars.foldLeft(immutable.Map.empty[String, Monster]) { (m, char) ⇒
      m + ((char.getKey, monsterFromConf(char.getValue.asInstanceOf[ConfigObject])))
    }
  }

  private def monsterFromConf(v: ConfigObject) = {
    Monster(
      v("name")[String],
      Level(v("level")[Int]),
      XP(v("xp")[Int]),
      HP(v("hp")[Int]),
      HP(v("hp")[Int]),
      MP(v("mp")[Int]),
      MP(v("mp")[Int]),
      Attack(v("attack")[Int]),
      Defense(v("defense")[Int]),
      DefensePercent(v("defensepercent")[Int]),
      Dexterity(v("dexterity")[Int]),
      MagicAttack(v("magicattack")[Int]),
      MagicDefense(v("magicdefense")[Int]),
      Luck(v("luck")[Int]),
      DummyAi
    )
  }

  private object DummyAi extends AI {
    def setup(self: Monster): Interact[Monster] =
      Interact.unit(self)

    def apply(self: Monster, targets: Team): Interact[BattleAttack] =
      Interact.unit(BattleAttack.none)
  }

  def selectDynamic(name: String): Monster = loaded(name)
}
