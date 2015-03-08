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

import stats._
import weapons.Weapons

import scalaz.Maybe._

import com.typesafe.config.ConfigObject

import collection.JavaConverters._
import collection.immutable
import scala.language.dynamics


object Characters extends Dynamic {

  private val config = Weapons.config

  private val loaded = {
    val c = config.getObject("ff7.characters")
    val chars = c.entrySet.asScala

    chars.foldLeft(immutable.Map.empty[String, Character]) { (m, char) ⇒
      m + ((char.getKey, charFromConf(char.getValue.asInstanceOf[ConfigObject])))
    }
  }

  private def charFromConf(v: ConfigObject) = {
    Character(
      v("name")[String],
      Level(v("level")[Int]),
      HP(v("hp")[Int]),
      HP(v("hp")[Int]),
      MP(v("mp")[Int]),
      MP(v("mp")[Int]),
      Strength(v("strength")[Int]),
      Dexterity(v("dexterity")[Int]),
      Vitality(v("vitality")[Int]),
      Magic(v("magic")[Int]),
      Spirit(v("spirit")[Int]),
      Luck(v("luck")[Int]),
      XP(v("xp")[Int]),
      fromNullable(v("weapon")).map(s ⇒ Weapons.selectDynamic(s[String])),
      empty
    )
  }

  def selectDynamic(name: String): Character = loaded(name)
}
