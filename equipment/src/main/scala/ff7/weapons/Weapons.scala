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
package weapons

import stats._

import com.typesafe.config.{ConfigFactory, ConfigObject}
import spire.math.Rational

import collection.JavaConverters._
import collection.immutable
import scala.language.dynamics


object Weapons extends Dynamic {

  private[ff7] val config = ConfigFactory.load()

  private val loaded = {
    val c = config.getObject("ff7.weapons")
    val chars = c.entrySet.asScala

    chars.foldLeft(immutable.Map.empty[String, Weapon]) { (m, char) ⇒
      m + ((char.getKey, weaponFromConf(char.getValue.asInstanceOf[ConfigObject])))
    }
  }

  private def weaponFromConf(v: ConfigObject): Weapon = {
    Weapon(
      v("name")[String],
      Power(v("power")[Rational]),
      Attack(v("attack")[Int]),
      AttackPercent(v("attackpercent")[Int]),
      MagicAttack(v("magicattack")[Int])
    )
  }

  def selectDynamic(name: String): Weapon = loaded(name)
}
