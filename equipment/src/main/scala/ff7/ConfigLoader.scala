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

import com.typesafe.config.ConfigFactory

import collection.JavaConverters._
import collection.immutable
import scala.language.dynamics


abstract class ConfigLoader[A: Caster](configPath: String) extends Dynamic {

  private lazy val loaded = {
    val c = ConfigLoader.config.getObject(configPath)
    val chars = c.entrySet.asScala
    chars.foldLeft(immutable.Map.empty[String, A]) { (m, char) ⇒
      char.getValue.apply[A].toOption
        .fold(m)(w ⇒ m + ((char.getKey, w)))
    }
  }

  final def selectDynamic(name: String): Option[A] = loaded.get(name)

  final def available = loaded.keySet
}
object ConfigLoader {
  private val config = ConfigFactory.load()
}
