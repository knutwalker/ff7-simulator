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

import scalaz._
import Scalaz._

import com.typesafe.config.ConfigFactory

import collection.JavaConverters._
import collection.immutable.Set
import scala.language.dynamics


abstract class ConfigLoader[A](configPath: String, entityName: String)(implicit A: ConfigReader[A]) extends Dynamic {
  import Validation.FlatMap._

  private val loaded = {
    TryVN(ConfigLoader.config.getObject(configPath))
      .map(c ⇒ c.asScala.map(_.map(A.read)).toMap)
  }

  final def selectDynamic(name: String): Val[A] =
    loaded.flatMap(_.getOrElse(name,
      s"[$name] was not a configured $entityName".failureNel))

  final def available: Set[String] =
    loaded.map(_.keySet).getOrElse(Set.empty)
}
object ConfigLoader {
  private val config = ConfigFactory.load()
}
