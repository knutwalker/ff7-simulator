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

import algebra.LogLevel.{Debug, Error, Info, Warn}
import algebra.LogOp.Log
import algebra._

import scalaz._
import effect.IO

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

package object log {
  private val logger = Logger(LoggerFactory.getLogger("ff7"))

  def info(msg: String): IO[Unit] =
    IO(logger.info(msg))

  val Interpreter: LogOp ~> IO = new (LogOp ~> IO) {
    def apply[A](fa: LogOp[A]): IO[A] = fa match {
      case Log(x, Debug, Some(ex))     ⇒ IO(logger.debug(x, ex))
      case Log(x, Debug, None)         ⇒ IO(logger.debug(x))
      case Log(x, Info, Some(ex))      ⇒ IO(logger.info(x, ex))
      case Log(x, Info, None)          ⇒ IO(logger.info(x))
      case Log(x, Warn, Some(ex))      ⇒ IO(logger.warn(x, ex))
      case Log(x, Warn, None)          ⇒ IO(logger.warn(x))
      case Log(x, Error, Some(ex))     ⇒ IO(logger.error(x, ex))
      case Log(x, Error, None)         ⇒ IO(logger.error(x))
    }
  }
}
