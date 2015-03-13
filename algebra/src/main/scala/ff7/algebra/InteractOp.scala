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
package algebra

import com.nicta.rng.Rng


sealed trait InteractOp[A]
object InteractOp {
  case class  ShowMessage(s: String)                                 extends InteractOp[Unit]
  case class  ShowItems(ps: List[UiItem], id: TeamId)                extends InteractOp[Unit]
  case class  Log(x: String, level: LogLevel, ex: Option[Throwable]) extends InteractOp[Unit]
  case class  Random[A](rng: Rng[A])                                 extends InteractOp[A]
  case object ReadInput                                              extends InteractOp[Input]
}
