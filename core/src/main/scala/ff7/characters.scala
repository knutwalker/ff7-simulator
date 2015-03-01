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

import scalaz.Maybe._

object characters {
  import weapons._

  val cloud = Character("Cloud",
    Level(6),
    HP(302), HP(302),
    MP(54), MP(56),
    Strength(18),
    Dexterity(6),
    Vitality(16),
    Magic(23),
    Spirit(17),
    Luck(14),
    XP(610),
    just(busterSword),
    empty
  )

  val cloud2 = Character("Cloud",
    Level(7),
    HP(302), HP(316),
    MP(54), MP(57),
    Strength(19),
    Dexterity(9),
    Vitality(17),
    Magic(26),
    Spirit(18),
    Luck(16),
    XP(642),
    just(busterSword),
    empty
  )

  val barret = Character("Barret",
    Level(6),
    HP(328), HP(328),
    MP(43), MP(43),
    Strength(19),
    Dexterity(9),
    Vitality(19),
    Magic(18),
    Spirit(14),
    Luck(16),
    XP(395),
    just(gatlingGun),
    empty
  )

  val cloud0 = Character("Cloud",
    Level(6),
    HP(314), HP(314),
    MP(54), MP(54),
    Strength(20),
    Dexterity(16),
    Vitality(19),
    Magic(17),
    Spirit(6),
    Luck(14),
    XP(610),
    just(busterSword),
    empty
  )
}
