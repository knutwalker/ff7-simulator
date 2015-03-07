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
package simulation

import battle.Person

import scalaz._

final case class Team(persons: NonEmptyList[Person])
object Team {
  def apply(person: Person, persons: Person*): Team =
    Team(NonEmptyList(person, persons: _*))
}

//sealed trait RowPosition
//case object FrontRow extends RowPosition
//case object BackRow extends RowPosition
//case object NoRow extends RowPosition
//
//final case class Row(persons: NonEmptyList[Person])
//final case class Team(rows: NonEmptyList[Row]) {
//  def persons: NonEmptyList[Person] = rows.flatMap(_.persons)
//  def rowOf(x: Person): Option[Row] = {
//    rows.list.find(_.persons.element(x))
//  }
//  def rowPosition(x: Person): RowPosition = {
//    if(rows.head.persons.element(x)) FrontRow
//    else if (rows.tail.exists(r ⇒ r.persons.element(x))) BackRow
//    else NoRow
//  }
//}
//object Team {
//  def apply(person: Person, persons: Person*): Team =
//    Team(Row(NonEmptyList(person, persons: _*)))
//  def apply(row: Row): Team =
//    Team(row.wrapNel)
//}
