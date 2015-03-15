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
package battle

import scalaz.NonEmptyList

final case class Team(first: Person, rest: List[Person], originalStart: Option[Int]) {

  def persons: List[Person] = first :: rest

  def isHero = persons.forall(_.isHero)

  def inOrder: (Person, List[Person]) =
    originalStart.fold((first, rest)) { idx ⇒
      val inOrder = split(idx)
      (inOrder.head, inOrder.tail)
    }

  def alivesInOrder: List[Person] =
    originalStart.fold(alives) { idx ⇒
      split(idx).filter(_.hp.x > 0)
    }

  private def split(idx: Int) = {
    val (tail, init) = rest.splitAt(idx)
    init ::: first :: tail ::: Nil
  }

  def alives: List[Person] = persons.filter(_.hp.x > 0)

  def alive: Option[Person] = persons.find(_.hp.x > 0)

  def cycle: Team =
    if (rest.isEmpty) this
    else {
      val newStart = originalStart match {
        case None ⇒ Some(0)
        case Some(x) if x < (rest.length - 1) ⇒ Some(x + 1)
        case _ ⇒ None
      }
      Team(rest.head, rest.tail ::: first :: Nil, newStart)
    }

  def updated(old: Person, updated: Person): Team = {
    if (old == first) copy(first = updated)
    else {
      val idx = rest.indexOf(old)
      if (idx == -1) this
      else copy(rest = rest.updated(idx, updated))
    }
  }

  def toNel = {
    val (first, rest) = inOrder
    NonEmptyList.nel(first, rest)
  }

  override def toString = s"Team(${persons.mkString(", ")})"
}
object Team {
  def apply(person: Person, persons: Person*): Team =
    Team(person, persons.toList, None)
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
