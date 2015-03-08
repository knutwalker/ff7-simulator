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

import algebra._

import scalaz.effect.IO
import scalaz.~>

import rx.lang.scala.JavaConversions._
import rx.lang.scala.subjects.PublishSubject
import rx.lang.scala.{Observable, Observer, Scheduler}
import rx.schedulers.SwingScheduler

import swing._
import event._
import scala.concurrent.duration._
import java.awt.{Color, Font}

package object gui {

  implicit val GuiInterpreter: InteractOp ~> IO = new (InteractOp ~> IO) {
    def apply[A](fa: InteractOp[A]): IO[A] = fa match {
      case PrintPersons(ps, id) ⇒ printsPersons(ps, id)
      case PrintString(s)       ⇒ printsString(s)
      case Random(rng)          ⇒ rng.run
      case ReadInput            ⇒ readsInput
    }
  }

  def start(): Unit = {
    SwingApp.main(Array())
  }

  def stop(): Unit = {
    SwingApp.shutdown()
  }

  private def printsPersons(ps: List[UiItem], id: TeamId): IO[Unit] = IO {
    Swing.onEDT(SwingApp.setButtonsFor(ps, id))
  }

  private def printsString(s: String): IO[Unit] = IO {
    Swing.onEDT(SwingApp.label.text = s)
  }

  private def readsInput: IO[Input] = IO {
    SwingApp.inputs.take(1).toBlocking.first
  }

  private val swingScheduler: Scheduler = SwingScheduler.getInstance()

  object SwingApp extends SimpleSwingApplication { also: Reactor ⇒

    private val keypresses = PublishSubject[KeyPressed]()

    val inputs: Observable[Input] = keypresses
      .map(_.key)
      .collect {
        case Key.Up    | Key.W | Key.K ⇒ Input.Up
        case Key.Down  | Key.S | Key.J ⇒ Input.Down
        case Key.Left  | Key.A | Key.H ⇒ Input.Left
        case Key.Right | Key.D | Key.L ⇒ Input.Right
        case Key.Enter | Key.Space     ⇒ Input.Ok
        case Key.Q                     ⇒ Input.Quit
        case Key.BackSpace             ⇒ Input.Undo
      }
      .observeOn(swingScheduler)
      .subscribeOn(swingScheduler)

    val konami = List(
      Key.Up, Key.Up, Key.Down, Key.Down,
      Key.Left, Key.Right, Key.Left, Key.Right,
      Key.B, Key.A)

    keypresses
      .map(_.key)
      .sliding(10, 1)
      .flatMap(_.zipWith(konami)(_ == _).forall(Predef.identity))
      .filter(Predef.identity)
      .subscribeOn(swingScheduler)
      .observeOn(swingScheduler)
      .subscribe(_ ⇒ flashWindow())

    def setButtonsFor(ps: List[UiItem], id: TeamId): Unit = {
      val labels = ps map { p ⇒
        new Label {
          text = if (p.active) s"> ${p.text}" else p.text
          font = if (p.active) new Font(font.getName, Font.BOLD, font.getSize) else font
        }
      }
      val panel = panels(id)
      panel.rows = ps.size
      panel.contents.clear()
      panel.contents ++= labels
      panel.revalidate()
      panel.repaint()
    }

    val flashColors = List(
      Color.white, Color.lightGray, Color.gray, Color.darkGray, Color.black,
      Color.red, Color.pink, Color.orange, Color.yellow, Color.green,
      Color.magenta, Color.cyan, Color.blue)

    def flashWindow(): Unit = {
      top.contents = flashPanel
      val startColor = flashPanel.background

      Observable.interval(80 millis, swingScheduler)
        .zip(flashColors)
        .map(_._2)
        .subscribe(new Observer[Color] {
        override def onNext(value: Color): Unit = {
          flashPanel.background = value
          flashPanel.revalidate()
          flashPanel.repaint()
          top.repaint()
        }
        override def onCompleted(): Unit = {
          flashPanel.background = startColor
          top.contents = worldPanel
          top.repaint()
        }
      })
    }

    val label = new Label {
      text = ""
    }

    val alliesPanel = new GridPanel(0, 1)
    val opponentsPanel = new GridPanel(0, 1)
    val panels: TeamId ⇒ GridPanel = {
      case TeamId.Allies    ⇒ alliesPanel
      case TeamId.Opponents ⇒ opponentsPanel
    }

    val flashPanel = new Panel {}
    val worldPanel = new GridPanel(3, 1) {
      focusable = true

      contents += alliesPanel
      contents += label
      contents += opponentsPanel

      listenTo(keys)
      listenTo(mouse.clicks)

      reactions += {
        case e: KeyPressed ⇒ keypresses.onNext(e)
      }
    }

    val top = new MainFrame {
      title = "FF7"
      preferredSize = new Dimension(350, 250)
      contents = worldPanel
      centerOnScreen()
    }

    override def shutdown(): Unit = {
      keypresses.onCompleted()
      top.close()
      top.dispose()
      super.shutdown()
    }
  }
}
