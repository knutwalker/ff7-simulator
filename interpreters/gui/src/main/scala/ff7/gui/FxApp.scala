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
package gui

import algebra.{Input, TeamId, UiItem}

import rx.lang.scala.JavaConversions._
import rx.lang.scala.subjects.{AsyncSubject, PublishSubject}
import rx.lang.scala.{Observable, Observer, Scheduler}
import rx.schedulers.Schedulers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Separator
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{GridPane, Pane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.Text
import scalafx.stage.Stage
import java.util.concurrent.Executor
import javafx.application.Application


private[gui] object FxApp {

  private val keypresses = PublishSubject[KeyEvent]()
  private val uiItems = PublishSubject[(List[UiItem], TeamId)]()
  private val labelTexts = PublishSubject[String]()

  private val stopSubject = AsyncSubject[Unit]()

  private val fxExecutor = new Executor {
    def execute(command: Runnable): Unit = {
      javafx.application.Platform.runLater(command)
    }
  }
  private val sfxScheduler: Scheduler = Schedulers.from(fxExecutor)

  val labels: Observer[String] = labelTexts
  val items: Observer[(List[UiItem], TeamId)] = uiItems

  val inputs: Observable[Input] = keypresses
    .map(_.code)
    .collect {
      case KeyCode.UP    | KeyCode.W | KeyCode.K ⇒ Input.up
      case KeyCode.DOWN  | KeyCode.S | KeyCode.J ⇒ Input.down
      case KeyCode.LEFT  | KeyCode.A | KeyCode.H ⇒ Input.left
      case KeyCode.RIGHT | KeyCode.D | KeyCode.L ⇒ Input.right
      case KeyCode.ENTER | KeyCode.SPACE         ⇒ Input.ok
      case KeyCode.ESCAPE                        ⇒ Input.cancel
      case KeyCode.Q                             ⇒ Input.quit
      case KeyCode.BACK_SPACE                    ⇒ Input.undo
  }

  def launch(): Unit = {
    Future(Application.launch(Predef.classOf[App]))
  }

  def stop(): Unit = {
    stopSubject.onNext(())
    stopSubject.onCompleted()
  }

  private object LabelUpdater {
    val label1 = new Text {
      text = ""
      fill = Color.Gray
    }
    val label2 = new Text {
      text = ""
      fill = Color.DarkGray
    }
    val label3 = new Text {
      text = ""
      style = "-fx-font-weight:bold"
    }

    def update(texts: List[String]): Unit = {
      val labels = List(label1, label2, label3)
      (texts zip labels) foreach {
        case (s, l) ⇒ l.text = s
      }
    }
  }

  private object PaneUpdater {
    val alliesGrid = new GridPane {
      GridPane.setConstraints(LabelUpdater.label1, 0, 0)
      GridPane.setConstraints(LabelUpdater.label2, 0, 1)
      GridPane.setConstraints(LabelUpdater.label3, 0, 2)

      margin = Insets(14)
    }

    val opponentsGrid = new GridPane {
      GridPane.setConstraints(LabelUpdater.label1, 0, 0)
      GridPane.setConstraints(LabelUpdater.label2, 0, 1)
      GridPane.setConstraints(LabelUpdater.label3, 0, 2)

      margin = Insets(14)
    }
    val teamGrids: TeamId ⇒ GridPane = {
      case TeamId.Allies    ⇒ alliesGrid
      case TeamId.Opponents ⇒ opponentsGrid
    }

    def update(ps: List[UiItem], id: TeamId): Unit = {
      val labels = ps.zipWithIndex map { case (p, row) ⇒
        val l = new Text {
          text = if (p.active) s"> ${p.text}" else p.text
          if (p.active) {
            style = "-fx-font-weight:bold"
          }
        }
        GridPane.setConstraints(l, 0, row)
        l
      }
      val pane = teamGrids(id)
      pane.children = labels
    }
  }

  private lazy val labelsGrid = new GridPane {
    GridPane.setConstraints(LabelUpdater.label1, 0, 0)
    GridPane.setConstraints(LabelUpdater.label2, 0, 1)
    GridPane.setConstraints(LabelUpdater.label3, 0, 2)

    margin = Insets(14)
    children ++= List(LabelUpdater.label1, LabelUpdater.label2, LabelUpdater.label3)
  }

  private lazy val box = new VBox {
    spacing = 10
    padding = Insets(20)
    children = List(
      new Pane { children = List(PaneUpdater.alliesGrid) },
      new Separator,
      new Pane { children = List(labelsGrid) },
      new Separator,
      new Pane { children = List(PaneUpdater.opponentsGrid) }
    )
  }

  private def stage(delegate: javafx.stage.Stage) = new Stage(delegate) {
//    initStyle(StageStyle.UNDECORATED)
    title = "FF7 Simulator"
    width = 640
    height = 480
    scene = new Scene {
      // cursor = None
      content = box
      onKeyPressed = { e: KeyEvent ⇒ keypresses.onNext(e) }
    }
    centerOnScreen()
  }

  private var _stage: Option[Stage] = None

  private[gui] def start(delegate: javafx.stage.Stage): Unit = {
    _stage = Some(stage(delegate))

    (Observable.just("", "", "") ++ labelTexts)
      .observeOn(sfxScheduler)
      .subscribeOn(sfxScheduler)
      .sliding(3, 1)
      .flatMap(_.toList)
      .subscribe(LabelUpdater.update(_))

    uiItems
      .observeOn(sfxScheduler)
      .subscribeOn(sfxScheduler)
      .subscribe((PaneUpdater.update _).tupled)

    stopSubject
      .observeOn(sfxScheduler)
      .subscribeOn(sfxScheduler)
      .subscribe(_ ⇒ shutdown())

    delegate.show()
  }

  private def shutdown(): Unit = {
    keypresses.onCompleted()
    _stage.foreach(_.close())
  }

  private class App extends Application {
    def start(stage: javafx.stage.Stage): Unit = FxApp.start(stage)
    override def stop(): Unit = FxApp.stop()
  }
}
