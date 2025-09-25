/***********************************************************************

 Copyright (c) 2025 Jean-Marie Jacquet and the CoordiNam Lab members
 (University of Namur, Belgium)

 Permission is hereby granted, free of charge, to any person obtaining
 a  copy of  this  software and  associated  documentation files  (the
 "Software"), to  deal in the Software  without restriction, including
 without limitation the  rights to use, copy,  modify, merge, publish,
 distribute, sublicense,  and/or sell copies  of the Software,  and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:
 
 The  above  copyright notice  and  this  permission notice  shall  be
 included in all copies or substantial portions of the Software.
 
 THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
 EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
 MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
 NONINFRINGEMENT. IN NO  EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
 BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY, WHETHER  IN AN
 ACTION OF  CONTRACT, TORT OR  OTHERWISE, ARISING  FROM, OUT OF  OR IN
 CONNECTION WITH  THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
 SOFTWARE.
 
************************************************************************/
package bscala.bsc_widget

import scalafx.stage
import scalafx.scene.layout.Pane
import scalafx.collections.ObservableBuffer

import scalafx.scene._
import scalafx.scene.paint.Color
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scala.collection.mutable.Queue
import javafx.animation.{Timeline, KeyFrame}
import javafx.util.Duration
import bscala.bsc_data._
import scalafx.scene.image.Image
import scalafx.scene.layout.{Background, BackgroundImage, BackgroundPosition, BackgroundRepeat, BackgroundSize}

class BSC_Scene(w: Double, h: Double, f: String) extends Pane {

  private var widgets: List[BSC_Widget] = List()
  private var widgetNames: Set[String] = Set()

  val mycontent = new Pane()
  var myfill = Color.White

import scalafx.scene.image.Image
import scalafx.scene.layout._
import scalafx.geometry.Insets

   if (f.nonEmpty) {
   val bg = new BackgroundImage(
      new Image(f),
      BackgroundRepeat.NoRepeat,
      BackgroundRepeat.NoRepeat,
      BackgroundPosition.Center,
      new BackgroundSize(1.0, 1.0, true, true, false, true) 
   )
   mycontent.background = new Background(Array(bg))
   }

   private val myscene = new Scene {
   fill = if (f.isEmpty) myfill else Color.Transparent
   content = mycontent
   }

   mycontent.padding = Insets(0)
   mycontent.minWidth  <== myscene.width
   mycontent.prefWidth <== myscene.width
   mycontent.maxWidth  <== myscene.width
   mycontent.minHeight  <== myscene.height
   mycontent.prefHeight <== myscene.height
   mycontent.maxHeight  <== myscene.height



  def addWidgets(ws: Seq[BSC_Widget]): Unit = {
    val newWidgets = ws.filterNot(w => widgetNames.contains(w.name))
    widgets ++= newWidgets
    widgetNames ++= newWidgets.map(_.name)

    mycontent.children ++= newWidgets.map(_.getNode.delegate)

    newWidgets.foreach(w =>
      println(s"[POSITION] ${w.name} → layoutX=${w.getNode.layoutX.value}, layoutY=${w.getNode.layoutY.value}")
    )
  }

  def draw(): Unit = {
    widgets.foreach { w =>
      w.getNode.delegate.setVisible(w.visible)
    }
  }

  def launch(): Unit = {
    new PrimaryStage {
      title = "Timed B2Scala Animation"
      width = w
      height = h
      scene = myscene
    }
  }

  private val animationQueue: Queue[() => Unit] = Queue()
  private var playing: Boolean = false

  def addAnimationStep(step: () => Unit): Unit = {
    animationQueue.enqueue(step)
    if (!playing) playNextStep()
  }

  private def playNextStep(): Unit = {
    if (animationQueue.nonEmpty) {
      playing = true
      val step = animationQueue.dequeue()
      step()

      val delay = new Timeline(new KeyFrame(Duration.millis(4000), _ => {
        playNextStep()
      }))
      delay.play()
    } else {
      playing = false
    }
  }
}
