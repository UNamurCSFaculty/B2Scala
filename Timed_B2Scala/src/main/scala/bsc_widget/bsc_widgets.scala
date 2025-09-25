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

import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.text.Text
import scalafx.scene.paint.Color
import scalafx.scene.Node
import scalafx.scene.shape.Rectangle
import scalafx.animation.TranslateTransition
import scalafx.util.Duration
import bscala.bsc_data._




abstract class BSC_Widget(val name: String) {
  var visible: Boolean = true
  var node: Node = _

  def place_at(x: Double, y: Double): Unit
  def move_to(x: Double, y: Double): Unit
  def hide(): Unit = {
    visible = false
    if (node != null) node.visible = false
  }
  def show(): Unit = {
    visible = true
    if (node != null) node.visible = true
  }
  def getNode: Node = node
}



case class img_Widget(override val name: String, x: Double, y: Double, filePath: String) extends BSC_Widget(name) {
  val imageView = new ImageView(new Image(filePath)) {
    fitWidth = 100
    preserveRatio = true
  }
  imageView.layoutX = x
  imageView.layoutY = y

  node = imageView

  override def place_at(nx: Double, ny: Double): Unit = {
    node.asInstanceOf[ImageView].layoutX = nx
    node.asInstanceOf[ImageView].layoutY = ny
  }

  override def move_to(nx: Double, ny: Double): Unit = {
    println(s"[MOVE] Widget '$name' moved to ($nx, $ny)")
    place_at(nx, ny)
  }
}


case class txt_Widget(override val name: String, x: Double, y: Double, t: String) extends BSC_Widget(name) {
  val textNode = new Text {
    text = t
    fill = Color.Black
    style = "-fx-font: 16 arial;"
  }
  textNode.layoutX = x
  textNode.layoutY = y

  node = textNode

  override def place_at(nx: Double, ny: Double): Unit = {
    node.asInstanceOf[Text].layoutX = nx
    node.asInstanceOf[Text].layoutY = ny
  }

  override def move_to(nx: Double, ny: Double): Unit = {
  val transition = new TranslateTransition {
      duration = Duration(2500)
      node = textNode
      toX = nx - textNode.layoutX()
      toY = ny - textNode.layoutY()
  }
    transition.play()

    transition.setOnFinished { _ =>
      textNode.layoutX = nx
      textNode.layoutY = ny
      textNode.translateX = 0
      textNode.translateY = 0
    }

  
  
    println(s"[ANIMATED MOVE] $name from (${textNode.layoutX()}, ${textNode.layoutY()}) to ($nx, $ny)")
  }

 
  def setMessage(msg: SI_Term): Unit = {
    textNode.text = msg.toString
    println(s"[SET MESSAGE] $name <- ${msg.toString}")
  }
 

}



case class rect_Widget(
  override val name: String,
  x: Double,
  y: Double,
  widthVal: Double,
  heightVal: Double,
  color: Color = Color.LightGray
) extends BSC_Widget(name) {


  val rectangle = new Rectangle {
    x_=(0) 
    y_=(0)
    width = widthVal
    height = heightVal
    fill = color
    stroke = Color.Black
    strokeWidth = 2
  }

  rectangle.layoutX = x
  rectangle.layoutY = y

  node = rectangle

  override def place_at(nx: Double, ny: Double): Unit = {
    rectangle.layoutX = nx
    rectangle.layoutY = ny
  }

  override def move_to(nx: Double, ny: Double): Unit = {
    println(s"[MOVE] Rectangle '$name' moved to ($nx, $ny)")
    place_at(nx, ny)
  }
}