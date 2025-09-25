package bscala.bsc_agent

import bscala.bsc_data._
import bscala.bsc_agent._
import bscala.bsc_widget._


object show {
  def apply(w: BSC_Widget): BSC_Agent = ShowAgent(w)
}

object hide {
  def apply(w: BSC_Widget): BSC_Agent = HideAgent(w)
}

object move_to {
  def apply(w: BSC_Widget, x: Double, y: Double): BSC_Agent = MoveToAgent(w, x, y)
}

object place_at {
  def apply(w: BSC_Widget, x: Double, y: Double): BSC_Agent = PlaceAtAgent(w, x, y)
}


case class ShowAgent(widget: BSC_Widget) extends BSC_Agent {
  override def bsc_toString: String = s"show(${widget.name})"
  override def increase_time: BSC_Agent = this
}

case class HideAgent(widget: BSC_Widget) extends BSC_Agent {
  override def bsc_toString: String = s"hide(${widget.name})"
  override def increase_time: BSC_Agent = this
}

case class MoveToAgent(widget: BSC_Widget, x: Double, y: Double) extends BSC_Agent {
  override def bsc_toString: String = s"move_to(${widget.name}, $x, $y)"
  override def increase_time: BSC_Agent = this
}

case class PlaceAtAgent(widget: BSC_Widget, x: Double, y: Double) extends BSC_Agent {
  override def bsc_toString: String = s"place_at(${widget.name}, $x, $y)"
  override def increase_time: BSC_Agent = this
}
