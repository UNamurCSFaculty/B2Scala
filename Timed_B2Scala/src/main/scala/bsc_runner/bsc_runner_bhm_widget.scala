package bscala.bsc_runner

import bscala.bsc_data._
import bscala.bsc_blackboard._
import bscala.bsc_agent._
import bscala.bsc_formula._
import bscala.bsc_widget._
import bscala.bsc_settings._
import scala.util.Random
import scala.language.postfixOps
import bscala._
import bscala.TDuration
import bscala.TTimeStamp
import bscala.infiniteDuration

/**
 * Hybrid runner: BHM semantics (store + logical formula) + widget animations.
 * It delegates ONLY the UI primitives to the scene, and lets BSC_Runner_BHM
 * handle all protocol primitives (tell/get/ask/nask/delay, + time & formula).
 */


class BSC_Runner_BHM_Widget(val scene: BSC_Scene) extends BSC_Runner_BHM {

  private val uiDelayMs: Long = 6000L

  override def run_one(ag: BSC_Agent, f: BHM_Formula): (Boolean, BSC_Agent, BHM_Formula) = ag match {

    case ShowAgent(widget) =>
      scene.addAnimationStep(() => widget.show())
      Thread.sleep(uiDelayMs)
      successMessage(ag)
      (true, EmptyAgent(), f)

    case HideAgent(widget) =>
      scene.addAnimationStep(() => widget.hide())
      Thread.sleep(uiDelayMs)
      successMessage(ag)
      (true, EmptyAgent(), f)

    case MoveToAgent(widget, x, y) =>
      scene.addAnimationStep(() => widget.move_to(x, y))
      Thread.sleep(uiDelayMs)
      successMessage(ag)
      (true, EmptyAgent(), f)

    case PlaceAtAgent(widget, x, y) =>
      scene.addAnimationStep(() => widget.place_at(x, y))
      Thread.sleep(uiDelayMs)
      successMessage(ag)
      (true, EmptyAgent(), f)

    case SetTxt(widget, value) =>
      scene.addAnimationStep(() => widget.setMessage(value))
      Thread.sleep(uiDelayMs)
      successMessage(ag)
      (true, EmptyAgent(), f)

    case _ =>
      super.run_one(ag, f)
  }
}
