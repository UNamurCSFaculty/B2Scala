package bscala.bsc_runner

import bscala.bsc_agent._
import bscala.bsc_blackboard._
import bscala.bsc_formula._
import bscala.bsc_widget._     
import bscala.bsc_settings._
import bscala._
import scala.language.postfixOps


class BSC_Runner_LA_BHM_Widget(val scene: BSC_Scene) extends BSC_Runner_LA_BHM {


  private val uiDelayMs: Long = 0L
  private def sleepUI(): Unit =
    if (uiDelayMs > 0) try Thread.sleep(uiDelayMs) catch { case _: InterruptedException => () }

  override def run_list_prim(lprim: List[BSC_Agent]): Unit = {
    lprim.foreach {
      case CommAgent(mess) =>
        println(mess)

      case a: BSC_Action =>
        scene.addAnimationStep(() => a.run()); sleepUI()

      case SetTxt(w: txt_Widget, value) =>
        scene.addAnimationStep(() => w.setMessage(value)); sleepUI()

      case ShowAgent(w) =>
        scene.addAnimationStep(() => w.show()); sleepUI()

      case HideAgent(w) =>
        scene.addAnimationStep(() => w.hide()); sleepUI()

      case MoveToAgent(w, x, y) =>
        scene.addAnimationStep(() => w.move_to(x, y)); sleepUI()

      case PlaceAtAgent(w, x, y) =>
        scene.addAnimationStep(() => w.place_at(x, y)); sleepUI()

      case TelltAgent(_, _) | AsktAgent(_, _) | NasktAgent(_, _) | GettAgent(_, _) | DelayAgent(_) =>
        () 

      case _ => ()
    }
  }



  override def lag_first_steps(n: search_node): List[search_node] = {
    n match {
      case bfs_node(ag, sigma, lprim) =>
        ag match {
          case a: BSC_Action =>
            List(bfs_node(EmptyAgent(), sigma, lprim :+ a))

          case SetTxt(_, _) =>
            List(bfs_node(EmptyAgent(), sigma, lprim :+ ag))

          case ShowAgent(_) | HideAgent(_) | MoveToAgent(_, _, _) | PlaceAtAgent(_, _, _) =>
            List(bfs_node(EmptyAgent(), sigma, lprim :+ ag))

          case _ =>
            super.lag_first_steps(n)
        }
      case _ =>
        super.lag_first_steps(n)
    }
  }
}
