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

package bscala.bsc_runner

import bscala.bsc_data._
import bscala.bsc_blackboard._
import bscala.bsc_agent._
import bscala.bsc_widget._
import bscala.bsc_settings._
import scala.util.Random
import scala.language.postfixOps
import bscala._
import bscala.TDuration
import bscala.TTimeStamp
import bscala.infiniteDuration

case class SetTxt(widget: txt_Widget, value: SI_Term) extends BSC_Agent


class BSC_Runner_Basic_widget(val scene: BSC_Scene) {
  /*

  ***************************************************************************
  This Runner is for running the protocol with a Scene and use widgets
  ***************************************************************************

  */

  val db_random_choice = new Random()


  /*  Displaying messages  */
  /*  -------------------  */

  def successMessage(ag: BSC_Agent) = {
      if (mySettings.verbose) {
        print("Successfully evaluated ")
	      println("\t\t" + ag.toString)
      }
  }

  def errorMessage(ag: BSC_Agent) = {
      if (mySettings.verbose) {
        print("!!! ERROR !!! with ")
	      println("\t\t" + ag.toString)
      }
  }


  /* Running one step  */
  /* ----------------  */
  
  def run_one(ag: BSC_Agent): (Boolean, BSC_Agent) = {

    ag match {

      case CalledAgent(fct_agent) => { run_one(fct_agent()) }

      case CommAgent(mess) => {
        if (mySettings.verbose) { println(mess) }
        (true, EmptyAgent())
      }
      
      case TelltAgent(si,d) => {
        if (exec_tellt_primitive(si,d)) {
          successMessage(ag)
          (true, EmptyAgent())
        } else {
          (false, ag)
        }
      }
      
      case AsktAgent(si,d) => {
        if (exec_askt_primitive(si,d)) {
          successMessage(ag)
          (true, EmptyAgent())
        } else {
          (false, ag)
        }
      }
      
      case NasktAgent(si,d) => {
        if (exec_naskt_primitive(si,d)) {
          successMessage(ag)
          (true, EmptyAgent())
        } else {
          (false, ag)
        }
      }
      
      case GettAgent(si,d) => {
        if (exec_gett_primitive(si,d)) {
          successMessage(ag)
          (true, EmptyAgent())
        } else {
          (false, ag)
        }
     }

      case DelayAgent(d) => {
          if (exec_delay_primitive(d)) {
            successMessage(ag)
            (true, EmptyAgent())
          } else {
            (false, ag)
          }
      }

      case ConcatenationAgent(fct_ag_i, fct_ag_ii) => {
        run_one(fct_ag_i()) match {
          case (false, _) => (false, ag)
          case (true, EmptyAgent()) => (true, CalledAgent(fct_ag_ii))
          case (true, ag_cont) => (true, ConcatenationAgent(() => ag_cont, fct_ag_ii))
        }
      }
      
      case ParallelAgent(fct_ag_i, fct_ag_ii) => {
        var branch_choice = db_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(fct_ag_i()) match {
            case (false, _) =>
              run_one(fct_ag_ii()) match {
                case (false, _) => (false, ag)
                case (true, EmptyAgent()) => (true, CalledAgent(fct_ag_i))
                case (true, ag_cont) => (true, ParallelAgent(fct_ag_i, () => ag_cont))
              }
            case (true, EmptyAgent()) => (true, CalledAgent(fct_ag_ii))
            case (true, ag_cont) => (true, ParallelAgent(() => ag_cont, fct_ag_ii))
          }
        } else {
          run_one(fct_ag_ii()) match {
            case (false, _) =>
              run_one(fct_ag_i()) match {
                case (false, _) => (false, ag)
                case (true, EmptyAgent()) => (true, CalledAgent(fct_ag_ii))
                case (true, ag_cont) => (true, ParallelAgent(() => ag_cont,fct_ag_ii))
              }
            case (true, EmptyAgent()) => (true, CalledAgent(fct_ag_i))
            case (true, ag_cont) => (true, ParallelAgent(fct_ag_i, () => ag_cont))
          }
        }
      }
      
      case ChoiceAgent(fct_ag_i, fct_ag_ii) => {
        var branch_choice = db_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(fct_ag_i()) match {
            case (false, _) =>
              run_one(fct_ag_ii()) match {
                case (false, _) => (false, ag)
                case (true, EmptyAgent()) => (true, EmptyAgent())
                case (true, ag_cont) => (true, ag_cont)
              }
            case (true, EmptyAgent()) => (true, EmptyAgent())
            case (true, ag_cont) => (true, ag_cont)
          }
        } else {
          run_one(fct_ag_ii()) match {
            case (false, _) =>
              run_one(fct_ag_i()) match {
                case (false, _) => (false, ag)
                case (true, EmptyAgent()) => (true, EmptyAgent())
                case (true, ag_cont) => (true, ag_cont)
              }
            case (true, EmptyAgent()) => (true, EmptyAgent())
            case (true, ag_cont) => (true, ag_cont)
          }
        }
      }
      
      case EmptyAgent() => { (true, EmptyAgent()) }

      case ShowAgent(widget) =>
      scene.addAnimationStep(() => widget.show())
      Thread.sleep(6000)
      (true, EmptyAgent())

      case HideAgent(widget) =>
        scene.addAnimationStep(() => widget.hide())
        Thread.sleep(6000)
        (true, EmptyAgent())

      case MoveToAgent(widget, x, y) =>
        scene.addAnimationStep(() => widget.move_to(x, y))
        //time sleep ? 
        Thread.sleep(6000)
        (true, EmptyAgent())

      case PlaceAtAgent(widget, x, y) =>
        scene.addAnimationStep(() => widget.place_at(x, y))
        Thread.sleep(6000)
        (true, EmptyAgent())

      case SetTxt(widget, value) =>
      scene.addAnimationStep(() => widget.setMessage(value))
      Thread.sleep(6000)
      (true, EmptyAgent())

      case _ => {
          errorMessage(ag)
	  (false, EmptyAgent())
      }
      
    }
  }


  /* Executing elementary primitives */
  /* ------------------------------- */

  // Note that the execution of the ask, nask and get primitives
  // do not need time arguments
  
  def exec_tellt_primitive(si: SI_Term, d: TDuration): Boolean = {
    BB.tell(si,d) 
  }

  def exec_askt_primitive(si: SI_Term, d: TDuration): Boolean = {
    BB.ask(si) 
  }

  def exec_naskt_primitive(si: SI_Term, d: TDuration): Boolean = {
    BB.nask(si) 
  }

  def exec_gett_primitive(si: SI_Term, d: TDuration): Boolean = {
    BB.get(si) 
  }

  def exec_delay_primitive(d: TDuration): Boolean = {
    if (d == 0) {
      true
    } else {
      false  
    }
  }


  /* The main loop */
  /* ------------- */

  def execute(ag: BSC_Agent): (Boolean, BSC_Agent) = {

    var current_agent = ag
    var new_agent = ag
    var failure = false
    var it_current_agent = ag
    var it_sigma = true

    println(" ")
    println(" ")
    println("Welcome to the B2Scala execution engine.")
    println("We are going to process the following query.")
    println(" ")
    println(ag.toString)
    println(" ")
    println(" ")
    
    while (    (current_agent != EmptyAgent() )
               &&
	       (it_current_agent != current_agent || it_sigma)
	  ) {
        
        current_agent = it_current_agent

        while (current_agent != EmptyAgent() && !failure) {

          run_one(current_agent) match {
            case (false, _) =>
              failure = true 
            case (true, new_agent) =>
              current_agent = new_agent 
          }
	}
	
        if (current_agent != EmptyAgent()) {
            it_current_agent = current_agent.increase_time
            it_sigma = BB.increase_time()
        }

    }
        

    println(" ")
    println(" ")
    println("=-=-=-=-=-= RESULT =-=-=-=-=-=-=")
    println(" ")

    if (mySettings.verbose) {
      if (current_agent == EmptyAgent()) {
        println(" ")
        println("Agent Computation successfully ended")
        println(" "); println(" ")
      } else {
        println(" ")
        print("Agent Computation blocked at ")
        println(current_agent.toString)
        println(" "); println(" ")
      }
    }

    println(" ")
    println("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    println(" ")
    println(" ")

    // Returning the final result
    (failure, current_agent)
  }

}
 



