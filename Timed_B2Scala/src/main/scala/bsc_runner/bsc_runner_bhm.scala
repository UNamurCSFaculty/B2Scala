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
import bscala.bsc_formula._
import bscala.bsc_settings._
import scala.util.Random
import scala.language.postfixOps
import bscala._
import bscala.TDuration
import bscala.TTimeStamp
import bscala.infiniteDuration

class BSC_Runner_BHM {

  val LOCAL_STORE = new BSC_Store
  
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
  
  def run_one(ag: BSC_Agent, f: BHM_Formula): (Boolean, BSC_Agent, BHM_Formula) = {

    ag match {

      case CalledAgent(fct_agent) => { run_one(fct_agent(),f) }

      case CommAgent(mess) => {
        println(mess)
        (true, EmptyAgent(), f)
      }
      
      case TelltAgent(si,d) => {
        val (satisfied, f_cont) = exec_tellt_primitive(si,d,f)
	if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }
      }
      
      case AsktAgent(si,d) => {
        val (satisfied, f_cont) = exec_askt_primitive(si,d,f)
	if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }
      }
      
      case NasktAgent(si,d) => {
        val (satisfied, f_cont) = exec_naskt_primitive(si,d,f)
	if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }
      }
      
      case GettAgent(si,d) => {
        val (satisfied, f_cont) = exec_gett_primitive(si,d,f)
	if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }
      }

      case DelayAgent(d) => {
          val (satisfied, f_cont) = exec_delay_primitive(d,f)
	  if (satisfied) {
            successMessage(ag)
            (true, EmptyAgent(), f_cont)
          } else {
            (false, ag, f)
          }
        }

      case ConcatenationAgent(fct_ag_i, fct_ag_ii) => {
        run_one(fct_ag_i(), f) match {
          case (false, _, _) => (false, ag, f)
          case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_ii), ff)
          case (true, ag_cont, ff) => (true, ConcatenationAgent(() => ag_cont, fct_ag_ii),ff)
        }
      }

      // to be used with arguments of the form
      // () => primitive,
      // () => list of communication/graphical primitives
      case StrongSeqAgent(fct_ag_i, fct_ag_ii) => {
        run_one(fct_ag_i(), f) match {
          case (false, _, _) => (false, ag, f)
          case (true, _, ff) => {
	     for (p <- fct_ag_ii()) { run_one(p, Bf_true_formula()) }
	     (true, EmptyAgent(), ff)
          }
        }
      }
      
      case ParallelAgent(fct_ag_i, fct_ag_ii) => {
        var branch_choice = db_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(fct_ag_i(), f) match {
            case (false, _, _) =>
              run_one(fct_ag_ii(), f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_i), ff)
                case (true, ag_cont, ff) => (true, ParallelAgent(fct_ag_i, () => ag_cont), ff)
              }
            case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_ii), ff)
            case (true, ag_cont, ff) => (true, ParallelAgent(() => ag_cont, fct_ag_ii), ff)
          }
        } else {
          run_one(fct_ag_ii(), f) match {
            case (false, _, _) =>
              run_one(fct_ag_i(), f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_ii), ff)
                case (true, ag_cont, ff) => (true, ParallelAgent(() => ag_cont,fct_ag_ii), ff)
              }
            case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_i), ff)
            case (true, ag_cont, ff) => (true, ParallelAgent(fct_ag_i, () => ag_cont), ff)
          }
        }
      }
      
      case ChoiceAgent(fct_ag_i, fct_ag_ii) => {
        var branch_choice = db_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(fct_ag_i(), f) match {
            case (false, _, _) =>
              run_one(fct_ag_ii(), f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
                case (true, ag_cont, ff) => (true, ag_cont, ff)
              }
            case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
            case (true, ag_cont, ff) => (true, ag_cont, ff)
          }
        } else {
          run_one(fct_ag_ii(), f) match {
            case (false, _, _) =>
              run_one(fct_ag_i(), f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
                case (true, ag_cont, ff) => (true, ag_cont, ff)
              }
            case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
            case (true, ag_cont, ff) => (true, ag_cont, ff)
          }
        }
      }
      
      case EmptyAgent() => { (true, EmptyAgent(), f) }

      case _ => {
          errorMessage(ag)
	  (false, EmptyAgent(), f)
      }
      
    }
  }


  /* Executing elementary primitives */
  /* ------------------------------- */

  // Note that the execution of the ask, nask and get primitives
  // do not need time arguments
  
  def exec_tellt_primitive(si: SI_Term, d: TDuration, f: BHM_Formula): (Boolean,BHM_Formula) = {
    LOCAL_STORE.copy_store(BB)
    val (satisfied, f_cont) = LOCAL_STORE.tellf(si,d,f)
    if (satisfied) {
         BB.tell(si,d)
         LOCAL_STORE.reset_store	 
	 (true,f_cont)
    } else {
         LOCAL_STORE.reset_store	 
         (false,f)
    }
  }

  def exec_askt_primitive(si: SI_Term, d: TDuration, f: BHM_Formula): (Boolean,BHM_Formula) = {
    if (d == 0) {
      (false,f)
    } else {
      BB.askf(si,f)
    }
  }

  def exec_naskt_primitive(si: SI_Term, d: TDuration, f: BHM_Formula): (Boolean,BHM_Formula) = {
    if (d == 0) {
      (false,f)
    } else {
      BB.naskf(si,f)
    }
  }

  def exec_gett_primitive(si: SI_Term, d: TDuration, f: BHM_Formula): (Boolean,BHM_Formula) = {
    if (d == 0) {
      (false,f)
    } else {
      LOCAL_STORE.copy_store(BB)
      val (satisfied, f_cont) = LOCAL_STORE.getf(si,f) 
      if (satisfied) {
           BB.getf(si,f)
           LOCAL_STORE.reset_store	 
  	  (true,f_cont)
      } else {
           LOCAL_STORE.reset_store	 
           (false,f)
      }
    }
  }

  def exec_delay_primitive(d: TDuration, f: BHM_Formula): (Boolean,BHM_Formula) = {
    if (d == 0) {
      val (satisfied, ff) = BB.is_bhm_satisfied(f)
      if (satisfied) {
           (true,ff)
      } else {
           (false,f)
      }
    } else {
        (false,f)  
    }
  }


  /* The main loop */
  /* ------------- */

  def execute(ag: BSC_Agent, f: BHM_Formula): (Boolean, BSC_Agent, BHM_Formula) = {

    var current_agent = ag
    var new_agent = current_agent.increase_time
    var current_formula = f    
    var failure = false
    var it_current_agent = ag
    var it_progress_agent = true
    var it_sigma = true

    println(" ")
    println(" ")
    println("Welcome to the Timed B2Scala BHM execution engine.")
    println("We are going to process the following query.")
    println(" ")
    println(ag.toString)
    println(" ")
    println(" ")
    
    while (    (current_agent != EmptyAgent())
               &&
	       (! current_agent.is_deadlock_def)
               && !failure
               && (it_progress_agent || it_sigma)
	  ) {

	
        while (current_agent != EmptyAgent() && !failure) {


	  
          run_one(current_agent, current_formula) match {
            case (false, _, _) =>
              failure = true 
            case (true, new_agent, new_formula) =>
              current_agent = new_agent
	      current_formula = new_formula
          }
	}
	
        if (current_agent != EmptyAgent()) {
            it_current_agent = current_agent.increase_time
	    it_progress_agent = !(current_agent == it_current_agent)
	    current_agent = it_current_agent
            it_sigma = BB.increase_time()
	    failure = false
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

    if (current_formula == EmptyBHM()) { 
          println(" ")
          println("Formula computation successfully ended")
          println(" "); println(" ")
    } else {
          println(" ")
          print("Formula computation blocked at ")
          println(current_formula.toString)
          println(" "); println(" ")
    }

    println(" ")
    println("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    println(" ")
    println(" ")

    // Returning the final result
    (failure, current_agent, current_formula)
  }

}
 



