/***********************************************************************

 Copyright (c) 2024 Jean-Marie Jacquet and the CoordiNam Lab members
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
import bscala.bsc_settings._
import bscala.bsc_formula._
import scala.util.Random
import scala.language.postfixOps

class BSC_Runner {

   val db_random_choice = new Random()

  def successMessage(ag: BSC_Agent) = {
      if (mySettings.verbose) {
        print("Successfully evaluated ")
	      println("\t\t" + ag.toString)
      }
  }

  /*def successMessage(ag: BSC_Agent) = {
  if (mySettings.verbose) {
    println("Successfully evaluated: ")
    println("\t\t\t" + ag.toString)
    println()
  }
  }*/

   
  def successMessage(bf: BHM_Formula) = {
    if (mySettings.verbose) {
      print("Successfully evaluated ")
      println(bf.toString)
    }
  }



  def run_one(ag: BSC_Agent,f:BHM_Formula):(Boolean,BSC_Agent,BHM_Formula) = {
    ag match {
      case CalledAgent(fct_agent) => run_one(fct_agent(), f)

      case CommAgent(mess) => {
        if (mySettings.verbose) { println(mess) }
        (true, EmptyAgent(), f)
      }

      case TellAgent(si) => {
        val (satisfied, f_cont) = exec_tell_primitive(si, f)
        if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }
      }

      case AskAgent(si) => { 
        val (satisfied, f_cont) = exec_ask_primitive(si, f)
        if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }
      }

      case NaskAgent(si) => 
        val (satisfied, f_cont) = exec_nask_primitive(si, f)
        if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }

      case GetAgent(si) =>
        val (satisfied, f_cont) = exec_get_primitive(si, f)
        if (satisfied) {
          successMessage(ag)
          (true, EmptyAgent(), f_cont)
        } else {
          (false, ag, f)
        }

      case ConcatenationAgent(fct_ag_i, fct_ag_ii) =>
        run_one(fct_ag_i apply, f) match {
          case (false, _, _) => (false, ag, f)
          case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_ii), ff)
          case (true, ag_cont, ff) => (true, ConcatenationAgent(() => ag_cont, fct_ag_ii), ff)
        }

      case ParallelAgent(fct_ag_i, fct_ag_ii) =>
        var branch_choice = db_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(fct_ag_i apply, f) match {
            case (false, _, _) =>
              run_one(fct_ag_ii apply, f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_i), ff)
                case (true, ag_cont, ff) => (true, ParallelAgent(fct_ag_i, () => ag_cont), ff)
              }
            case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_ii), ff)
            case (true, ag_cont, ff) => (true, ParallelAgent(() => ag_cont, fct_ag_ii), ff)
          }
        } else {
          run_one(fct_ag_ii apply, f) match {
            case (false, _, _) =>
              run_one(fct_ag_i apply, f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_ii), ff)
                case (true, ag_cont, ff) => (true, ParallelAgent(() => ag_cont, fct_ag_ii), ff)
              }
            case (true, EmptyAgent(), ff) => (true, CalledAgent(fct_ag_i), ff)
            case (true, ag_cont, ff) => (true, ParallelAgent(fct_ag_i, () => ag_cont), ff)
          }
        }

      case ChoiceAgent(fct_ag_i, fct_ag_ii) =>
        var branch_choice = db_random_choice.nextInt(2)
        if (branch_choice == 0) {
          run_one(fct_ag_i apply, f) match {
            case (false, _, _) =>
              run_one(fct_ag_ii apply, f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
                case (true, ag_cont, ff) => (true, ag_cont, ff)
              }
            case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
            case (true, ag_cont, ff) => (true, ag_cont, ff)
          }
        } else {
          run_one(fct_ag_ii apply, f) match {
            case (false, _, _) =>
              run_one(fct_ag_i apply, f) match {
                case (false, _, _) => (false, ag, f)
                case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
                case (true, ag_cont, ff) => (true, ag_cont, ff)
              }
            case (true, EmptyAgent(), ff) => (true, EmptyAgent(), ff)
            case (true, ag_cont, ff) => (true, ag_cont, ff)
          }
        }
    
      case EmptyAgent() => (true, EmptyAgent(), f)


    }
  }




   def exec_tell_primitive(si: SI_Term,f:BHM_Formula) = {
       BB.tellf(si,f)
   }

   def exec_ask_primitive(si: SI_Term,f:BHM_Formula) = {
       BB.askf(si,f)
   }

   def exec_nask_primitive(si: SI_Term,f:BHM_Formula) = {
       BB.naskf(si,f)
   }

   def exec_get_primitive(si: SI_Term,f:BHM_Formula) = {
       BB.getf(si,f)
   }

   def execute(ag: BSC_Agent, f: BHM_Formula) = {

    var current_agent = ag
    var current_formula = f
    var failure = false
    var i = 0

    println(" ")
    println(" ")
    println("Welcome to the B2Scala excution engine.")
    println("We are going to process the following query.")
    println(" ")
    println(ag.toString)
    println(" ")
    println(" ")

    while ((current_agent != EmptyAgent() && current_formula != EmptyBHM()) && !failure) {

      run_one(current_agent, current_formula) match {
        case (false, _, _) => failure = true
        case (true, new_agent, new_formula) =>
          current_agent = new_agent
          current_formula = new_formula
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

      if (current_formula == EmptyBHM()) {
        println(" ")
        println("Formula Computation successfully ended")
        println(" "); println(" ")
      } else {
        println(" ")
        print("Formula Computation blocked at ")
        println(current_agent.toString)
        println(" "); println(" ")
      }
    }

    println(" ")
    println("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    println(" ")
    println(" ")

  }

}


