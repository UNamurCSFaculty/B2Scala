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


class search_node
case class empty_node() extends search_node
case class bfs_node(ag: BSC_Agent, store:  BSC_Store, lprim: List[BSC_Agent]) extends search_node

class la_search_node
case class la_empty_node() extends la_search_node
case class la_bfs_node(ag: BSC_Agent, store:  BSC_Store, f: LA_BHM_Formula, lprim: List[BSC_Agent]) extends la_search_node



class BSC_Runner_LA_BHM {

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


  /* Running first steps  */
  /* -------------------  */

  def extend_seq(l: List[search_node], fct_ag: () => BSC_Agent) : List[search_node] = {

     l.map( x => { x match {
           case bfs_node(ag,store,lprim) => {
              if (ag == EmptyAgent()) {
	          bfs_node(fct_ag(),store,lprim)
	      } else {
		  bfs_node(ConcatenationAgent(() => ag, fct_ag),store,lprim)
	      } }
	    case _ => { empty_node() }
      } } )
   }

   def extend_para(l: List[search_node], fct_ag : () => BSC_Agent,i : Int) : List[search_node] = {
      l.map( x => { x match {
           case bfs_node(ag,store,lprim) => {
               if (ag == EmptyAgent()) {
                  bfs_node(fct_ag(),store,lprim)
               } else {
                   if (i == 1) {
    		      bfs_node(ParallelAgent(() => ag, fct_ag),store,lprim)
   		   } else {
                      bfs_node(ParallelAgent(fct_ag,() => ag),store,lprim)
		   }
		}
	      }
	    case _ => { empty_node() }
      } } )
   }

   def extend_sseq_lprim(l: List[search_node], fct_lprim: () => List[BSC_Agent]) : List[search_node] = {
      l.map( x => { x match {
            case bfs_node(ag,store,lprim) => { bfs_node(ag,store,lprim ::: fct_lprim()) }
	    case _ => { empty_node() }
      } } )
   }

   def lag_first_steps(n: search_node): List[search_node] = {

     n match {

         case bfs_node(ag,sigma,lprim) => {

            ag match {

              case CalledAgent(fct_agent) => {
	           lag_first_steps( bfs_node(fct_agent(),sigma,lprim) )
              }

              case CommAgent(mess) => {
                    List( bfs_node(EmptyAgent(),sigma,lprim :+ CommAgent(mess)) )
              }

              case TelltAgent(si,d) => {
                    val n_store = new BSC_Store
		    n_store.copy_store(sigma)
		    val op_ok = n_store.tell(si,d)
		    if (op_ok) {
                       List( bfs_node(EmptyAgent(),n_store,lprim :+ TelltAgent(si,d)) )
                    } else {
                       List[ search_node ]()
                    }
              }

              case AsktAgent(si,d) => {
                    if (d == 0) {
                       List[ search_node ]()
		    } else {
                       val n_store = new BSC_Store
                       n_store.copy_store(sigma)
		       val op_ok = n_store.ask(si)
		       if (op_ok) {
                          List( bfs_node(EmptyAgent(),n_store,lprim :+ AsktAgent(si,d)) )
                       } else {
                          List[ search_node ]()
                       }
		    }
              }

              case NasktAgent(si,d) => {
                    if (d == 0) {
                       List[ search_node ]()
		    } else {
                       val n_store = new BSC_Store
                       n_store.copy_store(sigma)
		       val op_ok = n_store.nask(si)
		       if (op_ok) {
                          List( bfs_node(EmptyAgent(),n_store,lprim :+ NasktAgent(si,d)) )
                       } else {
                          List[ search_node ]()
                       }
		    }
              }

              case GettAgent(si,d) => {
                    if (d == 0) {
                       List[ search_node ]()
		    } else {
                       val n_store = new BSC_Store
                       n_store.copy_store(sigma)
		       val op_ok = n_store.get(si)
		       if (op_ok) {
                          List( bfs_node(EmptyAgent(),n_store,lprim :+ GettAgent(si,d)) )
                       } else {
                          List[ search_node ]()
                       }
		    }
              }

              case DelayAgent(d) => {
                    val n_store = new BSC_Store
		    n_store.copy_store(sigma)
                    if (d == 0) {
                       List( bfs_node(EmptyAgent(),n_store,lprim :+ DelayAgent(d)) )
                    } else {
                       List[ search_node ]()
                    }
              }

              case ConcatenationAgent(fct_ag_i, fct_ag_ii) => {
                    val li = lag_first_steps( bfs_node(fct_ag_i(), sigma, lprim) )
		    extend_seq(li, fct_ag_ii)
              }

              // to be used with arguments of the form
              // () => primitive,
              // () => list of communication/graphical primitives
              case StrongSeqAgent(fct_ag_i, fct_ag_ii) => {
                    val li = lag_first_steps( bfs_node(fct_ag_i(), sigma, lprim) )
                    extend_sseq_lprim(li,fct_ag_ii)
              }
	      
              case ParallelAgent(fct_ag_i, fct_ag_ii) => {
                    val li  = lag_first_steps( bfs_node(fct_ag_i(), sigma, lprim) )
                    val lii = lag_first_steps( bfs_node(fct_ag_ii(), sigma, lprim) )
	            extend_para(li,fct_ag_ii,1) ::: extend_para(lii,fct_ag_i,2)
              }

              case ChoiceAgent(fct_ag_i, fct_ag_ii) => {
                    val li = lag_first_steps( bfs_node(fct_ag_i(), sigma, lprim) )
                    val lii = lag_first_steps( bfs_node(fct_ag_ii(), sigma, lprim) )
	            li ::: lii
              }

              case EmptyAgent() => List[ search_node ]()

              case _ => List[ search_node ]()

         } }
         
         case _ => List[search_node]()

       }

   }

   def lag_time_step(n: search_node): List[search_node] = {

     n match {

         case bfs_node(ag,sigma,lprim) => {

               if ( (ag != EmptyAgent()) && (! ag.is_deadlock_def) ) {
                    val n_store = new BSC_Store
		    n_store.copy_store(sigma)
                    val ag_older = ag.increase_time
                    val it_sigma = n_store.increase_time()
		    if ((ag != ag_older) || it_sigma) {
		      List( bfs_node(ag_older,n_store,lprim) )
		    } else {
                      List[ search_node ]()
		    }
		} else {
                   List[ search_node ]()
		}

          }

          case _ => List[ search_node ]()

     }

   }

   def lag_first_and_time_steps(n: search_node): List[search_node] = {

       val l_bfs_node = lag_first_steps(n)
       if (l_bfs_node.isEmpty) { lag_time_step(n)       
       } else { l_bfs_node }

   }

   def check_form_on_node(elm: search_node,f : LA_BHM_Formula): (Boolean,LA_BHM_Formula) = {

       elm match {
          case bfs_node(ag, store, lprim) => {
             val (satisfied,f_cont) = store.is_la_bhm_satisfied(f)
	     (satisfied, f_cont)
          }
          case _ => {
	     (false,LaEmptyBHM())
	  }
       }
   }


   def extract_from_node(elm: search_node): (BSC_Agent,BSC_Store,List[BSC_Agent]) = {

       elm match {
          case bfs_node(ag, store, lprim) => {
             (ag,store,lprim)
          }
          case _ => {
	     (EmptyAgent(),BB,List[BSC_Agent]())
	  }
       }
   }


   def search_for_one_big_step(ag: BSC_Agent, f: LA_BHM_Formula):
                                       (Boolean, BSC_Agent, BSC_Store, LA_BHM_Formula,List[BSC_Agent]) = {

        var found = false
        var current_open = scala.collection.mutable.ListBuffer[la_search_node]()
	var current_closed = scala.collection.mutable.ListBuffer[la_search_node]()
        var first_el: la_search_node = la_empty_node()
        var l_bfs_node = List[search_node]()

        var res_bool = false
	var res_ag:BSC_Agent = EmptyAgent()
	var res_store:BSC_Store = BB
        var res_form:LA_BHM_Formula = LaEmptyBHM()
	var res_lprim:List[BSC_Agent ] = List[BSC_Agent]()

	var mylinestop = " "
	
        current_open += la_bfs_node(ag, BB, f, List[BSC_Agent]())

        while ( (!found) && (!current_open.isEmpty) ) {

	   
           first_el = current_open.head
        
	   current_open = current_open.tail
           if (! current_closed.contains(first_el)) { current_closed += first_el }

           first_el match {

               case la_bfs_node(ag, store, f, l_prim) => {

                      if (!found) {
                           val (ok_form, form_rem) = check_form_on_node( bfs_node(ag,store, l_prim), f)
                           if (ok_form) {
			     found = true
			     res_bool = true
		             res_ag = ag
			     res_store = store
			     res_form = form_rem
			     res_lprim = l_prim
                           } else {
			     val f_older = f.older_by_one_step
                             l_bfs_node = lag_first_and_time_steps( bfs_node(ag,store,l_prim) )
                             for (elm <- l_bfs_node) {
                                val (ag_elm,store_elm,lprim_elm) = extract_from_node(elm)
                                val new_la_node = la_bfs_node(ag_elm,store_elm,f_older,lprim_elm)
                                if ( !(f_older == LaEmptyBHM()) &&
			           (! current_open.contains(new_la_node)) &&
			           (! current_closed.contains(new_la_node))
				   ) {
                                     current_open += new_la_node
                                     }
			     }
                           }
		       }
               }

               case _ => { }
	          
           }


        }

        (res_bool, res_ag, res_store, res_form, res_lprim)

   }


   def run_list_prim(lprim:List[BSC_Agent]) = {
      for (e <- lprim) {
         e match {

            case CommAgent(mess) => {
                println(mess)
            }

            case TelltAgent(si,d) => {
            }

            case AsktAgent(si,d) => {
            }

            case NasktAgent(si,d) => {
            }

            case GettAgent(si,d) => {
            }

            case DelayAgent(d) => {
            }

            case _ => {
            }

         }
      }
   }


   def run_one_big_step(ag: BSC_Agent, f: LA_BHM_Formula): (Boolean, BSC_Agent, LA_BHM_Formula) = {

      val (res_search, ag_cont, store_cont, f_cont, l_prim) = search_for_one_big_step(ag,f)
      if (res_search) {
           BB.copy_store(store_cont)
	   run_list_prim(l_prim)
	   (res_search,ag_cont,f_cont)
      } else {
           (res_search,ag,f)
      }
   }



  /* The main loop */
  /* ------------- */

  def execute(ag: BSC_Agent, f: LA_BHM_Formula): (Boolean, BSC_Agent, LA_BHM_Formula) = {

    var current_agent = ag
    var new_agent = current_agent.increase_time
    var current_formula = f    
    var failure = false
    var it_current_agent = ag
    var it_sigma = true

    println(" ")
    println(" ")
    println("Welcome to the Time B2Scala LA BHM execution engine.")
    println("We are going to process the following query.")
    println(" ")
    println(ag.toString)
    println(" ")
    println(" ")
    
    while (    (current_agent != EmptyAgent())
               &&
               (! current_agent.is_deadlock_def)	       
               && !failure
               &&
	       (current_formula != LaEmptyBHM())	       
               &&
	       ((it_current_agent != current_agent) || it_sigma)
	  ) {

	
        current_agent = it_current_agent

        while ( (current_agent != EmptyAgent())
	        && (current_formula != LaEmptyBHM())
		&& !failure) {


          run_one_big_step(current_agent, current_formula) match {
            case (false, _, _) =>
              failure = true 
            case (true, new_agent, new_formula) =>
              current_agent = new_agent
	      current_formula = new_formula
          }


        }
	
        if (current_agent != EmptyAgent()) {
            it_current_agent = current_agent.increase_time
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

    if (current_formula == LaEmptyBHM()) { 
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
 



