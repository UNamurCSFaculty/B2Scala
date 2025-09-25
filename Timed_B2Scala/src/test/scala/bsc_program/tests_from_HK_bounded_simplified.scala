package bscala

import bsc_data._
import bsc_blackboard._
import bsc_agent._
import bsc_runner._
import bsc_settings._
import bsc_formula._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class From_HK_bounded_simplified extends AnyFlatSpec with Matchers {





/* Verifier meets first challenge
   ------------------------------
*/   

   "verifier simplified" should "meet verifier challenge" in {

/* coding of HK
   ------------
*/


val c  = Token("c")     // challenge
val r  = Token("r")     // response

case class prover_site(x: SI_Term) extends SI_Term
case class verifier_site(x: SI_Term) extends SI_Term
case class attacker_site(x: SI_Term) extends SI_Term

val Verifier = Agent {
  delay(1) * 
  ( tell(verifier_site(c)) ** Comm("Verifier has told verifier_site(c)") ) *
  ( gett(1,verifier_site(r)) ** Comm("Verifier has got verifier_site(r)") ) 
}

val Prover = Agent {
  ( get(prover_site(c)) ** Comm("Prover has got prover_site(c)") ) *
  delay(1) * 
  ( tell(prover_site(r)) ** Comm("Prover has told prover_site(r)") )
}

val Attacker1 = Agent {
  ( get(verifier_site(c)) ** Comm("Attacker 1 has got verifier_site(c)") ) *
  delay(1) * 
  ( tell(attacker_site(c)) ** Comm("Attacker 1 has told attacker_site(c)") ) *
  ( get(attacker_site(r)) ** Comm("Attacker 1 has got attacher_site(r)") ) *
  delay(1) * 
  ( tell(verifier_site(r)) ** Comm("Attacker 1 has told verifier_site(r)") )
}

val Attacker2 = Agent {
  ( get(attacker_site(c)) ** Comm("Attacker 2 has got attacker_site(c)") ) *
  delay(1) * 
  ( tell(prover_site(c)) ** Comm("Attacker 2 has told prover_site(c)") ) *
  ( get(prover_site(r)) ** Comm("Attacker 2 has got prover_site(r)") ) *
  delay(1) * 
  ( tell(attacker_site(r)) ** Comm("Attacker 2 has told attacker_site(r)") )
}

val Protocol = Verifier || Prover || Attacker1 || Attacker2 

val verifier_challenge = la_bf(5, bf(verifier_site(c)))
/* val attack_underway = la_bf(5, bf(attacker_site(c)))
val attack_on_prover = la_bf(5, bf(prover_site(c)))
val prover_resp = la_bf(5,bf(prover_site(r)))
val attacker_resp = la_bf(5, bf(attacker_site(r)))
val verifier_resp = la_bf(5, bf(verifier_site(r)))
val message_proved = la_bf(5, not(bf(verifier_site(r))))
// val fin_avec_vrai = la_Do_Not_Mind
// val Fraud = la_bHM { attack_underway * message_proved }
val Fraud = la_bHM { attack_underway *
                     attack_on_prover *
		     prover_resp *
		     attacker_resp *
		     verifier_resp *
		     message_proved }
*/

val runner = new BSC_Runner_LA_BHM
// runner.execute(Protocol,Fraud)




// For the test

       val Verifier_Aux =
           Agent { delay(1) * 
                   ( tell(verifier_site(c)) **
		        Comm("Verifier has told verifier_site(c)") ) 
           }

       val vc_la_bhm = la_bHM(verifier_challenge)

// algo from runner starts here

       val ag = Verifier_Aux
       val f = vc_la_bhm
       
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

       current_open += la_bfs_node(ag, BB, f, List[BSC_Agent]())

       first_el = current_open.head
       current_open = current_open.tail
       if (! current_closed.contains(first_el)) { current_closed += first_el }

       first_el match {

               case la_bfs_node(ag, store, f, l_prim) => {
                   l_bfs_node = runner.lag_first_and_time_steps( bfs_node(ag,store,l_prim) )
		   for (elm <- l_bfs_node) {
                      if (!found) {
                           val (ok_form, form_rem) = runner.check_form_on_node(elm,f)
                           val (ag_elm,store_elm,lprim_elm) = runner.extract_from_node(elm)
                           if (ok_form) {
			     found = true
			     res_bool = true
		             res_ag = ag_elm
			     res_store = store_elm
			     res_form = form_rem
			     res_lprim = lprim_elm
                           } else {
			     val f_older = f.older_by_one_step
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

        // result returned
	// (res_bool, res_ag, res_store, res_form, res_lprim)

        res_bool shouldBe true
        
}



