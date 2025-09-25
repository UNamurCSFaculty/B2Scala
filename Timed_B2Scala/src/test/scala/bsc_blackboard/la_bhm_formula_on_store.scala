package bscala

import bsc_data._
import bsc_blackboard._
import bsc_agent._
import bsc_runner._
import bsc_settings._
import bsc_formula._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LA_BHM_Form_on_Store extends AnyFlatSpec with Matchers {

    /* bf formula verified after tests
       -------------------------------
    */

    "bf(t)" should "succeed after tell(t)" in {

       val t = BSC_Token("t")
       BB.tell(t,10)

       val mybf = Bf_formula(t)
       val result = BB.is_bf_satisfied(mybf)

       result shouldBe true
    }


    /* bf formula verified after tests
       -------------------------------
    */

    "la_bf(3,bf(t))" should "succeed after tell(t)" in {

       val t = BSC_Token("t")
       BB.tell(t,10)

       val mybf = Bf_formula(t)
       val mylabf = LA_bf_formula(3,mybf)
       val real_form = la_bHM(mylabf)
       val res_eval_form = BB.is_la_bhm_satisfied(real_form)
       var result = true

       res_eval_form match {
          case (true, LaEmptyBHM() ) => { result = true }
	  case _ => { result = false }
       }
       
       result shouldBe true
    }
         




}
