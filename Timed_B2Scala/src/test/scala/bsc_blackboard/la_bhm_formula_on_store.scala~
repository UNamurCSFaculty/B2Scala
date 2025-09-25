package bscala

import bsc_data._
import bsc_blackboard._
import bsc_agent._
import bsc_runner._
import bsc_settings._
import bsc_formula._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BasicBBOperations extends AnyFlatSpec with Matchers {

    /* test on tell followed by ask
       ----------------------------
    */

    "tell(t) followed by ask(t)" should "succeed" in {

       val t = BSC_Token("t")
       BB.tell(t,10)
       val result = BB.ask(t)

       result shouldBe true
    }
         
}
