/* ----------------------------------------------------- */
/*                                                       */
/*                GENERAL PATTERN FOR TESTS              */
/*                                                       */
/* see https://www.scalatest.org/user_guide/             */
/*                              writing_your_first_test  */
/*                                                       */
/* ----------------------------------------------------- */

/*

package bscala.bsc_agent

import org.scalatest.flatspec.AnyFlatSpec

class FirstSpec extends AnyFlatSpec {
  // tests go here...
}

*/


// or
// import org.scalatest.funsuite.AnyFunSuite
// class MathUtilsTests extends AnyFunSuite { }


package bscala

import bsc_data._
import bsc_agent._
import bsc_runner._
import bsc_settings._
import bsc_formula._

import org.scalatest.funsuite.AnyFunSuite

class primitiveTest extends AnyFunSuite {

  test("correct implementation of fbsc_toString for tell") {

    val myc = BSC_Token("c")
    val myTelltAgent = TelltAgent(myc,10)
    val result = myTelltAgent.fbsc_toString
    val expected = "tellt(c,10)"

    assert(result == expected)
    
  }

}





