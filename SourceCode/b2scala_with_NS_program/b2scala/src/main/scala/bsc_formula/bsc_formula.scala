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

package bscala.bsc_formula

import scala.language.postfixOps
import bscala.bsc_settings._
import bscala.bsc_data._


trait BHM_Formula { this: BHM_Formula =>
   def *(other: => BHM_Formula) = ConcatenationFormula( () => this, other _)
  	def +(other: => BHM_Formula) = ChoiceFormula( () => this, other _)
   def bsc_toString: String = { " " }

   
}

object bHM {
   def apply(f:BHM_Formula): BHM_Formula = CalledFormula(() => f)
}

case class EmptyBHM() extends BHM_Formula




trait BSC_Formula extends BHM_Formula {

   def and(other: BSC_Formula) = And_bf_formula(this, other)
   def or(other: BSC_Formula) = Or_bf_formula(this, other)
  	//def not()  = Neg_bf_formula(this)
   override def bsc_toString: String = { " " }
      

}
object not {
   def apply(f: BSC_Formula): BSC_Formula = Neg_bf_formula(f)
}

case class Bf_true_formula() extends BSC_Formula
case class Bf_false_formula() extends BSC_Formula
case class Bf_formula(si: SI_Term ) extends BSC_Formula
case class Neg_bf_formula(f:BSC_Formula) extends BSC_Formula
case class And_bf_formula(left:BSC_Formula, right:BSC_Formula) extends BSC_Formula
case class Or_bf_formula(left:BSC_Formula, right:BSC_Formula) extends BSC_Formula



object bf {
    def apply(siterm: SI_Term) = Bf_formula(siterm)
}


object bf_true {
    def apply() = Bf_true_formula()
}

object bf_false {
    def apply() = Bf_false_formula()
}

object Formula {
	def apply(Formula: BHM_Formula) = CalledFormula(() => Formula)
}



case class CalledFormula(fct_ag: () => BHM_Formula) extends BHM_Formula {
   override def bsc_toString: String = {
      "() => " + fct_ag().bsc_toString }
}

case class ConcatenationFormula(left: () => BHM_Formula, right: () => BHM_Formula) extends BHM_Formula {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ] ; [ ... ]" }
}

case class ChoiceFormula(left: () => BHM_Formula, right: () => BHM_Formula) extends BHM_Formula {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ]" + " + " + "[ " + "() => " + right().bsc_toString + " ]" }
}




