package bscala.bsc_formula
import scala.language.postfixOps
import bscala.bsc_settings._
import bscala.bsc_data._


/* -------------------------------------------------------------- */
/*                                                                */
/*                      TOP BHM FORMULAE                          */
/*                                                                */
/* -------------------------------------------------------------- */


/* BHM */

trait BHM_Formula { this: BHM_Formula =>
   def *(other: => BHM_Formula) = ConcatenationFormula( () => this, other _)
   def +(other: => BHM_Formula) = ChoiceFormula( () => this, other _)
   def bsc_toString: String = { " " }
   def fbsc_toString: String = { " " }
}

object bHM {
   def apply(f:BHM_Formula): BHM_Formula = CalledFormula(() => f)
}

/* LA_BHM */

trait LA_BHM_Formula { this: LA_BHM_Formula =>
   def *(other: => LA_BHM_Formula) = LaConcatenationFormula( () => this, other _)
   def +(other: => LA_BHM_Formula) = LaChoiceFormula( () => this, other _)
   def bsc_toString: String = { " " }
   def fbsc_toString: String = { " " }
   def older_by_one_step : LA_BHM_Formula = { this }
}

object la_bHM {
   def apply(f:LA_BHM_Formula): LA_BHM_Formula = LaCalledFormula(() => f)
}



/* -------------------------------------------------------------- */
/*                                                                */
/*                        BASIC FORMULAE                          */
/*                                                                */
/* -------------------------------------------------------------- */

trait BSC_Formula extends BHM_Formula {

   def and(other: BSC_Formula) = And_bf_formula(this, other)
   def or(other: BSC_Formula) = Or_bf_formula(this, other)
   //def not()  = Neg_bf_formula(this)
   override def bsc_toString: String = { " " }
   override def fbsc_toString: String = { " " }   
}

case class Bf_true_formula() extends BSC_Formula {
   override def fbsc_toString: String = { "true" }
}

case class Bf_false_formula() extends BSC_Formula {
   override def fbsc_toString: String = { "false" }
}

case class Bf_formula(si: SI_Term ) extends BSC_Formula {
   override def fbsc_toString: String = { si.bsc_toString }
}

case class Neg_bf_formula(f:BSC_Formula) extends BSC_Formula {
   override def fbsc_toString: String = { "not (" + f.fbsc_toString + ")" }
}

case class And_bf_formula(left:BSC_Formula, right:BSC_Formula) extends BSC_Formula {
   override def fbsc_toString: String = { left.fbsc_toString + " && " + right.fbsc_toString }
}

case class Or_bf_formula(left:BSC_Formula, right:BSC_Formula) extends BSC_Formula {
   override def fbsc_toString: String = { left.fbsc_toString + " || " + right.fbsc_toString }
}


object bf {
    def apply(siterm: SI_Term) = Bf_formula(siterm)
}

object not {
   def apply(f: BSC_Formula): BSC_Formula = Neg_bf_formula(f)
}

object bf_true {
    def apply() = Bf_true_formula()
}

object bf_false {
    def apply() = Bf_false_formula()
}


/* -------------------------------------------------------------- */
/*                                                                */
/*                LOOK-AHEAD BASIC FORMULAE                       */
/*                                                                */
/* -------------------------------------------------------------- */

trait LA_BSC_Formula extends LA_BHM_Formula {
   override def older_by_one_step : LA_BSC_Formula = { this }
   def without_la : BSC_Formula = { Bf_false_formula() }      
   override def fbsc_toString: String = { " " }
}

case class LA_bf_stop_formula() extends LA_BSC_Formula {
   override def older_by_one_step: LA_BSC_Formula = { this }
   override def without_la : BSC_Formula = { Bf_false_formula() }   
   override def fbsc_toString: String = { "STOP" }
}

case class LA_bf_formula(n: Int, bf: BSC_Formula) extends LA_BSC_Formula {
   override def older_by_one_step: LA_BSC_Formula = {
      if (n>0) { LA_bf_formula(n-1,bf)
      } else { LA_bf_stop_formula() }
   }
   override def without_la : BSC_Formula = { bf }
   override def fbsc_toString: String = { "(" + n + ")" + bf.fbsc_toString }
   
}

object la_bf {
    def apply(n: Int, bf: BSC_Formula) = LA_bf_formula(n,bf)
}



/* -------------------------------------------------------------- */
/*                                                                */
/*                            BHM LOGIC                           */
/*                                                                */
/* -------------------------------------------------------------- */


object Formula {
	def apply(f: BHM_Formula) = CalledFormula(() => f)
}

case class EmptyBHM() extends BHM_Formula {
   override def bsc_toString: String = {
      "Stop BHM Formula" }
   override def fbsc_toString: String = {
      "EmptyBHM" }
}

case class CalledFormula(fct_ag: () => BHM_Formula) extends BHM_Formula {
   override def bsc_toString: String = {
      "() => " + fct_ag().bsc_toString
   }
   override def fbsc_toString: String = {
      "() => " + fct_ag().fbsc_toString
   }
}

case class ConcatenationFormula(left: () => BHM_Formula, right: () => BHM_Formula) extends BHM_Formula {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ] ; [ ... ]" 
   }
   override def fbsc_toString: String = {
      "[ " + left().fbsc_toString + " ] ; [ ... ]" 
   }
}

case class ChoiceFormula(left: () => BHM_Formula, right: () => BHM_Formula) extends BHM_Formula {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ]" + " + " + "[ " + "() => " + right().bsc_toString + " ]" 
   }
   override def fbsc_toString: String = {
      "[ " + "() => " + left().fbsc_toString + " ]" + " + " + "[ " + "() => " + right().fbsc_toString + " ]" 
   }
}




/* -------------------------------------------------------------- */
/*                                                                */
/*                          LA_BHM LOGIC                          */
/*                                                                */
/* -------------------------------------------------------------- */

object LAFormula {
	def apply(f: LA_BHM_Formula) = LaCalledFormula(() => f)
}

case class LaEmptyBHM() extends LA_BHM_Formula {
   override def bsc_toString: String = {
      "Stop LA_BHM Formula"
   }
   override def fbsc_toString: String = {
      "LA_BHM_Empty" }
   override def older_by_one_step: LA_BHM_Formula = { this }
}

case class LaCalledFormula(fct_ag: () => LA_BHM_Formula) extends LA_BHM_Formula {
   override def bsc_toString: String = {
      "() => " + fct_ag().bsc_toString }
   override def fbsc_toString: String = {
      "() => " + fct_ag().fbsc_toString }
   override def older_by_one_step: LA_BHM_Formula = { 
      LaCalledFormula( () => fct_ag().older_by_one_step )
   }
}

case class LaConcatenationFormula(left: () => LA_BHM_Formula, right: () => LA_BHM_Formula) extends LA_BHM_Formula {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ] ; [ ... ]" }
   override def fbsc_toString: String = {
      "[ " + "() => " + left().fbsc_toString + " ] ; [ ... ]" }
   override def older_by_one_step : LA_BHM_Formula = {
      val left_older = left().older_by_one_step
      if ( (left_older == LaEmptyBHM()) || (left_older == LA_bf_stop_formula()) ) {
        LaEmptyBHM()
      } else {
        LaConcatenationFormula( () => left_older, right )
      }
   }
}

case class LaChoiceFormula(left: () => LA_BHM_Formula, right: () => LA_BHM_Formula) extends LA_BHM_Formula {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ]" + " + " + "[ " + "() => " + right().bsc_toString + " ]" }
   override def fbsc_toString: String = {
      "[ " + "() => " + left().fbsc_toString + " ]" + " + " + "[ " + "() => " + right().fbsc_toString + " ]" }
   override def older_by_one_step : LA_BHM_Formula = {
      val left_older = left().older_by_one_step
      val right_older = right().older_by_one_step
      if ( (left_older == LaEmptyBHM()) || (left_older == LA_bf_stop_formula()) ) {
        if ( (right_older == LaEmptyBHM()) || (right_older == LA_bf_stop_formula()) ) {
	  LaEmptyBHM()
	} else {
	  right_older
	}
      } else {
        if ( (right_older == LaEmptyBHM()) || (right_older == LA_bf_stop_formula()) ) {
	  left_older
	} else {
	  LaChoiceFormula( () => left_older, () => right_older)
	}
      }
   }  
}




