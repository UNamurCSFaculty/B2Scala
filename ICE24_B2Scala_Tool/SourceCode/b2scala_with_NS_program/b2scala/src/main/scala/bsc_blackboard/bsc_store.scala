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

package bscala.bsc_blackboard

import bscala.bsc_data._
import bscala.bsc_formula._

import scala.collection.mutable.Map


class BSC_Store {

   var theStore = Map[SI_Term,Int]()

   def nb_occ(si: SI_Term) = synchronized {
      if (theStore.contains(si)) { theStore(si) }
      else { 0 }
   }

   def storeContents = synchronized {
      theStore
   }


   def set_store(s:scala.collection.immutable.Map[SI_Term,Int]) =
      synchronized {
          theStore = scala.collection.mutable.Map() ++ s
      }


   def is_bf_satisfies(f:BSC_Formula) : Boolean = synchronized {
   f match {
      case Bf_true_formula() => true
      case Bf_false_formula() => false
      case Bf_formula(si) => ask(si)
      case Neg_bf_formula(f) => !is_bf_satisfies(f)
      case And_bf_formula(left, right) => is_bf_satisfies(left) && is_bf_satisfies(right)
      case Or_bf_formula(left, right) => is_bf_satisfies(left) || is_bf_satisfies(right)
      case _ => (false)
   }
   
   }


   def is_bhm_satisfies(f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
      if (f.isInstanceOf[BSC_Formula]){
         if (is_bf_satisfies(f.asInstanceOf[BSC_Formula])){
               (true,EmptyBHM())
            } else {
               (false,f)
            }
      } else {
         f match {
            case EmptyBHM() => (false, f)

            case CalledFormula(fct_ag) => is_bhm_satisfies((fct_ag()))

            case ConcatenationFormula(left, right) => {
            val (leftSatisfied, ff) = is_bhm_satisfies(left())
            if (leftSatisfied) {
               if (ff == EmptyBHM()) {
                  (true, right())
               } else {
                  (true, ConcatenationFormula(() => ff, right))
               }
            } else {
               (false, f)
            }
            }

            case ChoiceFormula(left, right) => {
            val random = new scala.util.Random
            val choice = random.nextInt(2)
            if (choice == 0) {
               val (bb, ff) = is_bhm_satisfies(left())
               if (bb) {
                  (bb, ff)
               } else {
                  val (bbb, fff) = is_bhm_satisfies(right())
                  if (bbb) {
                  (bbb, fff)
                  } else {
                  (false, f)
                  }
               }
            } else {
               val (bbb, fff) = is_bhm_satisfies(right())
               if (bbb) {
                  (bbb, fff)
               } else {
                  val (bb, ff) = is_bhm_satisfies(left())
                  if (bb) {
                  (bb, ff)
                  } else {
                  (false, f)
                  }
               }
            }
            }
            case _ => (false, f)
      }
      }
      }

   def tell(si:SI_Term):Boolean = synchronized {
      if (theStore.contains(si)) 
             { theStore(si) = theStore(si) + 1 }
      else
             { theStore = theStore ++ Map(si -> 1) }
      true
   }
 


   def test_tell(si:SI_Term):Boolean = true

   def ask(si:SI_Term):Boolean = synchronized {   
      if ( theStore.contains(si) ) 
             if (theStore(si) >= 1) { true }
             else { false }
      else false
   }

  

   def test_ask(si:SI_Term):Boolean = synchronized { 
      if (  theStore.contains(si) ) 
             if (theStore(si) >= 1) { true }
             else { false }
      else false
   }

   def get(si:SI_Term):Boolean = synchronized {   
      if ( theStore.contains(si) ) 
             if (theStore(si) >= 1) 
               { theStore(si) = theStore(si) - 1
	         if (theStore(si) == 0) { theStore = theStore - si }
                 true 
               }
             else { false }
      else false
   }



   def test_get(si:SI_Term):Boolean = test_ask(si)

   def nask(si:SI_Term):Boolean = synchronized {    
      if ( theStore.contains(si) ) 
             if (theStore(si) >= 1) { false }
             else { true }
      else 
             { true }

   }

   def test_nask(si:SI_Term):Boolean = synchronized {    
      if ( theStore.contains(si) ) 
             if (theStore(si) >= 1) { false }
             else { true }
      else 
             { true }
   }

 
   def tellf(si: SI_Term, f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
   // Check if the formula is satisfied by the store
      if (tell(si)) {
         val (satisfied, ff) = is_bhm_satisfies(f)
         if (satisfied) {
               (true, ff)
         } else {
         get(si)
         (false, f)
      }
   } else {
      (false, f)
   }
   }

   def getf(si: SI_Term, f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
   // Check if the formula is satisfied by the store
      if (get(si)) {
          val (satisfied, ff) = is_bhm_satisfies(f)
          if (satisfied) {
            (true, ff)
      } else {
         tell(si)
         (false, f)
      }
   } else {
      (false, f)
   }
   }

   def askf(si: SI_Term, f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
   // Check if the formula is satisfied by the store
      if (ask(si)) {
         val (satisfied, ff) = is_bhm_satisfies(f)
         if (satisfied) {
            (true, ff)
         } else {
            (false, f)
         }
      } else {
         (false, f)
      }
   }

   def naskf(si: SI_Term, f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
   // Check if the formula is satisfied by the store
      if (nask(si)) {
      val (satisfied,ff) = is_bhm_satisfies(f)
         if (satisfied) {
            (true, ff)
         } else {
            (false, f)
         }
      } else {
         (false, f)
      }
   }



   def print_store {
      for ((t,d) <- theStore) 
         println ( t.bsc_toString + "(" + theStore(t) + ")" )
   }

   def clear_store:Boolean = synchronized {
      theStore = Map[SI_Term,Int]()
      true
   }

}

object BB extends BSC_Store {

}