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


package bscala.bsc_blackboard


import bscala.bsc_data._
import bscala.bsc_formula._
import scala.collection.mutable.Map
import bscala.bscala._



class BSC_Store {
  
   var theStore = Map[SI_Term, List[TDuration]]()

   def nb_occ(si: SI_Term) = synchronized {
      if (theStore.contains(si)) { theStore(si).length }
      else { 0 }
   }

   def storeContents = synchronized {
      theStore
   }

   def copy_store(mybb: BSC_Store) = synchronized {
      theStore = mybb.storeContents
   }
   
   def set_store(s: scala.collection.immutable.Map[SI_Term, List[TDuration]]) = synchronized {
      theStore = scala.collection.mutable.Map() ++ s
   }

   def reset_store = synchronized {
      theStore = Map[SI_Term, List[TDuration]]()
   }
   
   def is_bf_satisfied(f:BSC_Formula) : Boolean = synchronized {
   // Check if the formula is satisfied by the store
   f match {
      case Bf_true_formula() => true
      case Bf_false_formula() => false
      case Bf_formula(si) => ask(si)
      case Neg_bf_formula(f) => !is_bf_satisfied(f)
      case And_bf_formula(left, right) => is_bf_satisfied(left) && is_bf_satisfied(right)
      case Or_bf_formula(left, right) => is_bf_satisfied(left) || is_bf_satisfied(right)
      case _ => (false)
   }
   
   }


   def is_bhm_satisfied(f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
      // Check if the bHM formula is satisfied by the store
      if (f.isInstanceOf[BSC_Formula]){
         if (is_bf_satisfied(f.asInstanceOf[BSC_Formula])){
               (true,EmptyBHM())
            } else {
               (false,f)
            }
      } else {
         f match {
            case EmptyBHM() => (false, f)

            case CalledFormula(fct_ag) => is_bhm_satisfied((fct_ag()))

            case ConcatenationFormula(left, right) => {
            val (leftSatisfied, ff) = is_bhm_satisfied(left())
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
               val (bb, ff) = is_bhm_satisfied(left())
               if (bb) {
                  (bb, ff)
               } else {
                  val (bbb, fff) = is_bhm_satisfied(right())
                  if (bbb) {
                  (bbb, fff)
                  } else {
                  (false, f)
                  }
               }
            } else {
               val (bbb, fff) = is_bhm_satisfied(right())
               if (bbb) {
                  (bbb, fff)
               } else {
                  val (bb, ff) = is_bhm_satisfied(left())
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

   def is_la_bhm_satisfied(f: LA_BHM_Formula): (Boolean, LA_BHM_Formula) = synchronized {
      // Check if the la_bHM formula is satisfied by the store
      if (f.isInstanceOf[LA_BSC_Formula]){
         val real_bsc = (f.asInstanceOf[LA_BSC_Formula]).without_la
//         if (is_bf_satisfied(real_bsc.asInstanceOf[BSC_Formula])){
         if (is_bf_satisfied(real_bsc)){
               (true,LaEmptyBHM())
            } else {
               (false,f)
            }
      } else {
         f match {
            case LaEmptyBHM() => (false, f)

            case LaCalledFormula(fct_ag) => is_la_bhm_satisfied((fct_ag()))

            case LaConcatenationFormula(left, right) => {
            val (leftSatisfied, ff) = is_la_bhm_satisfied(left())
            if (leftSatisfied) {
               if (ff == LaEmptyBHM()) {
                  (true, right())
               } else {
                  (true, LaConcatenationFormula(() => ff, right))
               }
            } else {
               (false, f)
            }
            }

            case LaChoiceFormula(left, right) => {
            val random = new scala.util.Random
            val choice = random.nextInt(2)
            if (choice == 0) {
               val (bb, ff) = is_la_bhm_satisfied(left())
               if (bb) {
                  (bb, ff)
               } else {
                  val (bbb, fff) = is_la_bhm_satisfied(right())
                  if (bbb) {
                  (bbb, fff)
                  } else {
                  (false, f)
                  }
               }
            } else {
               val (bbb, fff) = is_la_bhm_satisfied(right())
               if (bbb) {
                  (bbb, fff)
               } else {
                  val (bb, ff) = is_la_bhm_satisfied(left())
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



   def tell(si: SI_Term, d: TDuration): Boolean = synchronized {
      if (theStore.contains(si)) {
         theStore(si) = theStore(si) :+ d
      } else {
         theStore = theStore ++ Map(si -> List(d)) 
      }
      true
   }

   def test_tell(si:SI_Term, d: TDuration):Boolean = true

   def ask(si:SI_Term):Boolean = synchronized {   
      if ( theStore.contains(si) ) 
             if (theStore(si).length >= 1) { true }
             else { false }
      else false
   }

   def test_ask(si:SI_Term):Boolean = synchronized { 
      if (  theStore.contains(si) ) 
             if (theStore(si).length >= 1) { true }
             else { false }
      else false
   }

   def get(si:SI_Term):Boolean = synchronized {   
      if ( theStore.contains(si) ) 
             if (theStore(si).length >= 1) 
               { theStore(si) = theStore(si).tail
	         if (theStore(si).length == 0) { theStore = theStore - si }
                 true 
               }
             else { false }
      else false
   }

   def test_get(si:SI_Term):Boolean = test_ask(si)

   def nask(si:SI_Term):Boolean = synchronized {    
      if ( theStore.contains(si) ) 
             if (theStore(si).length >= 1) { false }
             else { true }
      else 
             { true }

   }

   def test_nask(si:SI_Term):Boolean = synchronized {    
      if ( theStore.contains(si) ) 
             if (theStore(si).length >= 1) { false }
             else { true }
      else 
             { true }
   }

    def tellf(si: SI_Term, d: TDuration, f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {   
   // Tell and check if the formula is satisfied by the store. If so give the resulting formula
      if (tell(si,d)) {
         val (satisfied, ff) = is_bhm_satisfied(f)
         if (satisfied) {
	       (true, ff)
         } else {
	       (false, f)
         }
      } else {
         (false, f)
   }
   }

   def getf(si: SI_Term, f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
   // Get and then check if the formula is satisfied by the store
      if (get(si)) {
          val (satisfied, ff) = is_bhm_satisfied(f)
          if (satisfied) {
            (true, ff)
          } else {
            (false, f)
          }
      } else {
         (false, f)
      }
   }


   def askf(si: SI_Term, f: BHM_Formula): (Boolean, BHM_Formula) = synchronized {
   // Ask and then check if the formula is satisfied by the store
      if (ask(si)) {
         val (satisfied, ff) = is_bhm_satisfied(f)
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
   // Nask and then check if the formula is satisfied by the store
      if (nask(si)) {
      val (satisfied,ff) = is_bhm_satisfied(f)
         if (satisfied) {
            (true, ff)
         } else {
            (false, f)
         }
      } else {
         (false, f)
      }
   }


   def clear_store(): Boolean = synchronized {
      // theStore.clear()
      theStore = Map[SI_Term, List[TDuration]]()
      true
   }

   def print_store(): Unit = {
      for ((t, entries) <- theStore) {
         val entryStr = entries.map { case duration => s"$duration" }.mkString(", ")
         println(s"${t.bsc_toString} -> [$entryStr]")
      }
   }


   def increase_time(): Boolean = synchronized {
      var hasChanges = false
      var newLD = List[TDuration]()

      for ((si, ld) <- theStore) {
         newLD = List[TDuration]()
         for (d <- ld) {
            if (d<infiniteDuration) { hasChanges = true }
	    if (d>1) {
	      if (d<infiniteDuration) {
	         newLD = newLD :+ (d-1)
	      } else {
	         newLD = newLD :+ infiniteDuration
	      }
	    }
         }
         if (newLD.isEmpty) { 
            theStore -= si
         } else {
            theStore(si) = newLD
         }
      }
      
      hasChanges
   }

}

object BB extends BSC_Store {

}
