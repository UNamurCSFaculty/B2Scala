/***********************************************************************

 Copyright (4) 2023 Jean-Marie Jacquet and the CoordiNam Lab members
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


package bscala.bsc_agent

import bscala.bsc_data._


/** Objects providing methods to create primitives as agents
 */

object tell {
    def apply(siterm: SI_Term) = TellAgent(siterm)
}

object ask {
    def apply(siterm: SI_Term) = AskAgent(siterm)
}

object nask {
    def apply(siterm: SI_Term) = NaskAgent(siterm)
}

object get {
    def apply(siterm: SI_Term) = GetAgent(siterm)
}


case class TellAgent(si:SI_Term) extends BSC_Agent {
   override def bsc_toString: String = {
      "tell(" + si.bsc_toString + ")" }
}

case class AskAgent(si:SI_Term) extends BSC_Agent {
   override def bsc_toString: String = {
      "ask(" + si.bsc_toString + ")" }
}

case class NaskAgent(si:SI_Term) extends BSC_Agent {
   override def bsc_toString: String = {
      "nask(" + si.bsc_toString + ")" }
}

case class GetAgent(si:SI_Term) extends BSC_Agent {
   override def bsc_toString: String = {
      "get(" + si.bsc_toString + ")" }
}


