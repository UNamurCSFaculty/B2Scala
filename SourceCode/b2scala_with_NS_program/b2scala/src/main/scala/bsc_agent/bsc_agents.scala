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


package bscala.bsc_agent

import bscala.bsc_data._
import scala.language.postfixOps

trait BSC_Agent { this: BSC_Agent =>

	def *(other: => BSC_Agent) = ConcatenationAgent( () => this, other _)
   def ||(other: => BSC_Agent) = ParallelAgent( () => this, other _)
  	def +(other: => BSC_Agent) = ChoiceAgent( () => this, other _)
	def **(other: () => BSC_Agent) = ConcatenationAgent( () => this, other)
   def bsc_toString: String = { " " }

}


case class EmptyAgent() extends BSC_Agent

case class CalledAgent(fct_ag: () => BSC_Agent) extends BSC_Agent {
   override def bsc_toString: String = {
      "() => " + fct_ag().bsc_toString }
}

case class ConcatenationAgent(left: () => BSC_Agent, right: () => BSC_Agent) extends BSC_Agent {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ] ; [ ... ]" }
}

case class ParallelAgent(left: () => BSC_Agent, right: () => BSC_Agent) extends BSC_Agent {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ]" + " || " + "[ " + "() => " + right().bsc_toString + " ]" }
}

case class ChoiceAgent(left: () => BSC_Agent, right: () => BSC_Agent) extends BSC_Agent {
   override def bsc_toString: String = {
      "[ " + "() => " + left().bsc_toString + " ]" + " + " + "[ " + "() => " + right().bsc_toString + " ]" }
}

object Agent {
	def apply(agent: BSC_Agent) = CalledAgent(() => agent)
}

object Comm {
   def apply(mess: String) = CommAgent(mess)
}

case class CommAgent(mess: String) extends BSC_Agent {
   override def bsc_toString: String = { mess }
}



object GSum {

   def bsc_list_to_choice(l: List[BSC_Agent]): BSC_Agent = {
      l match {
         case Nil => EmptyAgent()
         case x::Nil => x
         case y => {
            val half_length = (l.length)/2
            val first_half = y.take(half_length)
            val second_half = y.drop(half_length)
            ChoiceAgent( () => bsc_list_to_choice(first_half),
                        () => bsc_list_to_choice(second_half) )
         }
      }
   }
   
   def apply(l:List[SI_Term], f: SI_Term => BSC_Agent) = {
      val list_of_choices =
         for { si <- l } yield f(si)
      bsc_list_to_choice(list_of_choices)     
   }

}