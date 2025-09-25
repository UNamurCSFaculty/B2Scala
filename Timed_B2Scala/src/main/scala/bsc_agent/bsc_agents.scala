/***********************************************************************

 Copyright (c) 2023 Jean-Marie Jacquet and the CoordiNam Lab members
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
import bscala.bsc_widget.{BSC_Widget, txt_Widget}
import bscala.bsc_runner.SetTxt
import bscala.bscala._


trait BSC_Agent { self: BSC_Agent =>

  def *(other: => BSC_Agent): BSC_Agent = ConcatenationAgent(() => this, () => other)
  def ||(other: => BSC_Agent): BSC_Agent = ParallelAgent(() => this, () => other)
  def +(other: => BSC_Agent): BSC_Agent = ChoiceAgent(() => this, () => other)
  def **(other: => BSC_Agent): BSC_Agent = {
     other match {
        case ssa: StrongSeqAgent => {
	   val p = ssa.left
	   val l = ssa.right
	   StrongSeqAgent(() => this, () => p()::l()) }
	case prim => StrongSeqAgent( () => this, () => List(prim) )
     }
  }

  def bsc_toString: String = " "
  def fbsc_toString: String = " "
  def increase_time: BSC_Agent = this
  def is_deadlock_def: Boolean = false
}

case class EmptyAgent() extends BSC_Agent {
  override def increase_time: BSC_Agent = this
  override def is_deadlock_def: Boolean = false
  override def fbsc_toString: String = "Empty"
}

case class CalledAgent(fct_ag: () => BSC_Agent) extends BSC_Agent {
  override def bsc_toString: String = "() => " + fct_ag().bsc_toString
  override def fbsc_toString: String = fct_ag().fbsc_toString
  override def increase_time: BSC_Agent = fct_ag().increase_time
  override def is_deadlock_def: Boolean = fct_ag().is_deadlock_def  
}

case class ConcatenationAgent(left: () => BSC_Agent, right: () => BSC_Agent) extends BSC_Agent {
  override def bsc_toString: String = "[ " + left().bsc_toString + " ] ; [ " + right().bsc_toString + " ]"
  override def fbsc_toString: String = "[ " + left().fbsc_toString + " ... ]"  

  override def increase_time: BSC_Agent = {
      ConcatenationAgent(() => left().increase_time, right)
  }

  override def is_deadlock_def: Boolean = left().is_deadlock_def
}

case class StrongSeqAgent(left: () => BSC_Agent, right: () => List[BSC_Agent]) extends BSC_Agent {
  override def bsc_toString: String = {
     val laux = left() :: right()
     "[ " + (laux.map(x => bsc_toString)).mkString(" ** ") + " ]"
  }

  override def fbsc_toString: String = {
     "{ " + left().fbsc_toString + " *** }"
  }

  override def increase_time: BSC_Agent = {
      StrongSeqAgent(() => left().increase_time, right)
  }

  override def is_deadlock_def: Boolean = left().is_deadlock_def
}

case class ParallelAgent(left: () => BSC_Agent, right: () => BSC_Agent) extends BSC_Agent {
  override def bsc_toString: String = "[ " + left().bsc_toString + " ] || [ " + right().bsc_toString + " ]"

  override def fbsc_toString: String = "[ " + left().fbsc_toString + " ] || [ " + right().fbsc_toString + " ]"

  override def increase_time: BSC_Agent = {
      ParallelAgent(() => left().increase_time, () => right().increase_time) 
  }

  override def is_deadlock_def: Boolean = (left().is_deadlock_def || right().is_deadlock_def)
}

case class ChoiceAgent(left: () => BSC_Agent, right: () => BSC_Agent) extends BSC_Agent {
  override def bsc_toString: String = "[ " + left().bsc_toString + " ] + [ " + right().bsc_toString + " ]"

  override def fbsc_toString: String = "[ " + left().fbsc_toString + " ] + [ " + right().fbsc_toString + " ]"

  override def increase_time: BSC_Agent = {
      ChoiceAgent(() => left().increase_time, () => right().increase_time) 
  }

  override def is_deadlock_def: Boolean = (left().is_deadlock_def || right().is_deadlock_def)
}



  case class BSC_Action(action: () => Unit) extends BSC_Agent {

  override def bsc_toString: String = "Act(...)"
  override def increase_time: BSC_Agent = this

  def run(): Unit = action()
  

  }


  

  object Agent {
    def apply(agent: BSC_Agent): BSC_Agent = CalledAgent(() => agent)

    def Act(f: () => Unit): BSC_Agent = BSC_Action(f)

    def set_txt_widget(widget: txt_Widget, msg: SI_Term): BSC_Agent =
        SetTxt(widget, msg)
      
  }



object Comm {
  def apply(mess: String): CommAgent = CommAgent(mess)
}

case class CommAgent(mess: String) extends BSC_Agent {
  override def bsc_toString: String = mess
  override def fbsc_toString: String = "(COM : " + mess + " )"
  override def increase_time: BSC_Agent = this
  override def is_deadlock_def: Boolean = false  
}

object GSum {

  def bsc_list_to_choice(l: List[BSC_Agent]): BSC_Agent = {
    l match {
      case Nil => EmptyAgent()
      case x :: Nil => x
      case _ =>
        val half_length = l.length / 2
        val first_half = l.take(half_length)
        val second_half = l.drop(half_length)
        ChoiceAgent(
          () => bsc_list_to_choice(first_half),
          () => bsc_list_to_choice(second_half)
        )
    }
  }

  def apply(l: List[SI_Term], f: SI_Term => BSC_Agent): BSC_Agent = {
    val list_of_choices = l.map(f)
    bsc_list_to_choice(list_of_choices)
  }
}
