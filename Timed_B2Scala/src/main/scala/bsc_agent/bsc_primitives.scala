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

package bscala.bsc_agent

import bscala.bsc_data._
import bscala.bscala._


object tell {
    def apply(siterm: SI_Term) = TelltAgent(siterm, infiniteDuration)
}

object ask {
    def apply(siterm: SI_Term) = AsktAgent(siterm, infiniteDuration)
}

object nask {
    def apply(siterm: SI_Term) = NasktAgent(siterm, infiniteDuration)
}

object get {
    def apply(siterm: SI_Term) = GettAgent(siterm, infiniteDuration)
}

object tellt {
    def apply(d: TDuration, siterm: SI_Term) = TelltAgent(siterm, d)
}

object askt {
    def apply(d: TDuration, siterm: SI_Term) = AsktAgent(siterm, d)
}

object naskt {
    def apply(d: TDuration, siterm: SI_Term) = NasktAgent(siterm, d)
}

object gett {
    def apply(d: TDuration, siterm: SI_Term) = GettAgent(siterm, d)
}

object delay {
    def apply(d: TDuration) = DelayAgent(d) 

}


case class TelltAgent(si: SI_Term, duration: TDuration) extends BSC_Agent {
    override def bsc_toString: String = {
        "tellt(" + si.bsc_toString + "," + duration + ")"
    }

    override def fbsc_toString: String = {
        "tellt(" + si.bsc_toString + "," + duration + ")"
    }

    override def increase_time: BSC_Agent = {
        this
    }

    override def is_deadlock_def: Boolean = false
}


case class AsktAgent(si: SI_Term, duration: TDuration) extends BSC_Agent {
    override def bsc_toString: String = {
        "askt(" + si.bsc_toString + "," + duration + ")"
    }

    override def fbsc_toString: String = {
        "askt(" + si.bsc_toString + "," + duration + ")"
    }

    override def increase_time: AsktAgent = {
       if (duration == infiniteDuration) { this } else { 
        if (duration > 0) { AsktAgent(si, duration - 1) } else { this }
       }
    }

    override def is_deadlock_def: Boolean = {
       if (duration == 0) { true } else { false }
    }
}


case class NasktAgent(si: SI_Term, duration: TDuration) extends BSC_Agent {
    override def bsc_toString: String = {
        "naskt(" + si.bsc_toString + "," + duration + ")"
    }

    override def fbsc_toString: String = {
        "naskt(" + si.bsc_toString + "," + duration + ")"
    }

    override def increase_time: NasktAgent = {
       if (duration == infiniteDuration) { this } else {     
        if (duration > 0) { NasktAgent(si, duration - 1) } else { this }
       }
    }

    override def is_deadlock_def: Boolean = {
       if (duration == 0) { true } else { false }
    }

}


case class GettAgent(si: SI_Term, duration: TDuration) extends BSC_Agent {
    override def bsc_toString: String = {
        "gett(" + si.bsc_toString + "," + duration + ")"
    }

    override def fbsc_toString: String = {
        "gett(" + si.bsc_toString + "," + duration + ")"
    }

    override def increase_time: GettAgent = {
       if (duration == infiniteDuration) { this } else {         
        if (duration > 0) { GettAgent(si, duration - 1) } else { this }
       }
    }

    override def is_deadlock_def: Boolean = {
       if (duration == 0) { true } else { false }
    }

}


case class DelayAgent(duration: TDuration) extends BSC_Agent {
    override def bsc_toString: String = {
        "delay(" + duration + ")"
    }

    override def fbsc_toString: String = {
        "delay(" + duration + ")"
    }

    override def increase_time: DelayAgent = {
       if (duration == infiniteDuration) { this } else {         
        if (duration > 0) { DelayAgent(duration - 1) } else { this }
       }
    }

    override def is_deadlock_def: Boolean = false
}



