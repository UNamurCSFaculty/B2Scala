package bscala.bsc_program

import bscala.bsc_data._
import bscala.bsc_agent._
import bscala.bsc_runner._
import bscala.bsc_settings._
import bscala.bsc_formula._

object BSC_modelling_Hancke_Kuhn_protocol_BOUNDED_simplified extends App {

val c  = Token("c")     // challenge
val r  = Token("r")     // response

val V  = Token("Verifier")
val P  = Token("Prover")
val A1 = Token("Attacker1")
val A2 = Token("Attacker2")

case class prover_site(x: SI_Term) extends SI_Term
case class verifier_site(x: SI_Term) extends SI_Term
case class attacker_site(x: SI_Term) extends SI_Term

val Verifier = Agent {
  delay(1) * 
  ( tell(verifier_site(c)) ** Comm("Verifier has told verifier_site(c)") ) *
  ( gett(1,verifier_site(r)) ** Comm("Verifier has got verifier_site(r)") ) 
}

val Prover = Agent {
  ( get(prover_site(c)) ** Comm("Prover has got prover_site(c)") ) *
  delay(1) * 
  ( tell(prover_site(r)) ** Comm("Prover has told prover_site(r)") )
}

val Attacker1 = Agent {
  ( get(verifier_site(c)) ** Comm("Attacker 1 has got verifier_site(c)") ) *
  delay(1) * 
  ( tell(attacker_site(c)) ** Comm("Attacker 1 has told attacker_site(c)") ) *
  ( get(attacker_site(r)) ** Comm("Attacker 1 has got attacher_site(r)") ) *
  delay(1) * 
  ( tell(verifier_site(r)) ** Comm("Attacker 1 has told verifier_site(r)") )
}

val Attacker2 = Agent {
  ( get(attacker_site(c)) ** Comm("Attacker 2 has got attacker_site(c)") ) *
  delay(1) * 
  ( tell(prover_site(c)) ** Comm("Attacker 2 has told prover_site(c)") ) *
  ( get(prover_site(r)) ** Comm("Attacker 2 has got prover_site(r)") ) *
  delay(1) * 
  ( tell(attacker_site(r)) ** Comm("Attacker 2 has told attacker_site(r)") )
}

val Protocol = Verifier || Prover || Attacker1 || Attacker2 

val verifier_challenge = la_bf(5, bf(verifier_site(c)))
val attack_underway = la_bf(5, bf(attacker_site(c)))
val attack_on_prover = la_bf(5, bf(prover_site(c)))
val prover_resp = la_bf(5,bf(prover_site(r)))
val attacker_resp = la_bf(5, bf(attacker_site(r)))
val verifier_resp = la_bf(5, bf(verifier_site(r)))
val message_proved = la_bf(5, not(bf(verifier_site(r))))


val Fraud = la_bHM { verifier_challenge *
                     attack_underway *
                     attack_on_prover *
		     prover_resp *
		     attacker_resp *
		     verifier_resp *
		     message_proved }



val runner = new BSC_Runner_LA_BHM
runner.execute(Protocol,Fraud)


}


