package bscala.bsc_program

import bscala.bsc_data._
import bscala.bsc_agent._
import bscala.bsc_runner._
import bscala.bsc_settings._
import bscala.bsc_formula._

object BSC_modelling extends App {


    /////////////////////////////  DATA  /////////////////////////////

    val na = Token("Alice_nounce")
    val nb = Token("Bob_nounce")
    val nm = Token("Mallory_nounce")

    val pka = Token("Alice_public_key")
    val pkb = Token("Bob_public_key")
    val pkm = Token("Mallory_public_key")

    val alice = Token("Alice_as_agent")
    val bob = Token("Bob_as_agent")
    val mallory = Token("Mallory_as_intruder")

    def public_key(a: SI_Term) = {
      if (a.isInstanceOf[BSC_Token]) {
        a match {
          case BSC_Token(alice.name) => pka
          case BSC_Token(bob.name) => pkb
          case _ => pkm
        }
      } else pkm
    }

    def nounce(a: SI_Term) = {
      if (a.isInstanceOf[BSC_Token]) {
        a match {
          case BSC_Token(alice.name) => na
          case BSC_Token(bob.name) => nb
          case _ => nm
        }
      } else nm
    }

    case class encrypt_i(vNonce: SI_Term, vAg: SI_Term, vKey: SI_Term) extends SI_Term
    case class encrypt_ii(vNonce: SI_Term, wNonce: SI_Term, vKey: SI_Term) extends SI_Term
    case class encrypt_iii(vNonce: SI_Term, vKey: SI_Term) extends SI_Term
    case class message(agS: SI_Term, agR: SI_Term, encM: SI_Term) extends SI_Term

    case class a_running(vAg: SI_Term) extends SI_Term
    case class b_running(vAg: SI_Term) extends SI_Term
    case class a_commit(vAg: SI_Term) extends SI_Term
    case class b_commit(vAg: SI_Term) extends SI_Term

    /////////////////////////////  AGENTS  /////////////////////////////

    val Alice = Agent {
        GSum( List(bob,mallory),  Y => {
	       tell(a_running(Y)) *
               tell( message(alice, Y, encrypt_i(na, alice, public_key(Y))) ) *
               GSum( List(na,nb,nm), WNonce => {
                   get( message(Y, alice, encrypt_ii(na,WNonce,pka)) ) *
                   tell( message(alice,Y,encrypt_iii(WNonce,public_key(Y))) ) *
		               tell( a_commit(Y) )
	       })
        })
    }

    val Bob = Agent {
        GSum( List(alice,mallory), Y => { 
               tell(b_running(Y)) *
	       GSum( List(alice,mallory),  VAg => {
	            get( message(Y,bob,encrypt_i(na,VAg,pkb)) ) *
                    tell( message(bob,Y,encrypt_ii(na,nb,public_key(Y))) ) *
                    get( message(Y,bob,encrypt_iii(nb,pkb)) ) *
                    tell( b_commit(VAg) )
               })
  	    })
    }


    lazy val Mallory:BSC_Agent = Agent {

            ( GSum( List(na,nb,nm),  VNonce => {
              GSum( List(alice,bob),  VAg => {
              GSum( List(pka,pkb,pkm),  VPK => {
                      get( message(alice,mallory,encrypt_i(VNonce,VAg,VPK)) ) *
                      ( if ( VPK == pkm) {
                      tell( message(mallory,bob,encrypt_i(VNonce,VAg,pkb)) )
                      } else {
                      tell( message(mallory,bob,encrypt_i(VNonce,VAg,VPK)) )		 
                } ) *
                  Mallory
                })
                      })
              }) ) +
            ( GSum( List(na,nb,nm),  VNonce => {
              GSum( List(na,nb,nm),  WNonce => {
              GSum( List(pka,pkb,pkm),  VPK => {
                      get( message(bob,mallory,encrypt_ii(VNonce,WNonce,VPK)) ) *
                      ( if ( VPK == pkm) {
                    tell( message(mallory,alice,encrypt_ii(VNonce,WNonce,pka)) )
                        } else {
                    tell( message(mallory,alice,encrypt_ii(VNonce,WNonce,VPK)) )		 
              } ) *
                Mallory
              })
                      })
              }) ) +
            ( GSum( List(na,nb,nm),  VNonce => {
                GSum( List(pka,pkb,pkm),  VPK => {
                  get( message(alice,mallory,encrypt_iii(VNonce,VPK)) ) *
                  ( if ( VPK == pkm) {
                tell( message(mallory,bob,encrypt_iii(VNonce,pkb)) )
                    } else {
                tell( message(mallory,bob,encrypt_iii(VNonce,VPK)) )		 
              } ) 
              //* Mallory
                })
              }) )

    }


    /////////////////////////////   bHM FORMULA  /////////////////////////////

    val inproper_init = not(bf(a_running(bob)) or bf(b_running(alice)) or bf(b_commit(alice)))
    val end_session = bf(b_commit(alice))

    val F: BHM_Formula = bHM {
      (inproper_init * F) + end_session
    }

    ///////////////////////  EXECUTING THE PROTOCOL  /////////////////////////

    val Protocol = Agent {
      Alice || Bob || Mallory
    }

    val bsc_executor = new BSC_Runner
    bsc_executor.execute(Protocol, F)

}