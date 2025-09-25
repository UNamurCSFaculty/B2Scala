package bscala.bsc_program

import bscala.bsc_data._
import bscala.bsc_agent._
import bscala.bsc_runner._
import bscala.bsc_settings._
import bscala.bsc_formula._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

import bscala.bsc_widget._
import scalafx.scene.paint.Color
import bscala.bsc_agent.Agent.set_txt_widget


object BSC_modelling_Hancke_Kuhn_protocol_UNbounded_simplified_Annimated extends JFXApp  {

  val scene = new BSC_Scene(1000, 800, "file:project/resources/background.png")

  val VerifierImg = img_Widget("Verifier_img", 100, 100, "file:project/resources/verifier.png")
  val ProverImg   = img_Widget("Prover_img", 700, 100, "file:project/resources/prover.png")
  val A1Img       = img_Widget("A1_img", 700, 500, "file:project/resources/mallory.png")
  val A2Img       = img_Widget("A2_img", 100, 500, "file:project/resources/mallory.png")

  val VerifierLabel = txt_Widget("Verifier_label", 125, 250, "Verifier")
  val ProverLabel   = txt_Widget("Prover_label", 730, 250, "Prover")
  val A1Label       = txt_Widget("A1_label", 125, 650, "Attacker1")
  val A2Label       = txt_Widget("A2_label", 725, 650, "Attacker2")


  val StoreBoxHK   = rect_Widget("Store_box_HK", 300, 300, 300, 200)
  val StoreLabelHK = txt_Widget("Store_label_HK", 390, 270, "The Store")

  val cWidget  = txt_Widget("c_msg", 150, 180, "challenge c"); cWidget.hide()
  val rWidget  = txt_Widget("r_msg", 700, 250, "response r"); rWidget.hide()

  val allHKWidgets = Seq(
    VerifierImg, ProverImg, A1Img, A2Img,
    VerifierLabel, ProverLabel, A1Label, A2Label,
    StoreBoxHK, StoreLabelHK,
    cWidget, rWidget
  )
  scene.addWidgets(allHKWidgets)
  scene.launch

  /* ---------------- Terms ---------------- */
  val c  = Token("c") // challenge
  val r  = Token("r") // response

  val V  = Token("Verifier")
  val P  = Token("Prover")
  val A1 = Token("Attacker1")
  val A2 = Token("Attacker2")

  case class prover_site(x: SI_Term)    extends SI_Term
  case class verifier_site(x: SI_Term)  extends SI_Term
  case class attacker_site(x: SI_Term)  extends SI_Term

 

  val Verifier = Agent {
    delay(1) *
    ( tell(verifier_site(c))
        ** set_txt_widget(cWidget, c)
        ** show(cWidget)
        ** move_to(cWidget, 300, 350)
    ) *
    ( get(verifier_site(r))
        ** set_txt_widget(rWidget, r)
        ** show(rWidget)
        ** move_to(rWidget, 150, 150)
    )
  }

  val Prover = Agent {
    ( get(prover_site(c))
        ** show(cWidget)
        ** move_to(cWidget, 700, 200)
    ) *
    delay(1) *
    ( tell(prover_site(r))
        ** set_txt_widget(rWidget, r)
        ** show(rWidget)
        ** move_to(rWidget, 300, 370)
    )
  }

  val Attacker1 = Agent {
    ( get(verifier_site(c))
        ** show(cWidget)
        ** move_to(cWidget, 150, 500)
    ) *
    delay(1) *
    ( tell(attacker_site(c))
        ** show(cWidget)
        ** move_to(cWidget, 300, 350)
    ) *
    ( get(attacker_site(r))
        ** show(rWidget)
        ** move_to(rWidget, 150, 500)
    ) *
    delay(1) *
    ( tell(verifier_site(r))
        ** show(rWidget)
        ** move_to(rWidget, 300, 370)
    )
  }

  val Attacker2 = Agent {
    ( get(attacker_site(c))
        ** show(cWidget)
        ** move_to(cWidget, 700, 500)
    ) *
    delay(1) *
    ( tell(prover_site(c))
        ** show(cWidget)
        ** move_to(cWidget, 300, 350)
    ) *
    ( get(prover_site(r))
        ** show(rWidget)
        ** move_to(rWidget, 700, 500)
    ) *
    delay(1) *
    ( tell(attacker_site(r))
        ** show(rWidget)
        ** move_to(rWidget, 300, 370)
    )
  }

  val Protocol = Verifier || Prover || Attacker1 || Attacker2

  /* ---------------- LA_BHM formula ---------------- */
  val verifier_challenge = la_bf(5, bf(verifier_site(c)))
  val attack_underway    = la_bf(5, bf(attacker_site(c)))
  val attack_on_prover   = la_bf(5, bf(prover_site(c)))
  val prover_resp        = la_bf(5, bf(prover_site(r)))
  val attacker_resp      = la_bf(5, bf(attacker_site(r)))
  val verifier_resp      = la_bf(5, bf(verifier_site(r)))
  val message_proved     = la_bf(5, not(bf(verifier_site(r))))

  val Fraud = la_bHM {
    verifier_challenge *
    attack_underway *
    attack_on_prover *
    prover_resp *
    attacker_resp *
    verifier_resp *
    message_proved
  }

  /* ---------------- Runner (LA_BHM + widgets) ---------------- */
  val runner = new BSC_Runner_LA_BHM_Widget(scene)
    new Thread(() => {
        Thread.sleep(500)
        runner.execute(Protocol,Fraud) 
    }).start()
    

}
