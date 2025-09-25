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

object BSC_modelling_NS_BHM_GUI extends JFXApp {
// ----------------------------- Scene & Widgets -----------------------------
   val sceneRoot = new BSC_Scene(1000, 800, "file:project/resources/background.png")

  val AliceImg     = img_Widget("Alice_img", 200, 100, "file:project/resources/alice.png")
  val BobImg       = img_Widget("Bob_img", 1450, 100, "file:project/resources/bob.png")
  val MalloryImg   = img_Widget("Mallory_img", 800, 700, "file:project/resources/mallory.png")

  val AliceLabel   = txt_Widget("Alice_label", 225, 250, "Alice")
  val BobLabel     = txt_Widget("Bob_label", 1475, 250, "Bob")
  val MalloryLabel = txt_Widget("Mallory_label", 825, 850, "Mallory")

  val StoreBox   = rect_Widget("Store_box", 400, 320, 900, 300)
  val StoreLabel = txt_Widget("Store_label", 800, 300, "The Store")

  val msg1_widget = txt_Widget("msg1", 250, 150, "msg1"); msg1_widget.hide()
  val msg2_widget = txt_Widget("msg2", 1450, 200, "msg2"); msg2_widget.hide()
  val msg3_widget = txt_Widget("msg3", 250, 180, "msg3"); msg3_widget.hide()

  val msg_m1   = txt_Widget("msg_m1", 900, 700, "Mallory intercepts msg1"); msg_m1.hide()
  val msg_m2   = txt_Widget("msg_m2", 900, 730, "Mallory intercepts msg2"); msg_m2.hide()
  val msg_fake = txt_Widget("msg_fake", 1000, 760, "FAKE msg from Mallory"); msg_fake.hide()

  val allWidgets = Seq(
    AliceImg, AliceLabel,
    BobImg, BobLabel,
    MalloryImg, MalloryLabel,
    StoreBox, StoreLabel,
    msg1_widget, msg2_widget, msg3_widget,
    msg_m1, msg_m2, msg_fake
  )

  sceneRoot.addWidgets(allWidgets)
  sceneRoot.launch


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


    // Pretty output (kept local)
    case class TextTerm(s: String) extends SI_Term { override def toString: String = s }
    def realMsg(msg: message): SI_Term = msg match {
        case message(_, _, e @ encrypt_i(_, _, _))  => e
        case message(_, _, e @ encrypt_ii(_, _, _)) => e
        case message(_, _, e @ encrypt_iii(_, _))   => e
        case _ => throw new IllegalArgumentException("no encrypt term")
    }
    def pp(t: SI_Term): String = t match {
        case BSC_Token(n)      => n
        case encrypt_i(a,b,k)  => s"encrypt(${pp(a)}, ${pp(b)}, ${pp(k)})"
        case encrypt_ii(a,b,k) => s"encrypt(${pp(a)}, ${pp(b)}, ${pp(k)})"
        case encrypt_iii(a,k)  => s"encrypt(${pp(a)}, ${pp(k)})"
        case other             => other.toString
    }
    def realMsgText(msg: message): String = pp(realMsg(msg))

    /////////////////////////////  AGENTS  /////////////////////////////

// ------------------------------ Alice (logic + UI) -------------------------
    val Alice = Agent {
    GSum(List(bob, mallory), Y => {
    val m1 = message(alice, Y, encrypt_i(na, alice, public_key(Y)))
    tell(a_running(Y)) *
    ( tell(m1) ** set_txt_widget(msg1_widget, TextTerm(realMsgText(m1))) ** show(msg1_widget) ** move_to(msg1_widget, 420, 400) ) *
    GSum(List(na, nb, nm), WNonce => {
    val m2exp = message(Y, alice, encrypt_ii(na, WNonce, pka))
    ( get(m2exp) ** move_to(msg_m2, 250, 200) ) *
    {
    val m3 = message(alice, Y, encrypt_iii(WNonce, public_key(Y)))
    ( tell(m3) ** set_txt_widget(msg3_widget, TextTerm(realMsgText(m3))) ** show(msg3_widget) ** move_to(msg3_widget, 420, 400) )*
    tell(a_commit(Y))
    }
    })
    })
    }


// ------------------------------- Bob (logic + UI) --------------------------
    val Bob = Agent {
    GSum(List(alice, mallory), Y => {
    tell(b_running(Y)) *
    GSum(List(alice, mallory), VAg => {
    val exp1 = message(Y, bob, encrypt_i(na, VAg, pkb))
    ( get(exp1) ** move_to(msg_m1, 1450, 150) ) *
    {
    val m2 = message(bob, Y, encrypt_ii(na, nb, public_key(Y)))
    ( tell(m2) ** set_txt_widget(msg2_widget, TextTerm(realMsgText(m2))) ** show(msg2_widget) ** move_to(msg2_widget, 420, 400)  ) *
    {
    val exp3 = message(Y, bob, encrypt_iii(nb, pkb))
    ( get(exp3) ** move_to(msg_fake, 1450, 300) ) *
    tell(b_commit(VAg))
    }
    }
    })
    })
    }


// ------------------------------ Mallory (logic + UI) -----------------------
    lazy val Mallory: BSC_Agent = Agent {
    // Branch 1: intercept msg1
    (
      GSum(List(na, nb, nm), VNonce => {
        GSum(List(alice, bob), VAg => {
          val inc = message(alice, mallory, encrypt_i(VNonce, VAg, pkm))
          val forward = message(mallory, bob, encrypt_i(VNonce, VAg, pkb))
          ( get(inc) ** move_to(msg1_widget, 900, 700) ) *
          ( tell(forward) ** set_txt_widget(msg_m1, TextTerm(realMsgText(forward))) ** show(msg_m1) ** move_to(msg_m1, 420, 400) ) *
          Mallory
        })
      })
    ) +
    // Branch 2: intercept msg2
    (
      GSum(List(na, nb, nm), VNonce => {
        GSum(List(na, nb, nm), WNonce => {
          val inc = message(bob, mallory, encrypt_ii(VNonce, WNonce, pkm))
          val forward = message(mallory, alice, encrypt_ii(VNonce, WNonce, pka))
          ( get(inc) ** move_to(msg2_widget, 900, 730) ) *
          ( tell(forward) ** set_txt_widget(msg_m2, TextTerm(realMsgText(forward))) ** show(msg_m2) ** move_to(msg_m2, 420, 400) ) *
          Mallory
        })
      })
    ) +
    // Branch 3: intercept msg3
    (
      GSum(List(na, nb, nm), VNonce => {
        val inc = message(alice, mallory, encrypt_iii(VNonce, pkm))
        val forward = message(mallory, bob, encrypt_iii(VNonce, pkb))
        ( get(inc) ** move_to(msg3_widget, 1000, 760) ) *
        ( tell(forward) ** set_txt_widget(msg_fake, TextTerm(realMsgText(forward))) ** show(msg_fake) ** move_to(msg_fake, 420, 400) ) *
        Mallory
      })
    )
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

    
 val exec = new BSC_Runner_BHM_Widget(sceneRoot)

  new Thread(() => {
    Thread.sleep(500)
    exec.execute(Protocol, F)
  }).start()
}
