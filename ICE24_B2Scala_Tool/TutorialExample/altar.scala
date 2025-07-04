package exbscala.my_program

import bscala.bsc_data._
import bscala.bsc_agent._
import bscala.bsc_runner._
import bscala.bsc_settings._
import bscala.bsc_formula._


object BSC_modelling extends App {


    /////////////////////////////  DATA  /////////////////////////////
    
    val manel = Token("Manel")
    val alice = Token("Alice")
    val emma  = Token("Emma")
    val john  = Token("John")
    val tom   = Token("Tom")

    val t1 = Token("table 1")
    val t2 = Token("table 2")
    val t3 = Token("table 3")
    val t4 = Token("table 4")

    val nigiri = Token("Nigiri")
    val maki = Token("Maki")
    val temaki = Token("Temaki")
    val beer = Token("Beer")

    case class order(id:SI_Term,table:SI_Term,dish:SI_Term) extends SI_Term
    case class order_prepared(id:SI_Term,table:SI_Term,dish:SI_Term) extends SI_Term
    case class order_served(id:SI_Term,table:SI_Term,dish:SI_Term) extends SI_Term
    case class last_order(id:SI_Term,table:SI_Term,dish:SI_Term) extends SI_Term    
    case class logged(id:SI_Term) extends SI_Term
    case class client_sitting_at_table(id:SI_Term,t:SI_Term) extends SI_Term

    case class t_order(id:SI_Term) extends SI_Term
    case class t_check(id:SI_Term) extends SI_Term


    /////////////////////////////  CLIENT  /////////////////////////////

    def Client(id:SI_Term) = Agent {
        LogIn(id) * EnjoyAltar(id) * LogOut(id) }

    def LogIn(id:SI_Term) = Agent {
        tell(logged(id)) *
	tell(t_order(id)) * tell(t_order(id)) *
	tell(t_check(id)) * tell(t_check(id)) }
	
    def LogOut(id:SI_Term) = Agent {
        get(logged(id)) }

    def EnjoyAltar(id:SI_Term) = Agent {
          GSum( List(t1,t2,t3,t4), T => 
          ( SitAtTable(id,T) * EnjoyAtTable(id,T) ) ) }

    def SitAtTable(id:SI_Term,t:SI_Term) = Comm {
        "Client " + id.bsc_toString + " is sitting at " + t.bsc_toString }
	
    def EnjoyAtTable(id:SI_Term,t:SI_Term) = Agent {
        MakeFirstOrder(id,t) * HandleOrders(id,t) * PayOrders(id,t) }

    def PayOrders(id:SI_Term,t:SI_Term) = Comm {
        id.bsc_toString + " at " + t.bsc_toString + " just paid " }

    def HandleOrders(id:SI_Term,t:SI_Term): BSC_Agent = {
         Agent {
             ( get(t_order(id)) * MakeOrder(id,t) * HandleOrders(id,t) )  +
             ( get(t_check(id)) * CheckOrder(id,t) * HandleOrders(id,t) ) +
	     ( nask(t_order(id)) )	     
	 }
    }	 

     def MakeFirstOrder(id:SI_Term,t:SI_Term) = Agent {
        get(t_order(id)) * MakeOrder(id,t) }
	
     def MakeOrder(id:SI_Term,t:SI_Term) = Agent {
         GSum( List(nigiri,maki,temaki,beer), D =>
           ( tell(order(id,t,D)) * tell(last_order(id,t,D)) ) ) }

     def CheckOrder(id:SI_Term,t:SI_Term) = Agent {
         GSum( List(nigiri,maki,temaki,beer), D =>
	    CheckOrderDish(id,t,D) ) }

     def CheckOrderDish(id:SI_Term,t:SI_Term,d:SI_Term) = Agent {
         val in_prepa = d.bsc_toString + " for " + id.bsc_toString +
	                " in preparation "
         val prepared = d.bsc_toString + " for " + id.bsc_toString +
	                " prepared but not served "
         val served   = d.bsc_toString + " for " + id.bsc_toString +
	                " served "			
			
         ( ask(order(id,t,d)) * Comm{ in_prepa } ) +
	 ( ask(last_order(id,t,d)) * (
	      ( ask(order_prepared(id,t,d)) * Comm{ prepared } ) +
	      ( ask(order_served(id,t,d)) * Comm{ served } ) ) )	 
    }


    /////////////////////////////  COOK  /////////////////////////////

    def Cook(id:SI_Term): BSC_Agent = Agent {
        GSum( List(manel,alice,emma), C => 
           ( CookForCustomer(id,C) * Cook(id) ) ) }

    def CookForCustomer(id_cook:SI_Term,id_client:SI_Term) = Agent{
        GSum( List(t1,t2,t3,t4), T =>
           CookForCustomerAtTable(id_cook,id_client,T) ) }

    def CookForCustomerAtTable(id_cook:SI_Term,
                    id_client:SI_Term,id_table:SI_Term) = Agent {
        GSum( List(nigiri,maki,temaki,beer), O =>
           ( get(order(id_client,id_table,O)) *
             tell(order_prepared(id_client,id_table,O)) ) ) }


    /////////////////////////////  WAITER  /////////////////////////////
    
    def Waiter(id_waiter:SI_Term): BSC_Agent = Agent {
        GSum( List(manel,alice,emma), U =>
          ( WaiterForCustomer(id_waiter,U) * Waiter(id_waiter) ) ) }

    def WaiterForCustomer(id_waiter:SI_Term,id_client:SI_Term) = Agent {
        GSum( List(t1,t2,t3,t4), T =>
	   WaiterForCustomerAtTable(id_waiter,id_client,T) ) }

    def WaiterForCustomerAtTable(id_waiter:SI_Term,
                  id_client:SI_Term,id_table:SI_Term) = Agent {
        GSum( List(nigiri,maki,temaki,beer), O =>
	   ( get(order_prepared(id_client,id_table,O)) *
             tell(order_served(id_client,id_table,O)) ) ) }


    /////////////////////////////  PEOPLE  /////////////////////////////

    val CManel = Agent { Client(manel) }
    val CAlice = Agent { Client(alice) }
    val CEmma  = Agent { Client(emma) }
    val CJohn  = Agent { Cook(john) }
    val WTom   = Agent { Waiter(tom) }

    val Restaurant = Agent { CManel || CAlice || CEmma || CJohn || WTom }


    /////////////////////////////   bHM FORMULA  /////////////////////////////

    val special_dish_for_manel =
      not(bf(order(manel,t1,nigiri))) and
      not(bf(order(manel,t2,maki))) and
      not(bf(order(manel,t3,temaki))) and
      not(bf(order(manel,t4,beer)))

    val start_login_manel = bf(logged(manel))
    val start_login_alice = bf(logged(alice))
    val start_login_emma = bf(logged(emma))

    val end_session = not(bf(logged(manel))) and
                      not(bf(logged(alice))) and
                      not(bf(logged(emma)))

    val F: BHM_Formula = bHM {
      start_login_manel * start_login_alice * start_login_emma * Fcont
    }
    
    val Fcont: BHM_Formula = bHM {
      (special_dish_for_manel * Fcont) + end_session
    }




    /////////////////////////////  EXECUTING  /////////////////////////////

    val bsc_executor = new BSC_Runner
    bsc_executor.execute(Restaurant,F)
    
}
 
