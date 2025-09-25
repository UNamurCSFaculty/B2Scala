package bscala

import bsc_data._
import bsc_blackboard._
import bsc_agent._


import scala.collection.mutable.Map

package object bscala {
        
    val errorCode = 404
    //val duration: Int = 1000000

    type TTimeStamp = Long
    type TDuration = Long
    
    //val defaultDuration: TDuration = 25  // 1 minute in milliseconds //name it infiniteDuration
    val infiniteDuration: TDuration = 100000

    //type TTimeStamp = Int
    //type RB_Context_Map = Map[RB_Topic, TTimeStamp]

    //def emptyCxtMap = Map[RB_Topic, TTimeStamp]()
    //val emptyCxtObject = new RB_CXT
    //def EmptyCxtAgent =  CxtAgent(EmptyAgent(),emptyCxtObject)

    def initTTS = 0 

    
}



  
