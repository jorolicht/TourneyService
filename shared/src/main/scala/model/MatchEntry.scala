package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.model.CompPhase._
import shared.utils.Routines._
import shared.utils.{ Error, Return }

trait MEntry {
  def coId: Long 
  def coTyp: Int 
  def coPhId: Int
  def coPhTyp: Int
  def stNoA: String
  def stNoB: String
  def round: Int
  def gameNo: Int
  def status: Int
  def playfield: String
  def sets: (Int,Int)
  def info: String
  def result: String
  def winSets: Int
  def encode: String
  def toString: String
  def toTx: MEntryTx

  def setPantA(sNoA: String):Unit
  def setPantB(sNoB: String):Unit

  def setPant(pos: Int, sNo: String): MEntry = {
    pos match {
      case 0 => setPantA(sNo)
      case 1 => setPantB(sNo)
    }
    this
  }

  def setSets(value:(Int,Int)):Unit
  def setResult(value:String):Unit
  def setPlayfield(value:String):Unit
  def setInfo(value:String):Unit
  def setStatus(value:Int):Unit

  def setStatus(depFinished: Boolean=true): MEntry = {
    MEntry.setRunning(this, false) 
    val blocked = MEntry.isPlayerRunning(stNoA, stNoB, coTyp) || !depFinished

    if      (validSets & (SNO(stNoA).isBye | SNO(stNoB).isBye)) { setStatus(MEntry.MS_FIX)   }
    else if (validSets)                                         { setStatus(MEntry.MS_FIN)   } 
    else if (SNO(stNoA).isNN | SNO(stNoB).isNN)                 { setStatus(MEntry.MS_MISS)  } 
    else if (blocked)                                           { setStatus(MEntry.MS_BLOCK) }
    else if (sets==(0,0) & playfield!="")                       { 
                                                                  MEntry.setRunning(this, true) 
                                                                  setStatus(MEntry.MS_RUN)   
                                                                }
    else if (sets==(0,0) & playfield=="")                       { setStatus(MEntry.MS_READY) }        
    else if (sets._1 == sets._2 & sets._1 != 0)                 { setStatus(MEntry.MS_DRAW)  }      
    else                                                        { setStatus(MEntry.MS_UNKN)  }
    this
  }

  def getPlayfield = {
    try { 
      val pfCode = playfield.split("·")
      pfCode(pfCode.size-1) 
    } catch  { case _: Throwable => "" }
  }

  def finished = ((status == MEntry.MS_FIN) || (status == MEntry.MS_DRAW))
  def validSets(): Boolean = ((sets._1 == winSets & sets._2 < winSets) | (sets._1 < winSets & sets._2 == winSets))

  def reset(resetPantA: Boolean=false, resetPantB: Boolean=false):MEntry = { 
    if (resetPantA) setPantA("")
    if (resetPantB) setPantB("")
    setPlayfield("") 
    setInfo("") 
    setSets((0,0)) 
    setResult("")
    this
  } 

  def getWinner(): String = {
    if      (sets._1 > sets._2) stNoA
    else if (sets._2 > sets._1) stNoB
    else ""
  }
  
  def getLooser(): String = {
    if      (sets._1 > sets._2) stNoB
    else if (sets._2 > sets._1) stNoA
    else ""
  }
}


case class MEntryBase(coId: Long, coTyp: Int, coPhId: Int, coPhTyp: Int, 
                      gameNo: Int=0, round: Int=0, var playfield:String="", var status:Int=0,
                      var info: String="", var sets: (Int,Int) =(0,0), var result:String="",
                      val winSets: Int=0, var stNoA: String="", var stNoB: String=""  ) 
  extends MEntry { 

  def toTx = MEntryTx(coId, coTyp, coPhId, coPhTyp, "")
  def encode: String = s"""{  "coId":${coId}, "coTyp":${coTyp}, "coPhId":${coPhId}, "coPhTyp":${coPhTyp}, "content"="^_" } """
  override def toString(): String = s"""  Base-Match"""

  def setPantA(sNoA: String) = stNoA = sNoA
  def setPantB(sNoB: String) = stNoB = sNoB

  def setSets(value:(Int,Int)) = { sets = value }
  def setResult(value:String)  = { result = value } 
  def setPlayfield(value: String) = { playfield = value }
  def setInfo(value: String)  = { info = value } 
  def setStatus(value:Int)    = { status = value }
}


case class MEntryKo(
  val coId: Long,                         // competition identifier
  val coTyp: Int,                         // competition typ, e.g. CT_SINGLE, CT_DOUBLE
  val coPhId: Int,                        // competition phase identifier
  val coPhTyp: Int,                       // competition phase system, eg. CPT_GR, CPT_KO
  val gameNo: Int,                        //(0) game number within phase
  
  var stNoA:  String,                     //(1) participant A start number
  var stNoB:  String,                     //(2) participant B start number
  
  var round:  Int,                        //(3) (KO-Round (7 ... 0) for 128-field/7,64-field/6 ... Final/1, 3rdPlace/0 
  var maNo:   Int,                        //(4) Match number within round
  var winPos: String,                     //(5) Next position of winner within match array (gameNo, matchNo within Round, intRound, pos(0/1))
  var looPos: String,                     //(6) Next position of looser within match array

  var playfield:   String,                //(7) playfield eg. 1,2 or "table 5"
  var info:        String,                //(8) additional information of game

  var startTime:   String,                //(9) Format: yyyymmddhhmmss
  var endTime:     String,                //(10)  
  var status:      Int,                   //(11) see Match Status Values: MS_xxx

  var sets:       (Int,Int),              //(12)(13) sets e.g. (3,1)
  val winSets:    Int,                    //(14) number of sets to win the match or 0 for draw             
  var result:     String                  //(15) result details, depending on kind of sport
                                          //     TT MATCH: <ball1> . <ball2> . <3> . <set4> ...

) extends MEntry {
  def toTx = MEntryTx(coId, coTyp, coPhId, coPhTyp, s"${gameNo}^${stNoA}^${stNoB}^${round}^${maNo}^${winPos}^${looPos}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${winSets}^${result}^_")
  def encode: String = s"""{"coId":${coId},"coTyp":${coTyp},"coPhId":${coPhId},"coPhTyp":${coPhTyp},"content":"${gameNo}^${stNoA}^${stNoB}^${round}^${maNo}^${winPos}^${looPos}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${winSets}^${result}^_"}"""
  override def toString(): String = s"""
    |  Ko-Match: SnoA: ${stNoA} - SnoB: ${stNoB} Winner->${winPos} Looser->${looPos}
    |    gameNo: ${gameNo} round: ${round} maNo: ${maNo} info: ${info} winSets: ${winSets}
    |    coId: ${coId} coTyp: ${coTyp} coPhId: ${coPhId} coPhTyp: ${coPhTyp}
    |    playfield: ${playfield} status: ${MEntry.statusInfo(status)} sets: ${sets._1}:${sets._2} result: ${result}
    """.stripMargin('|')

  def setPantA(sNoA: String) = stNoA = sNoA
  def setPantB(sNoB: String) = stNoB = sNoB    
  def setSets(value:(Int,Int)) = { sets = value }
  def setResult(value:String)  = { result = value } 
  def setPlayfield(value: String) = { playfield = value }
  def setInfo(value:String)  = { info = value }  
  def setStatus(value:Int)   = { status = value } 

  def getWinPos():(Int,Int) = {
    val wPos = getMDIntArr(winPos)
    if (wPos.size == 4) (wPos(0), wPos(3)) else (0,0)
  }

  def getLooPos():(Int,Int) = {
    val lPos = getMDIntArr(looPos)
    if (lPos.size == 4) (lPos(0), lPos(3)) else (0,0)
  }

}


case class MEntryGr(
  val coId:      Long,                   // competition identifier
  val coTyp:     Int,                    // competition typ, e.g. CT_SINGLE, CT_DOUBLE
  val coPhId:    Int,                    // competition phase identifier
  val coPhTyp:   Int,                    // competition phase system, eg. CPT_GR, CPT_KO
  val gameNo:    Int,                    //(0) game number within phase
  
  var stNoA:     String,                 //(1) participant A start number
  var stNoB:     String,                 //(2) participant B start number
  
  var round:     Int,                    //(3) Group Runde 1 ...  
  var grId:      Int,                    //(4) Group Identifcaton
  var wgw:       (Int,Int),              //(5,6) who against who
  var depend:    String,                 //(7) List of games that should be finished before, 
                                         //    separated by middle dot
  var trigger:   String,                 //(8) List of games/matches that should be triggered (status updated), 
                                         //    separated by middle dot
  var playfield: String,                 //(9) playfield eg. 1,2 or "table 5"
  var info:      String,                 //(10) additional information of game

  var startTime: String,                 //(11) Format: yyyymmddhhmmss
  var endTime:   String,                 //(12)  

  var status:    Int,                    //(13) see Match Status Values: MS_xxx
  var sets:      (Int,Int),              //(14)(15) sets e.g. (3,1)
  val winSets:   Int,                    //(16) number of sets to win the match or 0 for draw                                         
  var result:    String                  //(17) result details, depending on kind of sport
                                         //     TT MATCH: <ball1> . <ball2> .  ...
) extends MEntry {

  def toTx = MEntryTx(coId, coTyp, coPhId, coPhTyp, s"${gameNo}^${stNoA}^${stNoB}^${round}^${grId}^${wgw._1}^${wgw._2}^${depend}^${trigger}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${winSets}^${result}^_")
  def encode: String = s"""{"coId":${coId},"coTyp":${coTyp},"coPhId":${coPhId},"coPhTyp":${coPhTyp},"content":"${gameNo}^${stNoA}^${stNoB}^${round}^${grId}^${wgw._1}^${wgw._2}^${depend}^${trigger}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${winSets}^${result}^_"}"""
  override def toString(): String = s"""
      |  Group-Match: ${wgw._1}-${wgw._2} SnoA: ${stNoA} - SnoB: ${stNoB} 
      |    gameNo: ${gameNo} round: ${round} grId: ${grId} info: ${info} winSets: ${winSets}
      |    coId: ${coId} coTyp: ${coTyp} coPhId: ${coPhId} coPhTyp: ${coPhTyp}
      |    depend: ${depend} trigger: ${trigger} playfield: ${playfield} 
      |    status: ${MEntry.statusInfo(status)} sets: ${sets._1}:${sets._2} result: ${result}
  """.stripMargin('|')

  def setPantA(sNoA: String) = stNoA = sNoA
  def setPantB(sNoB: String) = stNoB = sNoB
  def setSets(value:(Int,Int))    = { sets = value }
  def setResult(value:String)     = { result = value } 
  def setPlayfield(value: String) = { playfield = value }
  def setInfo(value:String)       = { info = value } 
  def setStatus(value:Int)        = { status = value } 

  def getTrigger()                = getMDIntArr(trigger)   
  def getDepend()                 = getMDIntArr(depend)
  def hasDepend()                 = (depend != "_depend_")
    
}


case class MEntryTx(coId: Long, coTyp: Int, coPhId: Int, coPhTyp: Int, content: String) {
  def decode: MEntry = {
    coPhTyp match {
      case CPT_GR => {
      try { 
          val m = content.split("\\^")
          val (gameNo,     stNoA, stNoB, round,      grId,       wgw1,       wgw2,       depend, trigger, playfield, info, startTime, endTime, status,      sets1,       sets2,       winSets,     result) =
              (m(0).toInt, m(1),  m(2),  m(3).toInt, m(4).toInt, m(5).toInt, m(6).toInt, m(7),   m(8),    m(9),      m(10), m(11),    m(12),   m(13).toInt, m(14).toInt, m(15).toInt, m(16).toInt, m(17))
          MEntryGr(coId, coTyp, coPhId, coPhTyp, gameNo, stNoA, stNoB, round, grId, (wgw1,wgw2), depend, trigger, playfield, info, startTime, endTime, status, (sets1,sets2), winSets, result)
        } catch { case _: Throwable => MEntryGr(coId, coTyp, coPhId, coPhTyp,0,"","",0,0,(0,0),"","","","","","", 0,(0,0),0, "") }
      }
      case CPT_KO => {
        try { 
          val m = content.split("\\^")
          val (gameNo,     stNoA, stNoB, round,      maNo,       winPos, looPos, playfield, info, startTime, endTime, status,      sets1,       sets2,       winSets,    result) =
              (m(0).toInt, m(1),  m(2),  m(3).toInt, m(4).toInt, m(5),   m(6),   m(7),      m(8), m(9),      m(10),   m(11).toInt, m(12).toInt, m(13).toInt, m(14).toInt, m(15))
          MEntryKo(coId, coTyp, coPhId, coPhTyp, gameNo, stNoA, stNoB, round, maNo, winPos, looPos, playfield, info, 
                   startTime, endTime, status, (sets1,sets2), winSets, result)
        } catch { case _: Throwable => MEntryKo(coId, coTyp, coPhId, coPhTyp,0,"","",0,0,"","","","","","",0,(0,0),0,"") }
      }   
      case _      => MEntryBase(coId, coTyp, coPhId, coPhTyp)
    }
  }
}

object MEntryTx {
  implicit def rw: RW[MEntryTx] = macroRW
}  

object MEntry {
  import scala.collection.mutable.HashSet
  import scala.collection.mutable.Map

  // Match Status Values
  val MS_RESET = -3   // match not yet configured
  val MS_MISS  = -2   // not finished (player missing)
  val MS_BLOCK = -1   // not finished (blocked)
  val MS_READY =  0   // not finished (runnable/ready)
  val MS_RUN   =  1   // running
  val MS_FIN   =  2   // finished with winner
  val MS_FIX   =  3   // finished with fixed winner (bye ...)
  val MS_DRAW  =  4   // finished with no winner
  val MS_UNKN  = 99   // finished with no winner

  // list of player currently playing in (coId, coIdPh, gameNo)
  val playing: Map[Long, HashSet[(Long,Int,Int)]] = Map().withDefaultValue(HashSet())

  def statusInfo(value: Int) = value match {
    case MS_RESET  => "RESET"
    case MS_MISS   => "MISS"
    case MS_BLOCK  => "BLOCK"
    case MS_READY  => "READY"
    case MS_RUN    => "RUN"
    case MS_FIN    => "FIN"
    case MS_FIX    => "FIX"    
    case MS_DRAW   => "DRAW"
    case MS_UNKN   => "UNKN"
    case _         => "ERROR"
      
  }

  /** addRunning(plId: Long, gaId: (Long,Int,Int))
   *  game identifier = tripple (competition identifier, competition phase identifier, game number)
   */
  def addPlayerRunning(plId: Long, gaId: (Long,Int,Int)) = {
    if (plId != 0) (playing(plId)) += (gaId)
  }  
  def removePlayerRunning(plId: Long, gaId: (Long,Int,Int)) = {
    if (plId != 0) (playing(plId)) -= (gaId) 
  }  
  def getPlayerRunning(plId1: Long, plId2: Long=0 ): Boolean = {
    if (plId2 == 0) { playing(plId1).size > 0 } else { (playing(plId1).size > 0) | (playing(plId2).size > 0) }
  } 


  def isPlayerRunning(snoA: String, snoB: String, coTyp: Int): Boolean = {
    import shared.model.Competition._
    coTyp match {
      case CT_SINGLE => getPlayerRunning(SNO.plId(snoA), SNO.plId(snoB))

      case CT_DOUBLE => {
        val idAs = getMDLongArr(snoA)
        val idBs = getMDLongArr(snoB)
        getPlayerRunning(idAs(0), idAs(1)) | getPlayerRunning(idBs(0), idBs(1))
      }
    }
  }

  def setRunning(m: MEntry, run: Boolean) = {
    import shared.model.Competition._
    import shared.utils.Routines._
    val gaId = (m.coId, m.coPhId, m.gameNo)
    m.coTyp match {
      case CT_SINGLE => {
        if (run) addPlayerRunning(SNO.plId(m.stNoA), gaId) else removePlayerRunning(SNO.plId(m.stNoA), gaId)
        if (run) addPlayerRunning(SNO.plId(m.stNoB), gaId) else removePlayerRunning(SNO.plId(m.stNoB), gaId)
      }
      case CT_DOUBLE => {
        val idAs = getMDLongArr(m.stNoA)
        val idBs = getMDLongArr(m.stNoB)
        if (run) addPlayerRunning(idAs(0), gaId) else removePlayerRunning(idAs(0), gaId)
        if (run) addPlayerRunning(idAs(1), gaId) else removePlayerRunning(idAs(1), gaId)
        if (run) addPlayerRunning(idBs(0), gaId) else removePlayerRunning(idBs(0), gaId)
        if (run) addPlayerRunning(idBs(1), gaId) else removePlayerRunning(idBs(1), gaId)
      }
    }
  }




}


