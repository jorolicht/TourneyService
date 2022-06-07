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
  def round: Int
  def gameNo: Int
  def playfield: String
  def encode: String
  def toString: String
  def toTx: MEntryTx

  def getPlayfield = {
    try { 
      val pfCode = playfield.split("·")
      pfCode(pfCode.size-1) 
    } catch  { case _: Throwable => "" }
  }

}

case class MEntryBase(coId: Long, coTyp: Int, coPhId: Int, coPhTyp: Int, gameNo: Int=0, round: Int=0, playfield:String="") 
  extends MEntry { 

  def toTx = MEntryTx(coId, coTyp, coPhId, coPhTyp, "")
  def encode: String = s"""{  "coId"=${coId}, "coTyp"=${coTyp}, "coPhId"=${coPhId}, "coPhTyp"=${coPhTyp}, "content"="^_" } """
  override def toString(): String = s"""  Base-Match"""
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
  var winPos: String,                     //(5) Next position of winner within match array
  var looPos: String,                     //(6) Next position of looser within match array

  var playfield:   String,                //(7) playfield eg. 1,2 or "table 5"
  var info:        String,                //(8) additional information of game

  var startTime:   String,                //(9) Format: yyyymmddhhmmss
  var endTime:     String,                //(10)  

  var status:      Int,                   //(11)   -1 = not finished (blocked)
                                          //       0 = not finished (runnable),
                                          //       1 = running
                                          //       2 = finished playerA won
                                          //       3 = finished playerB won
                                          //       4 = finished noWinner

  var sets:       (Int,Int),              //(12)(13) sets e.g. (3,1)                                         
  var result:     String                  //(14) result details, depending on kind of sport
                                          //     TT MATCH: <ball1> . <ball2> . <3> . <set4> ...

) extends MEntry {

  def toTx = MEntryTx(coId, coTyp, coPhId, coPhTyp, s"${gameNo}^${stNoA}^${stNoB}^${round}^${maNo}^${winPos}^${looPos}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${result}^_")

  def encode: String = s"""{"coId":${coId},"coTyp":${coTyp},"coPhId":${coPhId},"coPhTyp":${coPhTyp},"content":"${gameNo}^${stNoA}^${stNoB}^${round}^${maNo}^${winPos}^${looPos}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${result}^_"}"""
  
  override def toString(): String = s"""
    |  Ko-Match: SnoA: ${stNoA} - SnoB: ${stNoB} Winner->${winPos} Looser->${looPos}
    |    gameNo: ${gameNo} round: ${round} maNo: ${maNo} info: ${info}
    |    coId: ${coId} coTyp: ${coTyp} coPhId: ${coPhId} coPhTyp: ${coPhTyp}
    |    playfield: ${playfield} status: ${status} sets: ${sets._1}:${sets._2} result: ${result}
    """.stripMargin('|')
}


case class MEntryGr(
  val coId:    Long,                       // competition identifier
  val coTyp:   Int,                        // competition typ, e.g. CT_SINGLE, CT_DOUBLE
  val coPhId:  Int,                        // competition phase identifier
  val coPhTyp: Int,                        // competition phase system, eg. CPT_GR, CPT_KO
  val gameNo:  Int,                        //(0) game number within phase
  
  var stNoA:   String,                     //(1) participant A start number
  var stNoB:   String,                     //(2) participant B start number
  
  var round:   Int,                        //(3) Group Runde 1 ...  
  var grId:    Int,                        //(4) Group Identifcaton
  var wgw:     (Int,Int),                  //(5,6) who against who  

  var playfield:   String,                //(7) playfield eg. 1,2 or "table 5"
  var info:        String,                //(8) additional information of game

  var startTime:   String,                //(9) Format: yyyymmddhhmmss
  var endTime:     String,                //(10)  

  var status:      Int,                   //(11)   -1 = not finished (blocked)
                                          //       0 = not finished (runnable),
                                          //       1 = running
                                          //       2 = finished playerA won
                                          //       3 = finished playerB won
                                          //       4 = finished noWinner

  var sets:       (Int,Int),              //(12)(13) sets e.g. (3,1)                                         
  var result:     String                  //(14) result details, depending on kind of sport
                                          //     TT MATCH: <ball1> . <ball2> . <3> . <set4> ...

) extends MEntry {

  def toTx = MEntryTx(coId, coTyp, coPhId, coPhTyp, s"${gameNo}^${stNoA}^${stNoB}^${round}^${grId}^${wgw._1}^${wgw._2}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${result}^_")

  def encode: String = s"""{"coId":${coId},"coTyp":${coTyp},"coPhId":${coPhId},"coPhTyp":${coPhTyp},"content":"${gameNo}^${stNoA}^${stNoB}^${round}^${grId}^${wgw._1}^${wgw._2}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${result}^_"}"""

  override def toString(): String = s"""
      |  Group-Match: ${wgw._1}-${wgw._2} SnoA: ${stNoA} - SnoB: ${stNoB} 
      |    gameNo: ${gameNo} round: ${round} grId: ${grId} info: ${info}
      |    coId: ${coId} coTyp: ${coTyp} coPhId: ${coPhId} coPhTyp: ${coPhTyp}
      |    playfield: ${playfield} status: ${status} sets: ${sets._1}:${sets._2} result: ${result}
  """.stripMargin('|')
}


case class MEntryTx(coId: Long, coTyp: Int, coPhId: Int, coPhTyp: Int, content: String) {
  def decode: MEntry = {
    coPhTyp match {
      case CPT_GR => {
      try { 
          val m = content.split("\\^")
          val (gameNo,     stNoA, stNoB, round,      grId,       wgw1,       wgw2,       playfield, info, startTime, endTime, status,      sets1,       sets2,       result) =
              (m(0).toInt, m(1),  m(2),  m(3).toInt, m(4).toInt, m(5).toInt, m(6).toInt, m(7),      m(8), m(9),      m(10),   m(11).toInt, m(12).toInt, m(13).toInt, m(14))
          MEntryGr(coId, coTyp, coPhId, coPhTyp, gameNo, stNoA, stNoB, round, grId, (wgw1,wgw2), playfield, info, startTime, endTime, status, (sets1,sets2), result)
        } catch { case _: Throwable => MEntryGr(coId, coTyp, coPhId, coPhTyp,0,"","",0,0,(0,0),"","","","", 0,(0,0),"") }
      }
      case CPT_KO => {
        try { 
          val m = content.split("\\^")
          val (gameNo,     stNoA, stNoB, round,      maNo,       winPos, looPos, playfield, info, startTime, endTime, status,      sets1,       sets2,       result) =
              (m(0).toInt, m(1),  m(2),  m(3).toInt, m(4).toInt, m(5),   m(6),   m(7),      m(8), m(9),      m(10),   m(11).toInt, m(12).toInt, m(13).toInt, m(14))
          MEntryKo(coId, coTyp, coPhId, coPhTyp, gameNo, stNoA, stNoB, round, maNo, winPos, looPos, playfield, info, 
                   startTime, endTime, status, (sets1,sets2), result)
        } catch { case _: Throwable => MEntryKo(coId, coTyp, coPhId, coPhTyp,0,"","",0,0,"","","","","","",0,(0,0),"") }
      }   
      case _      => MEntryBase(coId, coTyp, coPhId, coPhTyp,0,0)
    }
  }
}

object MEntryTx {
  implicit def rw: RW[MEntryTx] = macroRW
}  
