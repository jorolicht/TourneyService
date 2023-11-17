package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.Constants._
import shared.utils.Routines._
import shared.model.{ MEntry, MEntryGr, MEntryKo, MEntryBase }
import shared.model.CompPhase._
import shared.model.Utility._
import shared.utils.{ Error, Return }


/** 
 * Result entry for group and ko matches   
 */
case class ResultEntry(
  var valid: Boolean, 
  var pos:   (Int,Int),                // KO:     pos._1 = rnd, pos._2 = match number
                                       // GROUP:  pos._1 = 1..size, pos._2 = 1..size (wgw)
  var sno:   (String, String),
  var sets:  (Int,Int),                // sets and balls with
  var balls: Array[String]             // view from player A
) {
  override def toString = s"ResultEntry valid:${valid} pos:${pos} sno:${sno} sets:${sets} balls:${balls.mkString(":")}" 
}

object ResultEntry {
  implicit def rw: RW[ResultEntry] = macroRW

  def decode(kStr: String): Either[Error, ResultEntry] = {
    val k = kStr.split("\\^")
    try { 
      if (k.length > 7) {
        Right( ResultEntry(k(0).toBoolean, (k(1).toInt,k(2).toInt), (k(3),k(4)), (k(5).toInt,k(6).toInt), k(7).split('·')) )
      } else {
        Right( ResultEntry(k(0).toBoolean, (k(1).toInt,k(2).toInt), (k(3),k(4)), (k(5).toInt,k(6).toInt), Array[String]()) )
      }
    } catch { case _: Throwable => Left(Error("err0148.decode.ResultEntry", kStr.take(10), "", "ResultEntry.decode")) }
  }


  def fromMatchEntry(mEntry: MEntry): ResultEntry = {
    mEntry.coPhTyp match {
      case CompPhaseTyp.GR => {
        val m = mEntry.asInstanceOf[MEntryGr]
        ResultEntry(m.status >= 2 & m.validSets(), m.wgw, (m.stNoA,m.stNoB), m.sets, m.result.split('·'))
      }  
      case CompPhaseTyp.KO => {
        val m = mEntry.asInstanceOf[MEntryKo]
        ResultEntry(m.status >= 2 & m.validSets(), (m.round, m.maNo), (m.stNoA,m.stNoB), m.sets, m.result.split('·'))
      }  
      case _      => ResultEntry(false, (0,0), ("",""), (0,0), Array(""))
    }
  }

  def decSeq(reEntStr: String): Either[Error, Seq[ResultEntry]] = {
    try Right(read[Seq[ResultEntry]](reEntStr))  
    catch { case _: Throwable => Left(Error("err0147.decode.ResultEntrys", reEntStr.take(20), "", "ResultEntry.decSeq")) }
  }

}
