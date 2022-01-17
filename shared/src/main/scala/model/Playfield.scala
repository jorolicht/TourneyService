package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.utils.{ Error, Return }


/*
 * information about ongoing match 
 */
case class Playfield(
  nr:        Int, 
  used:      Boolean, 
  startTime: String,  // Format "yyyyMMddhhmmss" 
  code:      String,  // <coId> <md> <coph> <md> <gamenumber>
  playerA:   String, 
  clubA:     String, 
  playerB:   String, 
  clubB:     String,  
  pClass:    String, 
  info:      String
) {
  def stringify = s"${nr}^${used}^${startTime}^${code}^${playerA}^${clubA}^${playerB}^${clubB}^${pClass}^${info}^_"
  def encode() = s"${nr}^${used}^${startTime}^${code}^${playerA}^${clubA}^${playerB}^${clubB}^${pClass}^${info}^_"
}

object Playfield {
  def tupled = (this.apply _).tupled

  def decode(x: String): Either[Error, Playfield] = {
    val pf = x.split("\\^")
    try   Right(Playfield(pf(0).toInt,pf(1).toBoolean, pf(2), pf(3), pf(4), pf(5), pf(6), pf(7), pf(8), pf(9)))
    catch { case _: Throwable => Left(Error("err0049.decode.Playfield", x.take(20))) }
  }

  def encSeq(pfields: Seq[Playfield]): String = write[Playfields](Playfields(pfields.map(_.stringify)))

  def decSeq(pfl: String): Either[Error, Seq[Playfield]] = {  
    if (pfl == "") {
      Right(Seq())
    } else {
      try {
        val pfs = read[Playfields](pfl)
        val pfsSeq = (for { pf <- pfs.list } yield { Playfield.decode(pf) }).toSeq
        pfsSeq.partitionMap(identity)  match {
          case (Nil, rights)       => Right(rights)
          case (firstLeft :: _, _) => Left(firstLeft)
        } 
      } catch { case _: Throwable => Left(Error("err0051.decode.Playfields", pfl.take(20))) }
    }
  } 

} 

case class Playfields (list: Seq[String])
object Playfields { implicit def rw: RW[Playfields] = macroRW }  



/** PfieldInfo - alternative info to set the playfield
  * 
  * @param nr
  * @param used
  * @param code // <coId> <md> <coph> <md> <gamenumber>
  * @param coId
  * @param snoA
  * @param snoB
  * @param info
  */
case class PfieldInfo(nr: Int, used: Boolean, code: String, coId: Long, snoA: String, snoB: String, info: String) {
  def encode() = s"${nr}^${used}^${code}^${coId}^${snoA}^${snoB}^${info}^_"
}

object PfieldInfo { 
  def decode(pfi: String): Either[Error, PfieldInfo] = {
    val x = pfi.split("\\^")
    try Right(PfieldInfo(x(0).toInt, x(1).toBoolean, x(2), x(3).toLong, x(4), x(5), x(6)))
    catch { case _: Throwable => Left(Error("err0084.decode.PfieldInfo", pfi)) }
  }
}