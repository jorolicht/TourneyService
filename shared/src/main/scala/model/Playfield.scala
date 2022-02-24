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
  def encode = write[Playfield](this)
}

object Playfield {
  implicit def rw: RW[Playfield] = macroRW
  def tupled = (this.apply _).tupled

  def decode(x: String): Either[Error, Playfield] = {
    try   Right(read[Playfield](x))
    catch { case _: Throwable => Left(Error("err0049.decode.Playfield", x.take(20))) }
  }

  def decSeq(pfl: String): Either[Error, Seq[Playfield]] = {  
    try Right(read[Seq[Playfield]](pfl))
    catch { case _: Throwable => Left(Error("err0051.decode.Playfields", pfl.take(20))) }
  }
} 


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
case class PfieldInfo(
  nr:   Int, 
  used: Boolean, 
  code: String, 
  coId: Long, 
  snoA: String, 
  snoB: String, 
  info: String
) {
  def encode = write[PfieldInfo](this)
}

object PfieldInfo { 
  implicit def rw: RW[PfieldInfo] = macroRW
  def tupled = (this.apply _).tupled

  def decode(x: String): Either[Error, PfieldInfo] = {
    try Right(read[PfieldInfo](x))
    catch { case _: Throwable => Left(Error("err0084.decode.PfieldInfo", x)) }
  }
}