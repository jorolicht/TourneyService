package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.utils.{ Error, Return }


/*
 * information about ongoing match 
 */
case class Playfield(
  nr:        String,          
  used:      Boolean, 
  startTime: String,      // Format "yyyyMMddhhmmss" 
  coCode:    (Long,Int),  // (<coId>,<coPhIdh>)
  gameNo:    Int,
  playerA:   String, 
  clubA:     String, 
  playerB:   String, 
  clubB:     String,  
  coInfo:    String, 
  info:      String
) {
  def encode = write[Playfield](this)
}

object Playfield {
  implicit def rw: RW[Playfield] = macroRW
  def tupled = (this.apply _).tupled
  def dummy = new Playfield("", false, "19700101000000", (0L,0),0,"","","","","","")

  def decode(x: String): Either[Error, Playfield] = {
    try   Right(read[Playfield](x))
    catch { case _: Throwable => Left(Error("err0049.decode.Playfield", x.take(20))) }
  }

  def decSeq(pfl: String): Either[Error, Seq[Playfield]] = {  
    try Right(read[Seq[Playfield]](pfl))
    catch { case _: Throwable => Left(Error("err0051.decode.Playfields", pfl.take(20))) }
  }
}