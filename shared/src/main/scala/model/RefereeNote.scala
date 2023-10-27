package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

case class RefereeNote(
  val organizer: String,
  val trnyName:  String,
  val toId:      Long,
  val compName:  String,
  val coId:      Long,
  val phaseName: String,
  val coPhId:    Int,
  val gameNo:    Int,
  val finished:  Boolean,
  val winSets:   Int,
  val playerA:   String,
  val playerB:   String,
  val clubA:     String,
  val clubB:     String,
  val sets:      List[(Int,Int)]
) {
  def getSets = sets.foldLeft((0,0))((a, e) => if (e._1 > e._2) (a._1 +1, a._2) else if (e._2 > e._1) (a._1, a._2 + 1) else (a._1, a._2) )   
}

object RefereeNote {
  implicit def rw: RW[RefereeNote] = macroRW 
  def nonce(toId: Long, coId: Long, coPhId: Int, gameNo: Int): Long = {
    (toId * 119 + coId * 117 + coPhId * 113 + gameNo * 1379) % 1000
  } 
} 