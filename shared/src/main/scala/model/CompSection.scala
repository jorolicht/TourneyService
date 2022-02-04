package shared.model

import scala.util.matching
import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils._
import shared.utils.Constants._
import shared.utils.Routines._
import shared.model.tabletennis._

/** CompSection describes a phase of a competition like first round
 *  intermediate round or final round. Every competition has at least
 *  a final round. Number of players always less or equal size.
 */
case class CompSection(val id: Int, val preId: Int, val coId: Long, val name: String, val secTyp: Int) {
  var noWinSets = 0 
  var winId     = 0  
  var looId     = 0 
  var status    = 0
  var size      = 0
  var rounds    = 0
  var noPlayer  = 0
  var pants     = Array[SNO]()
  var matches   = ArrayBuffer[ArrayBuffer[MatchEntry]]()

  var koRound: KoRound = _
  var switz:     Group = _
  var group:     Group = _
  var groups           = Vector[Group]()

  def setMatches(round: Int, matchList: ArrayBuffer[MatchEntry]) = {
    for (rnd <- matches.size to round) matches += ArrayBuffer[MatchEntry]()
    matches(round) = matchList
  }


  def encode(): String = {
    write[CompSectionTx](toTx())
  }

  def toTx(): CompSectionTx = {
    CompSectionTx(id, preId, winId, looId, name, coId, secTyp, noWinSets, 
                  rounds, status, size, noPlayer, pants.map(_.toString).toList, matches.map(_.toList).toList, "XXX")
  }
  
  override def toString(): String = {
    def pants2Str() = {
      val str = new StringBuilder("-- PARTICIPANTS\n")
      for { p <- pants }  yield { str ++= s"${p.value}:" } ; str.toString
    }
    def matches2Str() = {
      val str = new StringBuilder("-- MATCHES\n")
      for { (mList) <- matches }  yield { 
        if (mList.size > 0) {
          str ++= s"      ROUND: ${mList(0).round}\n" 
          for (m <- mList) yield str ++= m.toString
        }
      } 
      str.toString
    }

    s"""\nCOMPETITION SECTION: ${name} [coId: ${coId}]
       |  id: ${id} preId: ${preId} winId: ${winId} looId: ${looId}
       |  system:  ${csy2Name(secTyp)}
       |  winSets: ${noWinSets}
       |  ${pants2Str()}
       |  ${matches2Str()}
       |""".stripMargin('|')
  }
}


object CompSection {
  implicit def rw: RW[CompSection] = macroRW

  def decode(coStr: String): Either[Error, CompSection] = 
    try Right(fromTx(read[CompSectionTx](coStr)))
    catch { case _: Throwable => Left(Error("err0167.decode.CompSystem", coStr.take(20))) }

  def fromTx(tx: CompSectionTx): CompSection = {
    val coSec      = new CompSection(tx.id, tx.preId, tx.coId, tx.name, tx.secTyp)
                                  //(id: Int, preId: Int, coId: Long, name: String, secTyp: Int)
    coSec.winId     = tx.winId
    coSec.looId     = tx.looId
    coSec.noWinSets = tx.noWinSets

    coSec.status    = tx.status
    coSec.rounds    = tx.rounds
    coSec.size      = tx.size
    coSec.noPlayer  = tx.noPlayer
    coSec.matches   = tx.matches.map(_.to(ArrayBuffer)).to(ArrayBuffer)
    tx.secTyp match {
      case CST_JGJ    => ()
      case CST_GRPS3  => ()
      case _          => ()
    } 
    coSec
  }

  // decSeq - decode encoded Competition Section to sequence 
  def decSeq(input: String): Either[Error, Seq[CompSection]] = {
    try {
      val coSects = read[CompSectionsTx](input)
      (coSects.list.map{ co => CompSection.decode(co) }).partitionMap(identity) match {
        case (Nil, rights)      => Right(rights.toSeq)
        case (firstErr :: _, _) => Left(firstErr.add("CompSection.decSeq"))
      }
    } catch { case _: Throwable => Left(Error("err0176.decode.CompSection", input.take(20), "", "CompSection.deqSeq")) }
  }

}  


case class CompSectionTx (
  val id:        Int,
  val preId:     Int,
  val winId:     Int,
  val looId:     Int,
  val name:      String,
  val coId:      Long, 
  val secTyp:    Int,
  val noWinSets: Int,
  val rounds:    Int,
  val status:    Int,
  val size:      Int,
  val noPlayer:  Int,
  val pants:     List[String],
  val matches:   List[List[MatchEntry]],
  val system:    String
)

object CompSectionTx {
  implicit def rw: RW[CompSectionTx] = macroRW
}

case class CompSectionsTx (list: Seq[String])
object CompSectionsTx { implicit def rw: RW[CompSectionsTx] = macroRW }