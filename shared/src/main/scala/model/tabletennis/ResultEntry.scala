package shared.model.tabletennis

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.Constants._
import shared.utils.Routines._
import shared.model.tabletennis.utility._
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
  def stringify() = s"${valid}^${pos._1}^${pos._2}^${sno._1}^${sno._2}^${sets._1}^${sets._2}^${balls.mkString("·")}^"
}

object ResultEntry {
  def obify(kStr: String): ResultEntry = {
    val k = kStr.split("\\^")
    try { 
      if (k.length > 7) {
        ResultEntry(k(0).toBoolean, (k(1).toInt,k(2).toInt), (k(3),k(4)), (k(5).toInt,k(6).toInt), k(7).split('·'))
      } else {
        ResultEntry(k(0).toBoolean, (k(1).toInt,k(2).toInt), (k(3),k(4)), (k(5).toInt,k(6).toInt), Array[String]())
      }
    } catch { case _: Throwable => ResultEntry(false, (0,0), ("0","0"), (0,0),Array[String]()) }
  }


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


  def fromMatchEntry(m: MatchEntry, coTyp: Int, noWinSets: Int): ResultEntry = {
    val pos = (coTyp match {
      case CSY_GR => m.wgw
      case CSY_KO => (m.round, m.maNo)
      case _      => (0,0)
    })
    
    val balls     = m.result.split('·')
    val sets      = getSets(balls, noWinSets)

    ResultEntry(m.status >= 2 & pos != (0,0) & validSets(sets, noWinSets), pos, (m.stNoA,m.stNoB), sets, balls)
    //ResultEntry(m.status >= 2 & pos != (0,0), pos, (m.stNoA,m.stNoB), sets, balls)
  }

  def encSeq(reEns: Seq[ResultEntry]) = write[ResultEntrys](ResultEntrys(reEns.map(_.stringify)))

  def decSeq(reStr: String): Either[Error, Seq[ResultEntry]] = {  
    if (reStr== "") {
      Right(Seq())
    } else {
      try {
        val reEs = read[ResultEntrys](reStr)
        val reSeq = (for { rEn <- reEs.list } yield { ResultEntry.decode(rEn) }).toSeq
        reSeq.partitionMap(identity)  match {
          case (Nil, rights)       => Right(rights)
          case (firstLeft :: _, _) => Left(firstLeft.add("ResultEntry.deqSeq"))
        } 
      } catch { case _: Throwable => Left(Error("err0147.decode.ResultEntrys", reStr.take(20), "", "ResultEntry.deqSeq")) }
    }
  }

}

case class ResultEntrys (list : Seq[String])
object ResultEntrys { implicit def rw: RW[ResultEntrys] = macroRW }  