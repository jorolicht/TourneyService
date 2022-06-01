package shared.model.tabletennis

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.model.CompPhase._
import shared.utils.Routines._
import shared.utils.{ Error, Return }


/**
 *  Match entry for TT games   
 */
case class MatchEntry(                                             
  val stNoA:       String,                // (0) player A id
  val stNoB:       String,                // (1) player A id
 
  // general values
  val coId:        Long,                  // (2)  competition id 
  val coPh:        Int,                   // (3)  competition phase
  val coPhId:      Int,                   // (4) competition phase identifier
  val gameNo:      Int,                   // (5) game number unique within phase/section
  
  // KO-System specific entries
  val round:       Int,                   // (6) round / intRound for Group-System
  val maNo:        Int,                   // (7) match number within round

  // Group-System specific entries
  val grId:        Int,                   // (8) Group Identifcaton
  val wgw:         (Int,Int),             // (9,10) who against who  

  // Switzer System special case of Group-System uses
  // round = coPh mod 100
  // wgw

  var playfield:   String,                // (11) playfield eg. 1,2 or "table 5"
  var info:        String,                // (12) additional information of game

  var startTime:   String,                // (13)
  var endTime:     String,                // (14)

  var status:      Int,                   // (15) -1 = not finished (blocked)
                                          //       0 = not finished (runnable),
                                          //       1 = running
                                          //       2 = finished playerA won
                                          //       3 = finished playerB won
                                          //       4 = finished noWinner
  var sets:       (Int,Int),              // (16,17)                                         
  var result:     String                  // (18) format depending on type of competition
                                          //      TT MATCH: <Number Of Sets> . <set1> . <set2> . <set3> . <set4> ...
) extends {
  def stringify = s"${stNoA}^${stNoB}^${coId}^${coPh}^${coPhId}^${gameNo}^${round}^${maNo}^${grId}^${wgw._1}^${wgw._2}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${result}^_"
  def encode    = s"${stNoA}^${stNoB}^${coId}^${coPh}^${coPhId}^${gameNo}^${round}^${maNo}^${grId}^${wgw._1}^${wgw._2}^${playfield}^${info}^${startTime}^${endTime}^${status}^${sets._1}^${sets._2}^${result}^_"
  //Example                8^       4^     1^      8^       3^          2^     0^        0^      0^        0^      ^0           ^0         ^0        ^             ^
  //                   00025^   00018^     1^      1^       1^          1^     0^        0^      1^        1^       5^        1·1·1^       ^         ^20220207170409^       2^ 3^2^                  8·-4·4·-11·0^_",
 
 def getPoints: (Int,Int) = {
    val nowisets = getMDInt(result,0)
    val sets = getSets
    if (sets._1 == nowisets)        { (1,0)
    } else if (sets._2 == nowisets) { (0,1)
    } else                          { (0,0) }
  }

  def getSets: (Int,Int) = sets
  def printSets = {
    if (sets._1 == 0 & sets._2 == 0) "" else s"${sets._1}:${sets._2}"
  }

  def getBalls: (Int, Int) = {
    val nowisets = getMDInt(result,0)
    var setNo            = 0
    var nBallA           = 0
    var nBallB           = 0
    var finished         = false

    do {
       setNo = setNo + 1
       var balls = getBall(setNo)  
       balls._1 - balls._2 match {
         case a if (a != 0) => nBallA = nBallA + balls._1; nBallB = nBallB + balls._2
         case _             => finished = true
       }  
    }  while (setNo < nowisets*2 & !finished) 
    (nBallA, nBallB)
  }

  def getBall(setNo: Int): (Int, Int) = getBallFromStr(getMDStr(result, setNo))
  def getBallArr(): Array[String] = {
    try { 
     val bArr = result.split('·')
     val res = Array.fill[String](bArr.length)("") 
     for (i<-0 to bArr.length-1) res(i) = bArr(i)
     res
    } catch { case _: Throwable => Array() } 
  }

  def getBallFromStr(b: String) = {
    if (b == "")                      { (-1,-1)
    } else if (b == "+0" | b == "0") { (11,0)
    } else if (b == "-0")             { (0,11)
    } else { b.toIntOption.getOrElse(0) match {
      case a if   10 to 500 contains a => (a+2, a)
      case b if    1 to   9 contains b => (11, b)
      case c if   -9 to  -1 contains c => (-c, 11)
      case d if -500 to -10 contains d => (-d, 2 - d) 
      case _                           => (-1,-1)
    }}
  }

  def getPlayfield = {
    try { 
      val pfCode = playfield.split("·")
      pfCode(pfCode.size-1) 
    } catch  { case _: Throwable => "" }
  }

  def getType(): Int = {
    coPh match {
      case CP_VRGR | CP_ZRGR | CP_ERGR | CP_TRGR => CPT_GR 
      case CP_ERKO | CP_VRKO | CP_ZRKO | CP_TRKO => CPT_KO
      case _                                     => CPT_UNKN
    }
  }

  override def toString(): String = {
    val str = new StringBuilder("")
    str ++= s"        Match: SnoA: ${stNoA} - SnoB: ${stNoB}\n"
    str ++= s"          coId: ${coId} coPh: ${coPh} coPhId: ${coPhId} gameNo: ${gameNo} round: ${round} maNo: ${maNo} grId: ${grId}\n"
    str ++= s"          wgw: (${wgw._1},${wgw._2}) playfield: ${playfield} info: ${info} status: ${status} result: ${result}\n"
    str.toString
  }
}


object MatchEntry {
  implicit def rw: RW[MatchEntry] = macroRW

  def decode(x: String): Either[Error, MatchEntry] = {
    try {
      val pa = x.split("\\^")
      Right(MatchEntry(pa(0), pa(1), pa(2).toLong, pa(3).toInt, pa(4).toInt, pa(5).toInt, pa(6).toInt, pa(7).toInt, pa(8).toInt, 
                      (pa(9).toInt,pa(10).toInt), pa(11), pa(12), pa(13), pa(14), pa(15).toInt, (pa(16).toInt,pa(17).toInt), pa(18) ))
    } catch { case _: Throwable => Left(Error("err0165.decode.Match", x)) }
  }

  def encSeq(matches: Seq[MatchEntry]) = write[Matches](Matches(matches.map(_.encode)))

  def decSeq(mSeqStr: String): Either[Error, Seq[MatchEntry]] = {  
    if (mSeqStr == "") {
      Right(Seq())
    } else {
      try {
        val matches = read[Matches](mSeqStr)
        val mSeq = (for { m <- matches.list } yield { MatchEntry.decode(m) }).toSeq
        mSeq.partitionMap(identity)  match {
          case (Nil, rights)       => Right(rights)
          case (firstLeft :: _, _) => Left(firstLeft.add("Match.deqSeq"))
        } 
      } catch { case _: Throwable => Left(Error("err0166.decode.Matches", mSeqStr.take(20),"","Match.deqSeq")) }
    }
  }

  def obify(x: String) = objectify(x).getOrElse(MatchEntry("", "", 0L, 0, 0, 0, 0, 0, 0, (0,0), "", "", "", "", 0, (0,0), "")) 
  def objectify(s: String) : Option[MatchEntry] = {
    val pa = s.split("\\^")
    try Some(MatchEntry(pa(0), pa(1), pa(2).toLong, pa(3).toInt, pa(4).toInt, pa(5).toInt, pa(6).toInt, pa(7).toInt,
                        pa(8).toInt, (pa(9).toInt,pa(10).toInt), pa(11), pa(12), pa(13), pa(14), pa(15).toInt, (pa(16).toInt,pa(17).toInt), pa(18) ))
    catch { case _: Throwable => None }
  }
}

case class Matches (list: Seq[String])
object Matches { implicit def rw: RW[Matches] = macroRW }