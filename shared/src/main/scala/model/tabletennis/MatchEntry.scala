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
  val coPh:        Int,                   // (3)  competition phase (round xx)
  val gameNo:      Int,                   // (3b) game number 
  
  // KO-System specific entries
  val round:       Int,                   // (4) round
  val maNo:        Int,                   // (5) match number within round

  // Group-System specific entries
  val grId:        Int,                   // (6) Group Identifcaton
  val wgw:         (Int,Int),             // (7,8) who against who  

  // Switzer System special case of Group-System uses
  // round = coPh mod 100
  // wgw

  var playfield:   String,                // (9) playfield eg. 1,2 or "table 5"
  var info:        String,                // (10) additional information of game

  var startTime:   String,                // (11)
  var endTime:     String,                // (12)

  var status:      Int,                   // (13) -1 = not finished (blocked)
                                          //       0 = not finished (runnable),
                                          //       1 = running
                                          //       2 = finished playerA won
                                          //       3 = finished playerB won
                                          //       4 = finished noWinner
  var result:     String                  // (14) format depending on type of competition
                                          //      TT MATCH: <Number Of Sets> . <set1> . <set2> . <set3> . <set4> ...
) extends {
  def stringify = s"${stNoA}^${stNoB}^$coId}^${coPh}^${gameNo}^${round}^${maNo}^${grId}^${wgw._1}^${wgw._2}^${playfield}^${info}^${startTime}^${endTime}^${status}^${result}^"
  def encode    = s"${stNoA}^${stNoB}^$coId}^${coPh}^${gameNo}^${round}^${maNo}^${grId}^${wgw._1}^${wgw._2}^${playfield}^${info}^${startTime}^${endTime}^${status}^${result}^"
  //Example                8^       4^     1^      8^       3^       2^     0^        0^        0^           0^       ^0           ^0         ^0        ^         ^

 
 def getPoints: (Int,Int) = {
    val nowisets = getMDInt(result,0)
    val sets = getSets
    if (sets._1 == nowisets)        { (1,0)
    } else if (sets._2 == nowisets) { (0,1)
    } else                          { (0,0) }
  }

  def getSets: (Int,Int) = {
    val nowisets = getMDInt(result,0)
    var setNo    = 0
    var nSetA    = 0
    var nSetB    = 0
    var finished = false

    do {
       setNo = setNo + 1
       var balls = getBall(setNo)  
       (balls._1 - balls._2) match {
         case a if (a > 0) => nSetA = nSetA + 1
         case b if (b < 0) => nSetB = nSetB + 1
         case _     => finished = true
       }  
    }  while (nSetA < nowisets & nSetB < nowisets & setNo < nowisets*2 & !finished) 
    (nSetA, nSetB)
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
    str ++= s"          coId: ${coId} coPh: ${coPh} gameNo: ${gameNo} round: ${round} maNo: ${maNo} grId: ${grId}\n"
    str ++= s"          wgw: (${wgw._1},${wgw._2}) playfield: ${playfield} info: ${info} status: ${status} result: ${result}\n"
    str.toString
  }

}


object MatchEntry {
  implicit def rw: RW[MatchEntry] = macroRW

  def decode(x: String): Either[Error, MatchEntry] = {
    try {
      val pa = x.split("\\^")
      Right(MatchEntry(
        pa(0),             pa(1),      
        pa(2).toLong,      pa(3).toInt, pa(4).toInt,
        pa(5).toInt,       pa(6).toInt, 

        pa(7).toInt,      
        (pa(8).toInt,pa(9).toInt), 
        pa(10),            pa(11),
        pa(12),            pa(13), 
        pa(14).toInt,      pa(15)      
      ))
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

  def obify(x: String) = objectify(x).getOrElse(
    MatchEntry("0", "0", 0L, 0, 0, 0, 0, 0, (0,0), "", "", "19700101000000", "19700101000000", 0, "")
  ) 
  def objectify(s: String) : Option[MatchEntry] = {
    val pa = s.split("\\^")
    try { 
      Some(MatchEntry(
        pa(0),            pa(1),      
        pa(2).toLong,     pa(3).toInt, pa(4).toInt,
        pa(5).toInt,      pa(6).toInt, 

        pa(7).toInt,      
        (pa(8).toInt,pa(9).toInt), 
        pa(10),            pa(11),
        pa(12),           pa(13), 
        pa(14).toInt,     pa(15)      
      ))
    } catch { case _: Throwable => None }
  }
}

case class Matches (list: Seq[String])
object Matches { implicit def rw: RW[Matches] = macroRW }