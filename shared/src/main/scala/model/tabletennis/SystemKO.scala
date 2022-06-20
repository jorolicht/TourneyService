package shared.model.tabletennis

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.Constants._
import shared.model.CompPhase._
import shared.model.MEntryKo
import shared.model.ParticipantEntry
import shared.model.tabletennis.utility._

class KoRound(val size: Int,  val rnds: Int, var name: String, val noWinSets: Int) {
  var pants    = Array.fill[ParticipantEntry](size) (ParticipantEntry("0", "", "", 0, (0,0))) 
  var results  = Array.fill[ResultEntry](size) (ResultEntry(false, (0,0), ("0","0"), (0,0), Array[String]() ))
  var sno2pos  = scala.collection.mutable.Map[String, Int]()

  // calculate index of match within results
  def getIndex(pos: (Int, Int)): Int = getIndex(pos._1,pos._2)
  def getIndex(rnd: Int, mano: Int): Int = {
    var index = 0
    if (rnd > 0 ) {
      for (r <- rnd to rnds-1) { index = index + scala.math.pow(2,r).toInt }
      index = index + mano - 1
    } else {
      index = scala.math.pow(2,rnds).toInt - 1
    }
    index
  }

  def getRndManoFromIndex(index: Int):(Int,Int) = {
    size - index match {
      case a if   65 to 128 contains a => (7,index%64+1)
      case b if   33 to  64 contains b => (6,index%32+1)
      case c if   17 to  32 contains c => (5,index%16+1)
      case d if    9 to  16 contains d => (4,index%8+1)
      case e if    5 to   8 contains e => (3,index%4+1)
      case f if    3 to   4 contains f => (2,index%2+1)
      case 2                           => (1,1)
      case 1                           => (0,1)
      case _                           => (0,0)
    }
  }

  // name of player in ko tree view
  def getPlayerKoViewName(sno: String) = {
    try {
      val plArrName = pants(sno2pos(sno)).name.split("/")
      if (plArrName.length == 2) {
        s"${plArrName(0).split(",")(0).trim} / ${plArrName(1).split(",")(0).trim}"
      } else if (plArrName.length == 1) {
        s"${plArrName(0).split(",")(0).trim}"
      } else {
        pants(sno2pos(sno)).name
      }
    } catch { case _: Throwable => "?" } 
  }


  def validPos(pos: (Int, Int)): Boolean = validPos(pos._1,pos._2)
  def validPos(rnd: Int, mano: Int): Boolean = {
    (rnd >= 0 & rnd <= rnds & mano > 0 & mano <= scala.math.pow(2,scala.math.max(rnd-1,0)).toInt) 
  }

  def init(pls: Array[ParticipantEntry]): Boolean = {
    if (pls.length == size) {
      for (i <- 0 to size-1) {
        pants(i) = pls(i)
        sno2pos += (pls(i).sno -> i) 
      }
      /* init results with positions
      var index = 0 
      for (r <- rnds to 0 by -1; g <- 1 to scala.math.pow(2,scala.math.max(r-1,0)).toInt) {
        results(index).pos = (r,g)
        index = index + 1   
      } */
      true
    } else {
      false
    }
  }

  override def toString() = {
    val str = new StringBuilder(s"  KO-Runde size:${size} rounds:${rnds} noWinSets:${noWinSets}\n")
    
    calc
    str.append(s"    Player size:${pants.size}\n")
    for (pe <- pants) str.append(s"     - SNO:${pe.sno} ${pe.name}(${pe.club}) place(${pe.place._1}/${pe.place._2}) \n")
    str.append(s"    Results size:${results.size}\n")
    for (re <- results) str.append(s"     - R:${re.pos._1} M:${re.pos._2} ${re.sno._1}-${re.sno._2}: (${re.sets._1}:${re.sets._2}) ${re.balls.mkString(",")}\n")
    str.toString
  }

  // toTx convert KoRound to transfer representation
  def toTx(): KoRoundTx = {
    val koResults = if (size > 0) results.toList else List[ResultEntry]()
    val koPants = if (size > 0) pants.toList else List[ParticipantEntry]()
    KoRoundTx(size, rnds, name, noWinSets, koPants, koResults)
  }

  def setResultEntries(reEntries: Seq[ResultEntry]) = {
    for (i <-0 to size-1) results(i).valid = false 
    for (result <- reEntries) {
      val index = getIndex(result.pos._1, result.pos._2)
      if (result.valid & index >= 0 & index < scala.math.pow(2,rnds).toInt) {
        results(index) = result
      }
    }
    calc
  }


  def calc() = {
    for (i <- 0 to size-1) pants(i).place = KoRound.getPlace(rnds, false)
    for (res <- results) {
      if (res.valid & validSets(res.sets, noWinSets) & sno2pos.isDefinedAt(res.sno._1) & sno2pos.isDefinedAt(res.sno._2)) {
        pants(sno2pos(res.sno._1)).place = KoRound.getPlace(res.pos._1, res.sets._1 == noWinSets)
        pants(sno2pos(res.sno._2)).place = KoRound.getPlace(res.pos._1, res.sets._2 == noWinSets)
      }
    }
  }

  def setMatch(m: MEntryKo): Boolean = {
    val rEntry = ResultEntry.fromMatchEntry(m, CPT_KO, noWinSets)
    if (validPos(rEntry.pos)) {
      results(getIndex(rEntry.pos)) = rEntry
      true
    } else {
      false
    }
  }

  def setMatch(m: MEntryKo, prt: (String)=>Unit): Boolean = {
    val rEntry = ResultEntry.fromMatchEntry(m, CPT_KO, noWinSets)
    if (validPos(rEntry.pos)) {
      prt(s"addkoMatch: index=${getIndex(rEntry.pos)} rEntry=${rEntry}")
      results(getIndex(rEntry.pos)) = rEntry
      true
    } else {
      false
    }
  }
  
  def resetMatch(): Unit = {
    for (result <- results) result.valid = false
    calc
  }

}


object KoRound {
  def getNoRounds(noPlayers: Int): Int = {
    noPlayers match {  
      case a if   65 to 128 contains a => 7
      case b if   33 to  64 contains b => 6
      case c if   17 to  32 contains c => 5
      case d if    9 to  16 contains d => 4
      case e if    5 to   8 contains e => 3
      case f if    3 to   4 contains f => 2
      case g if    1 to   2 contains g => 1
      case _                           => 0
    }
  }

  def getPlace(rnd: Int, win: Boolean): (Int,Int) = {
    rnd match {
      case 7   => if (win) (33,64) else (65,128)
      case 6   => if (win) (17,32) else (33,64)
      case 5   => if (win) (9,16) else (17,32)
      case 4   => if (win) (5,8) else (9,16)
      case 3   => if (win) (3,4) else (5,8)
      case 2   => if (win) (1,2) else (3,4)
      case 1   => if (win) (1,0) else (2,0)
      case 0   => (1,0)
      case -1  => if (win) (3,0) else(4,0)
    }
  } 

  def fromTx(tx: KoRoundTx): KoRound = {
    val kr = new KoRound(tx.size, tx.rnds, tx.name, tx.noWinSets)

    // add players
    for ((plentry, count) <- tx.pants.zipWithIndex) {
      if (count < tx.pants.length) {
        kr.pants(count) = plentry
        kr.sno2pos += (kr.pants(count).sno -> count) 
      }  
    }

    // add matches
    for (result <- tx.results) {
      val resEntry = result
      val index = kr.getIndex(resEntry.pos._1, resEntry.pos._2)

      // calcualte position within array from the result entry
      // results of ko tree are linearized/flattend 
      if (resEntry.valid & index >= 0 & index < scala.math.pow(2,kr.rnds).toInt) {
        kr.results(index) = resEntry
      }
    }
    kr.calc
    kr
  }
}


// KoRound transfer representation
case class KoRoundTx (
  val size:      Int, 
  val rnds:      Int,
  val name:      String,
  val noWinSets: Int,
  val pants:     List[ParticipantEntry],
  val results:   List[ResultEntry]


  // var players:   List[ParticipantEntry] = List[ParticipantEntry](),
  // var results:   List[ResultEntry] = List[ResultEntry]()
)

object KoRoundTx {
  implicit def rw: RW[KoRoundTx] = macroRW
}
