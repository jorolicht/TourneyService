package shared.model

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils._
import shared.utils.Constants._
import shared.model.CompPhase._
import shared.model.MEntry
import shared.model.PantEntry
import shared.model.Utility._

class KoRound(var size: Int, var name: String, val noWinSets: Int, var rnds: Int = 0) {
  var pants    = ArrayBuffer.fill[PantEntry](size) (PantEntry("0", "", "", 0, (0,0))) 
  var results  = Array.fill[ResultEntry](size) (ResultEntry(false, (0,0), ("0","0"), (0,0), Array[String]() ))
  var sno2pos  = scala.collection.mutable.Map[String, Int]()

  // optional generated info for draw round [GroupName, GroupId, GroupPos/up, RankValue/down]
  var drawInfo = ArrayBuffer[(String, Int, Int, Int)]()

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


  def setDraw(participants: ArrayBuffer[PantEntry], dInfo: ArrayBuffer[(String, Int, Int, Int)]): Either[Error, Int] = {
    if (participants.size != size) {
      Left(Error("???"))
    } else {
      pants    = participants
      drawInfo = dInfo
      Right(size)
    }
  }

  def initDraw(participants: ArrayBuffer[PantEntry], dInfo: ArrayBuffer[(String, Int, Int, Int)]): Either[Error, Int] = {
    def changeUpDown(invert: Boolean, value: Boolean): Boolean = if (invert) !value else value

    size = KoRound.getSize(participants.size)
    rnds = KoRound.getNoRounds(size)

    if (size ==0 || rnds == 0) {
      Left(Error("???"))
    } else {
      // draw => map pants (sorted by results) to ko-tree
      // position of best placed participants follows the following scheme: up/down/down/up/up/down/down
      // position of 2nd best is in the opposite part
      // 0. step initialize up down scheme ULLUULLUULL and
      //    setting positions     
      val upDownScheme = List.fill(size/4)(List(true, false, false, true)).flatten.toArray
      val settingPositions = KoRound.genSettingPositions(size)

      // 1. step generate upDownMap grId/pos -> Up/Down
      //    [GroupName, GroupId, GroupPos, RankValue]
      val upDownMap = HashMap[ (Int,Int), Boolean]()
      val minPos = dInfo.minBy(_._3)._3

      dInfo.zip(upDownScheme).foreach { case (item, updo) => if (item._3 == minPos) upDownMap += ((item._2, minPos) -> updo) } 
      dInfo.zip(upDownScheme).foreach { case (item, updo) => 
        if (!upDownMap.contains((item._2, minPos))) {
          upDownMap += ((item._2, item._3) -> updo); println(s"Error: upDownMap does not contain all values e.g. ${item._2} ${minPos}")  
        } else {
          upDownMap += ( (item._2, item._3) -> changeUpDown( (item._3-minPos)%2 == 1, upDownMap((item._2, minPos))) )   
        }
      } 

      // 2. step generate initial drawing
      //val pantsWithDrawInfo = pants.zip(drawInfo).map(x => (x, upDownMap((x._2._2,x._2._3))))
      val pantsWithDInfo = participants.zip(dInfo)
      pants              = ArrayBuffer.fill(size) (PantEntry.bye("")) 
      drawInfo           = ArrayBuffer.fill(size) ("", 0, 0, 0) 

      // 3. step split pants into up and down positions
      val (upList, downList) = pantsWithDInfo.partition(x => upDownMap((x._2._2,x._2._3))       )
      
      for (i <- 0 until size) {
        val pos = settingPositions(i+1)
        val up  = (pos <= size/2)
        if (up && upList.size > 0)    { pants(pos-1) = upList(0)._1; drawInfo(pos-1) = upList(0)._2; upList.remove(0) }  
        if (!up && downList.size > 0) { pants(pos-1) = downList(0)._1;  drawInfo(pos-1) = downList(0)._2;  downList.remove(0) }
      }

      // Error check, all participants set?
      if (upList.size > 0 || downList.size > 0) {
        Left(Error("???"))
      } else {  
        // initialize sno mapping 
        sno2pos = scala.collection.mutable.Map[String, Int]()
        for (i <- 0 to size-1) sno2pos += (pants(i).sno -> i) 
        Right(size) 
      }
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
    val koPants = if (size > 0) pants.toList else List[PantEntry]()
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

  def setMatch(m: MEntry): Either[shared.utils.Error, Boolean] = {
    val rEntry = ResultEntry.fromMatchEntry(m)
    if (validPos(rEntry.pos)) { 
      results(getIndex(rEntry.pos)) = rEntry
      Right(true)
    } else Right(false)
  }

  def setMatch(m: MEntry, prt: (String)=>Unit): Either[shared.utils.Error, Boolean] = {
    val rEntry = ResultEntry.fromMatchEntry(m)
    if (validPos(rEntry.pos)) {
      prt(s"addkoMatch: index=${getIndex(rEntry.pos)} rEntry=${rEntry}")
      results(getIndex(rEntry.pos)) = rEntry
      Right(true)
    } else {
      Right(false)
    }
  }
  
  def resetMatch(): Unit = {
    for (result <- results) result.valid = false
    calc
  }

}


object KoRound {

  /** genPosField - generate position field              
    *
    * @param posField  4,8,16,32,64, 128 + 1 length 
    * @param lowerBound
    * @param upperBound
    */
  def genPosField(posField: Array[Int], lowerBound: Int, upperBound: Int):Unit = {

    def firstLastSum(baseFieldSize: Int, fieldSize:Int): Int = {
      var value = 3
      var step = 2
      var tmp = baseFieldSize
      while(tmp > fieldSize) {
        value = value + step
        step = 2 * step
        tmp = tmp / 2
      }  
      value
    }  

    val fSize = posField.length - 1
    val curSize = upperBound - lowerBound + 1

    if (posField(lowerBound) == 0 && posField(upperBound) == 0) {
      posField(lowerBound) = 1 // START
      genPosField(posField, lowerBound, upperBound)
    } else if (posField(lowerBound) > 0) {
      posField(upperBound) = firstLastSum(fSize, curSize) - posField(lowerBound)
    } else if (posField(upperBound) > 0) {  
      posField(lowerBound) = firstLastSum(fSize, curSize) - posField(upperBound)
    }

    if (curSize > 2) {
       genPosField(posField, lowerBound,  lowerBound + (curSize / 2) - 1 )
       genPosField(posField, lowerBound + (curSize / 2), upperBound)
    }
  }  

  /** genSettingPositions - generate setting positions for ko-field              
    *                       LUULLUULLUULLUU....
    * @param fSize   (4,8,16,32, ... )
    */  
  def genSettingPositions(fSize: Int): Array[Int] = {
    val posField = Array.fill(fSize+1)(0)
    val setPositions = Array.fill(fSize+1)(0)
    genPosField(posField, 1, fSize)
    for (i<-1 to fSize) { setPositions(posField(i)) = i }
    setPositions 
  }


  // getSize - generates KO-size for number of players
  def getSize(cntPlayer: Int): Int = cntPlayer match {
    case 2                          =>   2
    case x if (3  <= x && x <= 4)   =>   4
    case x if (5  <= x && x <= 8)   =>   8
    case x if (9  <= x && x <= 16)  =>  16
    case x if (17 <= x && x <= 32)  =>  32
    case x if (33 <= x && x <= 64)  =>  64
    case x if (65 <= x && x <= 128) => 128
    case _                          =>   0
  }



  // getNoRounds calculates number of rounds
  def getNoRounds(noPlayers: Int): Int = {
    noPlayers match {  
      case a if 65 to 128 contains a => 7
      case b if 33 to  64 contains b => 6
      case c if 17 to  32 contains c => 5
      case d if  9 to  16 contains d => 4
      case e if  5 to   8 contains e => 3
      case f if  3 to   4 contains f => 2
      case g if  1 to   2 contains g => 1
      case _                         => 0
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

  def getMatchesPerRound(rnd: Int): Int = {
    rnd match { case 7 => 64; case 6 => 32; case 5 => 16; case 4 => 8; case 3 => 4; case 2 => 2; case 1|0 => 1; case _ => 0 }
  } 


  def fromTx(tx: KoRoundTx): KoRound = {
    val kr = new KoRound(tx.size, tx.name, tx.noWinSets, tx.rnds)

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
  val pants:     List[PantEntry],
  val results:   List[ResultEntry]
)

object KoRoundTx {
  implicit def rw: RW[KoRoundTx] = macroRW
}
