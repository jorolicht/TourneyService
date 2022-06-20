package shared.model.tabletennis

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.Routines._
import shared.model.MEntryGr
import shared.model.ParticipantEntry
import shared.model.tabletennis.utility._


case class GroupEntry(
  var valid:    Boolean, 
  var points:   (Int,Int), 
  var sets:     (Int,Int), 
  var ballDiff: (Int,Int),
  var balls:    Array[String]
) {
  
  def toResultEntry(pos: (Int, Int), sno: (String, String)) =  ResultEntry(valid, pos, sno, sets, balls)
  def invert = new GroupEntry(valid,(points._2,points._1),(sets._2,sets._1),(ballDiff._2,ballDiff._1),balls.map(invBall(_)))
}

object GroupEntry {

  def apply(valid: Boolean): GroupEntry =  GroupEntry(false,(0,0),(0,0),(0,0),Array[String]())

  def fromResultEntry(re: ResultEntry, noSets: Int): GroupEntry = {
    if (re.valid) {
      GroupEntry(true, getPoints(re.sets, noSets), re.sets, getBalls(re.balls, noSets), re.balls)
    } else {
      GroupEntry(false)
    }  
  }

  def obify(gREStr: String): GroupEntry = {
    val g = gREStr.split(";")
    try { 
      if (g.length > 7) {
        GroupEntry(g(0).toBoolean, (g(1).toInt,g(2).toInt), (g(3).toInt,g(4).toInt), (g(5).toInt,g(6).toInt), g(7).split('·'))
      } else {
        GroupEntry(g(0).toBoolean, (g(1).toInt,g(2).toInt), (g(3).toInt,g(4).toInt), (g(5).toInt,g(6).toInt), Array[String]())
      }
    } catch { case _: Throwable => GroupEntry(false) }
  }
}


/*
 * Group Definition for Table Tennis
 * val tup2     = """\((\d+),(\d+)\)""".r 
 */
class Group(val grId: Int, val size: Int, quali: Int, val name: String, noWinSets: Int) {
  var pants      = Array.fill[ParticipantEntry](size) (ParticipantEntry("0", "", "", 0, (0,0)))                      
  val results    = Array.fill[GroupEntry](size, size) (GroupEntry(false, (0,0), (0,0), (0,0), Array("")))
  var points     = Array.ofDim[(Int, Int)](size)
  var sets       = Array.ofDim[(Int, Int)](size)
  var balls      = Array.ofDim[(Int, Int)](size) 

  def init(pls: Array[ParticipantEntry]): Boolean = {
    if (pls.length == size) {
      for (i <- 0 to size-1) pants(i) = pls(i) 
      true
    } else {
      false
    }
  }

  override def toString() = {
    val str = new StringBuilder(s"  Group ${name} (Id:${grId}/Size:${size}/Quali:${quali}) WinSets: ${noWinSets}\n")
    str.append("    Player\n")
    for (pe <- pants) {
      str.append(s"     - ${pe.sno} ${pe.name}(${pe.club}) \n")
    }
    
    calc()
    str.append("    Table\n")  
    for (i <- 0 to size-1; j <- 0 to size-1) {
      if (j == 0) str.append(s"      (${i+1})")
      if (i == j) str.append(" X ") else str.append(s"${results(i)(j).sets._1}:${results(i)(j).sets._2} ")
      if (j == size-1) str.append(s" | ${balls(i)._1}:${balls(i)._2} | ${sets(i)._1}:${sets(i)._2} | ${points(i)._1}:${points(i)._2}\n")
    }
    str.append("\n")
    str.toString
  }

  // convert group to transfer representation
  def toTx(): GroupTx = {
    val grtx = GroupTx(name, grId, size, quali, noWinSets)
    for (i <-0 to size-1; j <- i+1 to size-1) {
      grtx.results = grtx.results :+ results(i)(j).toResultEntry((i+1,j+1),(pants(i).sno,pants(j).sno))
    }
    grtx.pants = pants.toList
    grtx
  }


  def setMatch(m: MEntryGr): Boolean = { 
    val balls     = m.result.split('·')
    val sets      = getSets(balls, noWinSets)

    if (m.wgw._1 < 1 | m.wgw._1 > size | m.wgw._2 < 1 | m.wgw._2 > size) {
      false 
    } else if (m.status >= 2 & validSets(sets, noWinSets)) {
      results(m.wgw._1-1)(m.wgw._2-1).valid    = true
      results(m.wgw._1-1)(m.wgw._2-1).balls    = balls
      results(m.wgw._1-1)(m.wgw._2-1).sets     = sets
      results(m.wgw._1-1)(m.wgw._2-1).points   = getPoints(sets, noWinSets)
      results(m.wgw._1-1)(m.wgw._2-1).ballDiff = getBalls(balls, noWinSets)
      results(m.wgw._2-1)(m.wgw._1-1) = results(m.wgw._1-1)(m.wgw._2-1).invert
      true
    } else {
      results(m.wgw._1-1)(m.wgw._2-1).valid = false
      results(m.wgw._2-1)(m.wgw._1-1).valid = false   
      true
    }
  } 

  def resetMatch(): Unit = {
    for (i <- 0 to size-1; j <- 0 to size-1; if (j != i) ) results(i)(j).valid = false
    calc
  }


  // calc position of pants based on points, sets and ball difference
  def calc() = {
    def sumPoints(pos: Int): (Int,Int) = { 
      var sum = (0,0); for ( i <- 0 to size-1) if (results(pos)(i).valid) sum = sum + results(pos)(i).points; sum
    }
    def sumSets(pos:Int): (Int,Int) = {
      var sum = (0,0); for ( i <- 0 to size-1)  if (results(pos)(i).valid) sum = sum + results(pos)(i).sets; sum
    }
    def sumBallDiffs(pos: Int): (Int,Int) = {
      var sum = (0,0); for ( i <- 0 to size-1)  if (results(pos)(i).valid) sum = sum + results(pos)(i).ballDiff; sum
    }

    var tmpPos  = Array.ofDim[(Int,Long)](size)
    for (i <- 0 to size-1) {
      balls(i)  = sumBallDiffs(i)
      sets(i)   = sumSets(i)
      points(i) = sumPoints(i)
      tmpPos(i)    = (i, (balls(i)._1 - balls(i)._2) + 2000L +  ((sets(i)._1 - sets(i)._2) + 50) * 10000 +  ((points(i)._1 - points(i)._2) + 50) * 10000000)
    }
    tmpPos = tmpPos.sortBy(_._2).reverse
    var cnt = 1 
    pants(tmpPos(0)._1).place = (cnt,0)
    for (i <- 1 to size-1) { 
      if (tmpPos(i)._2 < tmpPos(i-1)._2) { cnt = cnt + 1 }
      pants(tmpPos(i)._1).place = (cnt,0) 
    }
  }

  def getResultEntrys(): Seq[ResultEntry] = {
    var resList:   List[ResultEntry] = List[ResultEntry]()
    for (i <-0 to size-1; j <- i+1 to size-1) {
      resList = resList :+ results(i)(j).toResultEntry( (i+1,j+1) , (pants(i).sno, pants(j).sno) )
    }
    resList.toSeq
  }

  def setResultEntries(reEntries: Seq[ResultEntry]) = {
    for (i <- 0 to size-1; j <- 0 to size-1; if (j != i) ) results(i)(j).valid = false
    for (result  <- reEntries) {
      if (result.valid & result.pos._1 > 0 & result.pos._2 > 0 & result.pos._1 <= size & result.pos._2 <= size) {
        results(result.pos._1-1)(result.pos._2-1) = GroupEntry.fromResultEntry(result, noWinSets)
        results(result.pos._2-1)(result.pos._1-1) = results(result.pos._1-1)(result.pos._2-1).invert
      } 
    }
    calc
  }
}

object Group {
  def getNoRounds(size: Int): Int = {
    if (size % 2 != 1 ) size - 1 else size
  } 

  def getName(grNo: Int, noGroups: Int) = {
    val grId = grNo-1
    if      (noGroups <  grNo)   {  s"Gr${grNo}"                     }
    else if (noGroups <= 26  )   {  abc(grId).toString               } 
    else if (noGroups <= 52  )   {  s"${abc(grId/2)}${(grId % 2)+1}" }
    else                         {  s"Gr${grNo}"                     }
  }

  def fromTx(grtx: GroupTx) = {
    val gr = new Group(grtx.grId, grtx.size, grtx.quali, grtx.name, grtx.noWinSets)

    // add pants
    for ((plentry, count) <- grtx.pants.zipWithIndex) {
      if (count < grtx.pants.length) gr.pants(count) = plentry
    }

    // add matches
    for (resEntry <- grtx.results) {
      if (resEntry.valid & resEntry.pos._1 > 0 & resEntry.pos._2 > 0 & resEntry.pos._1 <= grtx.size & resEntry.pos._2 <= grtx.size) {
        gr.results(resEntry.pos._1-1)(resEntry.pos._2-1) = GroupEntry.fromResultEntry(resEntry, grtx.noWinSets)
        gr.results(resEntry.pos._2-1)(resEntry.pos._1-1) = gr.results(resEntry.pos._1-1)(resEntry.pos._2-1).invert
      } 
    }
    gr.calc
    gr
  }
}


// Group transfer representation
case class GroupTx (
  val name:      String,
  val grId:      Int, 
  val size:      Int, 
  val quali:     Int, 
  val noWinSets: Int,
  var pants:     List[ParticipantEntry] = List[ParticipantEntry](),
  var results:   List[ResultEntry] = List[ResultEntry]()
) 

object GroupTx  {
  implicit def rw: RW[GroupTx] = macroRW
}