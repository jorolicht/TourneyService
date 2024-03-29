package shared.model

import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}


import shared.utils.Routines._
import shared.model.MEntryGr
import shared.model.Pant
import shared.model.Utility._
import shared.utils.Error


case class GroupConfig(id: Int, name: String, size: Int, quali: Int, pos: Int)

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

  def fromResultEntry(re: ResultEntry, noSets: Int): Either[Error, GroupEntry] = 
    try Right( if   (re.valid) GroupEntry(true, getPoints(re.sets, noSets), re.sets, getBalls(re.balls, noSets), re.balls)
               else            GroupEntry(false))
    catch { case _: Throwable => Left(Error("GroupEntry.fromResultEntry")) }
}


/*
 * Group Definition for Table Tennis
 * val tup2     = """\((\d+),(\d+)\)""".r 
 */
class Group(val grId: Int, val size: Int, val quali: Int, val name: String, noWinSets: Int) {
  var pants      = Array.fill[Pant](size) (Pant("0", "", "", 0, "", (0,0)))                      
  val results    = Array.fill[GroupEntry](size, size) (GroupEntry(false, (0,0), (0,0), (0,0), Array("")))
  var points     = Array.fill[(Int, Int)](size) ((0,0))
  var sets       = Array.fill[(Int, Int)](size) ((0,0))
  var balls      = Array.fill[(Int, Int)](size) ((0,0))

  // var points     = Array.ofDim[(Int, Int)](size)
  // var sets       = Array.ofDim[(Int, Int)](size)
  // var balls      = Array.ofDim[(Int, Int)](size) 

  // helper info
  var drawPos       = 0        // start position of group for draw 
  var fillCnt: Int  = 0 
  var avgRating:Int = 0
  var occu: Map[String, Int] = Map[String, Int]().withDefaultValue(0)

  // add participant
  def addPant(pant: Pant, avgPantRating: Int) = {
    pants(fillCnt) = pant
    fillCnt = fillCnt +  1
    if (pant.club != "") occu(pant.club) = occu(pant.club) + 1
    val (sum, pantCnt) = pants.foldLeft((0,0))((a, e) => if (e.rating == 0) (a._1 + avgPantRating, a._2+1) else (a._1 + e.rating, a._2+1) )
    avgRating = sum/pantCnt
  }

  def genOccuRating() = {
    val (sum, pantCnt) = pants.foldLeft((0,0))((a, e) => if (e.rating != 0) (a._1 + e.rating, a._2+1) else (a._1, a._2) )
    if (pantCnt > 0) avgRating = sum/pantCnt
    for (pant <- pants) { if (pant.club != "") occu(pant.club) = occu(pant.club) + 1 }
  }

  def init(pls: List[Pant]): Boolean = if (pls.length == size) { pants = pls.toArray; true } else false

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
    grtx.pants = pants
    grtx
  }


  def setMatch(m: MEntryGr): Either[shared.utils.Error, Boolean] = { 
    import shared.model.MEntry._
    import shared.utils.Error

    try {
      val balls  = if (m.result=="") new Array[String](0) else m.result.split('·')
      //println(s"INFO: setMatch ${m.toString()}")
      if (m.wgw._1 < 1 | m.wgw._1 > size | m.wgw._2 < 1 | m.wgw._2 > size) {
        Left(Error("err0225.systemgroup.invalid.whoagainstwho"))
      } else if ((m.status == MS_FIN | m.status == MS_FIX  | m.status == MS_DRAW) & validSets(m.sets, noWinSets)) {
        results(m.wgw._1-1)(m.wgw._2-1).valid    = true
        results(m.wgw._1-1)(m.wgw._2-1).balls    = balls
        results(m.wgw._1-1)(m.wgw._2-1).sets     = m.sets
        results(m.wgw._1-1)(m.wgw._2-1).points   = getPoints(m.sets, noWinSets)
        results(m.wgw._1-1)(m.wgw._2-1).ballDiff = getBalls(balls, noWinSets)
        results(m.wgw._2-1)(m.wgw._1-1) = results(m.wgw._1-1)(m.wgw._2-1).invert
        Right(true)
      } else if (m.result == "" & m.sets == (0,0)) {
        results(m.wgw._1-1)(m.wgw._2-1).valid    = false
        results(m.wgw._1-1)(m.wgw._2-1).balls    = new Array[String](0)
        results(m.wgw._1-1)(m.wgw._2-1).sets     = (0,0)
        results(m.wgw._1-1)(m.wgw._2-1).points   = (0,0)
        results(m.wgw._1-1)(m.wgw._2-1).ballDiff = (0,0) 
        results(m.wgw._2-1)(m.wgw._1-1) = results(m.wgw._1-1)(m.wgw._2-1).invert
        Right(true)
      } else {
        results(m.wgw._1-1)(m.wgw._2-1).valid = false
        results(m.wgw._2-1)(m.wgw._1-1).valid = false   
        Right(false)
      }
    } catch { case _: Throwable => println(s"ERROR Group.setMatch ${m.toString}"); Left(Error("Group_setMatch"))}  
  }


  def resetResult(): Unit = {
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
    try {
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
    } catch { case _: Throwable => s"ERROR Group.calc ${grId}"}

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

        GroupEntry.fromResultEntry(result, noWinSets) match {
          case Left(err)  => println(s"ERROR: ${err.toString}")
          case Right(res) => results(result.pos._1-1)(result.pos._2-1) = res
        }
        results(result.pos._2-1)(result.pos._1-1) = results(result.pos._1-1)(result.pos._2-1).invert
      } 
    }
    calc
  }
}

object Group {
  import scala.collection.mutable.{ ArrayBuffer }
  import shared.model.CompPhase._

  def getNoRounds(size: Int) = if (size % 2 != 1 ) size - 1 else size

  def fromTx(grtx: GroupTx, drawPos: Int = 0): Either[Error, Group] = {
    var error = Error.dummy
    try {
      val gr = new Group(grtx.grId, grtx.size, grtx.quali, grtx.name, grtx.noWinSets)

      // set participants
      gr.pants  = grtx.pants

      // add matches
      for (resEntry <- grtx.results) {
        //println(s"Group from Tx: resEntrys: ${resEntry.toString}")
        if (resEntry.valid & resEntry.pos._1 > 0 & resEntry.pos._2 > 0 & resEntry.pos._1 <= grtx.size & resEntry.pos._2 <= grtx.size) {
          GroupEntry.fromResultEntry(resEntry, grtx.noWinSets) match {
            case Left(err)  => error = err
            case Right(res) => {
              gr.results(resEntry.pos._1-1)(resEntry.pos._2-1) = res
              gr.results(resEntry.pos._2-1)(resEntry.pos._1-1) = gr.results(resEntry.pos._1-1)(resEntry.pos._2-1).invert
            }  
          }
        } 
      }


      gr.drawPos = drawPos
      gr.genOccuRating
      gr.calc

      Right(gr)
    } catch { case _: Throwable => Left(error.add("Group.fromTx")) }
  }


  // def fromTx1(grtx: GroupTx1, drawPos: Int = 0): Either[Error, Group] = {
  //   var error = Error.dummy
  //   try {
  //     val gr = new Group(grtx.grId, grtx.size, grtx.quali, grtx.name, grtx.noWinSets)

  //     // set participants
  //     gr.pants  = grtx.pants.map { _.toPant()}

  //     // add matches
  //     for (resEntry <- grtx.results) {
  //       //println(s"Group from Tx: resEntrys: ${resEntry.toString}")
  //       if (resEntry.valid & resEntry.pos._1 > 0 & resEntry.pos._2 > 0 & resEntry.pos._1 <= grtx.size & resEntry.pos._2 <= grtx.size) {
  //         GroupEntry.fromResultEntry(resEntry, grtx.noWinSets) match {
  //           case Left(err)  => error = err
  //           case Right(res) => {
  //             gr.results(resEntry.pos._1-1)(resEntry.pos._2-1) = res
  //             gr.results(resEntry.pos._2-1)(resEntry.pos._1-1) = gr.results(resEntry.pos._1-1)(resEntry.pos._2-1).invert
  //           }  
  //         }
  //       } 
  //     }


  //     gr.drawPos = drawPos
  //     gr.genOccuRating
  //     gr.calc

  //     Right(gr)
  //   } catch { case _: Throwable => Left(error.add("Group.fromTx")) }
  // }


  /** genGrpSplit - check whether the numbers of players can be
   *  configurated with groups of size and size+1
   */ 
  def genGrpSplit(noPlayers: Int, size: Int): (Int, Int) = {
    if (noPlayers < 2*size) (0,0) else {
      if (noPlayers % (size+1) == 0) {
        (0, noPlayers/(size+1))
      } else {
        if (noPlayers % size == 0) {
          (noPlayers/size, 0)
        } else {
          val x = noPlayers % size
          (noPlayers/size - x, x)
        }
      }
    }
  }


  /** genGrpConfig - generate configuration array for groups
   *  (grName: String, grId: Int, grSize: Int, pos: Int)
   */
  def genGrpConfig(secTyp: CompPhaseCfg.Value, noPlayer: Int): ArrayBuffer[GroupConfig] = {
    var gList:  ArrayBuffer[(Int,Int)] = new ArrayBuffer()
    var result: ArrayBuffer[GroupConfig] = new ArrayBuffer()

    secTyp match {
      case CompPhaseCfg.GRPS3  => { val g3 = noPlayer / 3; gList += ((3, g3)) } 
      case CompPhaseCfg.GRPS34 => { val (g3, g4) = genGrpSplit(noPlayer, 3); gList += ((4, g4)); gList += ((3, g3)) } 
      case CompPhaseCfg.GRPS4  => { val g4 = noPlayer / 4; gList += ((4, g4)) }       
      case CompPhaseCfg.GRPS45 => { val (g4, g5) = genGrpSplit(noPlayer, 4); gList += ((5, g5)); gList += ((4, g4)) }
      case CompPhaseCfg.GRPS5  => { val g5 = noPlayer / 5; gList += ((5, g5)) }  
      case CompPhaseCfg.GRPS56 => { val (g5, g6) = genGrpSplit(noPlayer, 5); gList += ((6, g6)); gList += ((5, g5)) }
      case CompPhaseCfg.GRPS6  => { val g6 = noPlayer / 6; gList += ((6, g6)) }
    }

    var cnt = 1
    var pos = 1
    for (gListEntry <- gList) {
      for (entry <- 1 to gListEntry._2) {
        result += (GroupConfig(cnt, genName(cnt), gListEntry._1, (gListEntry._1+1)/2, pos))
        cnt = cnt + 1
        pos = pos + gListEntry._1
      }
    }
    result
  }


  // genName - generates a name for a group (like excel column name)
  def genName(grpId: Int): String = {
    if (grpId < 1) "?" else {
      var num    = grpId
      var name   = ""
      var modulo = 0

      while (num > 0) {
        modulo = (num - 1) % 26
        name = ('A'.toInt + modulo).toChar + name
        num = (num - modulo) / 26
      }
      name
    }
  }
}


// Group transfer representation
case class GroupTx (
  val name:      String,
  val grId:      Int, 
  val size:      Int, 
  val quali:     Int, 
  val noWinSets: Int,
  var pants:     Array[Pant] = Array[Pant](),
  var results:   Array[ResultEntry] = Array[ResultEntry]()
) 

object GroupTx  {
  implicit def rw: RW[GroupTx] = macroRW
}


// Group transfer representation
case class GroupTx1 (
  val name:      String,
  val grId:      Int, 
  val size:      Int, 
  val quali:     Int, 
  val noWinSets: Int,
  var pants:     Array[Pant1] = Array[Pant1](),
  var results:   Array[ResultEntry] = Array[ResultEntry]()
) 

object GroupTx1  {
  implicit def rw: RW[GroupTx1] = macroRW
}