package shared.model

import scala.util.matching
import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils._
import shared.utils.Constants._
import shared.utils.Routines._
import shared.model.tabletennis._

/** CompPhase describes a phase of a competition like first round
 *  intermediate round or final round. Every competition has at least
 *  a final round. Currently only two typs are supported ("group" or "ko") 
 *  number of players always less or equal size
 */

case class CompPhase(val name: String, val coId: Long, val coPh: Int, 
                     val coPhId: Int,  val coPhTyp: Int,
                     val status: Int,  var enabled: Boolean, 
                     var size: Int,    var noPlayers: Int, noWinSets: Int = 3) {
  import CompPhase._
  
  var pants        = ArrayBuffer[SNO]()        // participant list (start numbers)
  var matches      = ArrayBuffer[MEntry]()
  var groups       = ArrayBuffer[Group]()      // groups of the competition (only gr rounds)
  var ko           = new KoRound(0, 0, "", 0)  // ko games of ghe competition (only ko rounds)

  def encode = write[CompPhaseTx](toTx())

  // distribute player to the groups (no,size,quali)
  def init(plList: Array[ParticipantEntry], grpCfg: List[(Int,Int,Int)] = List()): Boolean = {
    coPhTyp match { 
      case CPT_GR => {
        val noGroups = grpCfg.foldLeft((0,0,0))( (a:Tuple3[Int,Int,Int],b:Tuple3[Int,Int,Int]) => (a._1 + b._1,0,0))._1
        var grNo = 1
        var plId = 0
        for (grpCfgEntry <- grpCfg) {        
          for(grCnt <- 1 to grpCfgEntry._1) {
            groups = groups :+ new Group(grNo, grpCfgEntry._2, grpCfgEntry._3, Group.getName(grNo, noGroups), noWinSets )
            groups(grNo-1).init(plList.slice(plId, plId + grpCfgEntry._2))
            plId = plId + grpCfgEntry._2
            grNo = grNo + 1
          }   
        }
        true
      }  
      case CPT_KO => {
        ko = new KoRound(size, KoRound.getNoRounds(noPlayers), name, noWinSets)
        ko.init(plList)
        true
      }  
      case _      => false
    }
  }  

  def getMaxRnds(): Int = {
    coPhTyp match {
      case CPT_GR => if (groups(0).size % 2 == 0) groups(0).size - 1 else groups(0).size
      case CPT_KO => { 
        size match {
          case 2                          =>   1
          case x if (3  <= x && x <= 4)   =>   2
          case x if (5  <= x && x <= 8)   =>   3
          case x if (9  <= x && x <= 16)  =>   4
          case x if (17 <= x && x <= 32)  =>   5
          case x if (33 <= x && x <= 64)  =>   6
          case x if (65 <= x && x <= 128) =>   7
          case _                          =>  -1
        }
      }
      case _      => 0
    }  
  }


  def gameNoExists(gameNo: Int): Boolean =  (gameNo>0) && (gameNo <= matches.size)

  def depFinished(gameNo: Int, coPhTyp: Int): Boolean = {
    coPhTyp match {
      case CPT_GR => {
        val depend = matches(gameNo-1).asInstanceOf[MEntryGr].getDepend 
        // check if any dependend match is not yet finished
        // set new status based on dependend matches are finished
        val depFinished = depend.map(g => if (gameNoExists(g)) matches(g-1).finished else true) 
        !depFinished.contains(false)
      }
      case _     => true
    }      
  }

  def resetMatches(): Unit = {
    for (i<-0 to matches.length-1) matches(i).reset()
    coPhTyp match {
      case CPT_GR => for (i <- 0 to groups.size-1) groups(i).resetMatch
      case CPT_KO => ko.resetMatch()
      case _      => // do some error handling?
    }
  }


  // setMatch 
  def setMatch(m: MEntry): Unit = {  
    matches(m.gameNo-1) = m
    m.coPhTyp match {
      case CPT_GR => if (m.asInstanceOf[MEntryGr].grId > 0 & m.asInstanceOf[MEntryGr].grId <= groups.length) {
        groups(m.asInstanceOf[MEntryGr].grId-1).setMatch(m.asInstanceOf[MEntryGr])
      }
      case CPT_KO => ko.setMatch(m.asInstanceOf[MEntryKo])
 
      case _      => ()
    }
  }
  
  def setMatchPropagate(gameNo: Int, sets: (Int,Int), result: String, info: String, playfield: String): List[Int] = { 
    import scala.collection.mutable.ListBuffer
    
    val triggerList = ListBuffer[Int](gameNo)
    val m = getMatch(gameNo)
    m.setSets(sets)
    m.setResult(result)
    m.setInfo(info)
    m.setPlayfield(playfield)
    m.setStatus(depFinished(gameNo, m.coPhTyp))
    setMatch(m)

    // propagate changes to dependend matches
    // set trigger list for relevant matches
    m.coPhTyp match {

      case CPT_GR => {
        // set status for every match to be triggered
        val trigger = m.asInstanceOf[MEntryGr].getTrigger
        for (g <- trigger) { 
          setMatch(getMatch(g).setStatus(depFinished(g, m.coPhTyp)))
          triggerList.append(g) 
        }
      }

      case CPT_KO => {
        val (gWin, pWin) = m.asInstanceOf[MEntryKo].getWinPos
        // propagate winner
        if (gameNoExists(gWin) && m.finished) { 
          setMatch(getMatch(gWin).setPant(pWin, m.getWinner).setStatus(true))
          triggerList.append(gWin)
        }  
        // propagate looser i.e. 3rd place match
        val (gLoo, pLoo) = m.asInstanceOf[MEntryKo].getLooPos
        if (gameNoExists(gLoo) && m.finished) {
          setMatch(getMatch(gLoo).setPant(pLoo, m.getLooser).setStatus(true))
          triggerList.append(gLoo)
        }
      }
    }
    triggerList.toList
  }


  def getMatch(game: Int): MEntry = {
    coPhTyp match {
      case CPT_GR => matches(game-1).asInstanceOf[MEntryGr]
      case CPT_KO => matches(game-1).asInstanceOf[MEntryKo]
      case _      => matches(game-1).asInstanceOf[MEntryBase]
    }    
  }
  
  def resetMatch(gameNo: Int): Unit = {
    matches(gameNo-1).reset()
    setMatch(matches(gameNo-1))
  }

  def resetAllMatches(): List[Int] = {
    val triggerList = scala.collection.mutable.ListBuffer[Int]()
    val maxRnd = getMaxRnds()

    coPhTyp match {
      case CPT_KO => {
        // val mList = (for (m <- matches) yield { if (m.status == MEntry.MS_FIN && (m.round == maxRnd || m.round == (maxRnd-1))) m.gameNo else 0 }).filter(_ != 0)
        // mList.distinct.sorted foreach { g => triggerList ++= resetMatchPropagate(g) } 
        for (i<-0 to matches.length-1) {
          if (matches(i).status == MEntry.MS_FIN) {
            triggerList ++= resetMatchPropagate(matches(i).gameNo)
          }          
        }
      }
      case CPT_GR => {

      }
    }
    triggerList.distinct.sorted.toList
  }


  def resetMatchPropagate(gameNo: Int, resetPantA: Boolean=false, resetPantB: Boolean=false): List[Int] = {
    import scala.collection.mutable.ListBuffer

    val triggerList = ListBuffer[Int](gameNo)
    val m = getMatch(gameNo)
    m.reset(resetPantA, resetPantB)
    
    m.coPhTyp match {
      case CPT_GR => {
        println(s"Match 1: ${m}")

        setMatch(m.setStatus(depFinished(gameNo, m.coPhTyp)))
        println(s"Match 2: ${matches(m.gameNo-1)}")
        // set status for every match to be triggered
        val trigger = m.asInstanceOf[MEntryGr].getTrigger
        for (g <- trigger) { 
          setMatch(getMatch(g).setStatus(depFinished(g, m.coPhTyp)))
          triggerList.append(g)
        }  
      }

      case CPT_KO => {
        setMatch(m.setStatus(true))      
        // propagate deletion of that position
        val (gWin, pWin) = m.asInstanceOf[MEntryKo].getWinPos
        println(s"propagate winner gameNo: ${gWin} position: ${pWin}")
        if (gameNoExists(gWin)) { triggerList ++= resetMatchPropagate(gWin, pWin==0, pWin==1 ) }  

        // propagate looser i.e. 3rd place match
        val (gLoo, pLoo) = m.asInstanceOf[MEntryKo].getLooPos
        println(s"propagate looser gameNo: ${gLoo} position: ${pWin}")
        if (gameNoExists(gLoo)) { triggerList ++= resetMatchPropagate(gLoo, pLoo==0, pLoo==1 ) } 
      }
    }
    triggerList.toList
  } 


  // calculate players position within competition phase
  def calcModel(grId: Int = -1): Boolean = {
    coPhTyp match {
      case CPT_GR => grId match {
         case -1 =>  for (g <- 0 to groups.length-1) { groups(g).calc }; true 
         case g if (g > 0 && g <= groups.length) =>  groups(grId-1).calc; true
         case _ => false
      }
        // if (-1 == grId) { for (g <- 0 to groups.length-1) { groups(g).calc }; true } 
        // else { if (grId > 0 & grId <= groups.length) { groups(grId-1).calc; true } else { false } }  
      case CPT_KO => ko.calc; true
      case _      => false
    } 
  }  


  // print readable competition phase - for debug purposes
  override def toString() = {
    def grPhName(coPh: Int) = {
      coPh match {
        case CP_INIT => "READY"        
        case CP_VRGR => "GRUPPEN-VORRUNDE"
        case CP_ZRGR => "GRUPPEN-ZWISCHENRUNDE"
        case CP_ERGR => "GRUPPEN-ENDRUNDE"
        case CP_TRGR => "GRUPPEN-TROSTRUNDE"
        case CP_ERKO => "KO-ENDRUNDE"
        case CP_TRKO => "KO-TROSTRUNDE"
        case _       => "UNKNOWN"
      }
    }

    def pants2Str() = {
      val str = new StringBuilder("-- PARTICIPANTS\n")
      for { p <- pants }  yield { str ++= s"${p.value}:" } 
      str.toString
    }
    def matches2Str() = {
      val str = new StringBuilder("-- MATCHES\n")
      for { m <- matches }  yield { 
        str ++= m.toString + "\n"
      } 
      str.toString
    }

    val str   = new StringBuilder(s"${name} (coId:${coId}) ${grPhName(coPh)}(${coPh}) Typ:${coPhTyp}\n")
    str.append(s"${pants2Str()}\n")   
    str.append(s"${matches2Str()}\n") 

    coPhTyp match {
      case CPT_GR => for (i <- 0 to groups.length-1) str.append(groups(i).toString)
      case CPT_KO => str.append(s"${ko.toString}")
      case _      => str.append(s"UNKNOWN COMPETITION PHASE")
    }
    str.toString
  }  

  def getWinPhId(): Int = ???
  def getLooPhId(): Int = ???
  def toTx(): CompPhaseTx = {
    CompPhaseTx(name, coId, coPh, coPhId, coPhTyp, status, enabled, size, noPlayers, noWinSets, 
                pants.map(x=>x.value), matches.map(x=>x.toTx), groups.map(g=>g.toTx), ko.toTx) 
  }

}


object CompPhase {
  // Competition Phase
  val CP_UNKN = -99
  val CP_INIT =   0
  val CP_VRGR =   1
  val CP_ZRGR =   2
  val CP_ERGR =   3
  val CP_TRGR =   4
  val CP_ERBO =   3     // Endrunde: KO oder Gruppe
  val CP_TRBO =   4     // Trostrunde: KO oder Gruppe
  val CP_LEER =   5     // LEER
  val CP_VRKO =   6     // not yet available
  val CP_ZRKO =   7     // not yet available
  val CP_ERKO =   8     // only final KO round
  val CP_TRKO =   9     // nur Trostrunde KO

  // Competition Phase Type
  val CPT_UNKN = 0
  val CPT_GR   = 1  // Group Phase
  val CPT_KO   = 2  // KO Phase
  val CPT_SW   = 3  // Switzsystem

  // Competition Phase Status
  val CPS_UNKN = -1  // Unknown - eg. competition phase does yet not exist
  val CPS_CFG  = 0   // RESET
  val CPS_AUS  = 1   // Auslosung der Vorrunde, Zwischenrunde, Endrunde, Trostrunde
  val CPS_EIN  = 2   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CPS_FIN  = 3   // Runde/Phase beendet, Auslosung ZR oder ER kann erfolgen

  implicit val rw: RW[CompPhase] = upickle.default.readwriter[String].bimap[CompPhase](
    x   => write[CompPhaseTx](x.toTx()),   //s"""{ "name", "Hugo" } """,  
    str => fromTx(read[CompPhaseTx](str))
  )

  def decode(coPhStr: String): Either[Error, CompPhase] = 
    try Right(fromTx(read[CompPhaseTx](coPhStr)))
    catch { case _: Throwable => Left(Error("err0177.decode.CompPhase", coPhStr.take(20))) }

  def fromTx(tx: CompPhaseTx): CompPhase = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashSet
    import shared.model.Competition._

    val cop = new CompPhase(tx.name, tx.coId, tx.coPh, tx.coPhId, tx.coPhTyp, tx.status, tx.enabled, tx.size, tx.noPlayers, tx.noWinSets) 
    cop.pants   = tx.pants.map(x=>SNO(x))
    cop.matches = tx.matches.map(x=>x.decode)

    cop.coPhTyp match {
      case CPT_GR  => {
        for (g <- tx.groups) { cop.groups = cop.groups :+ Group.fromTx(g) }
        // hook for calculation trigger and depend, if not present
        if (cop.matches.size > 0 && !cop.matches(0).asInstanceOf[MEntryGr].hasDepend) {
          // calculate depend and trigger
          val depMap = Map[Long, ListBuffer[Int]]()
          // init map with default value
          for (p <- cop.pants) if (SNO.valid(p.value)) { depMap(SNO.plId(p.value)) = ListBuffer() }

          // setup list player -> games
          for (m <- cop.matches) {
            m.coTyp match {
              case CT_SINGLE => {
                if (SNO.valid(m.stNoA)) { (depMap(SNO.plId(m.stNoA))) += m.gameNo }
                if (SNO.valid(m.stNoB)) { (depMap(SNO.plId(m.stNoB))) += m.gameNo }
              }
              case CT_DOUBLE => {
                val idAs = getMDLongArr(m.stNoA)
                val idBs = getMDLongArr(m.stNoB)
                if (idAs.size>=1 && SNO.valid(idAs(0))) { (depMap(idAs(0))) += m.gameNo }
                if (idAs.size>=2 && SNO.valid(idAs(1))) { (depMap(idAs(1))) += m.gameNo }                   
                if (idBs.size>=1 && SNO.valid(idBs(0))) { (depMap(idBs(0))) += m.gameNo }
                if (idBs.size>=2 && SNO.valid(idBs(1))) { (depMap(idBs(1))) += m.gameNo }    
              }
            }   
          }

          // calculate depend, split depend on gameNo
          for (m <- cop.matches) {
            val plA:(ListBuffer[Int], ListBuffer[Int])  = if (SNO.valid(m.stNoA)) {
              depMap(SNO.plId(m.stNoA)).partition(_ <= m.gameNo)
            } else { (ListBuffer(), ListBuffer()) }
            val plB:(ListBuffer[Int], ListBuffer[Int])  = if (SNO.valid(m.stNoB)) {
              depMap(SNO.plId(m.stNoB)).partition(_ <= m.gameNo)
            } else { (ListBuffer(), ListBuffer()) }
            val depend = HashSet[Int]()
            val trigger = HashSet[Int]()
          
            if (plA._2.size > 0) trigger += (plA._2).sorted.head
            if (plB._2.size > 0) trigger += (plB._2).sorted.head
            if (plA._1.size > 1) depend += plA._1.sorted.reverse(1)
            if (plB._1.size > 1) depend += plB._1.sorted.reverse(1)
            cop.matches(m.gameNo-1).asInstanceOf[MEntryGr].depend  = depend.mkString("·")
            cop.matches(m.gameNo-1).asInstanceOf[MEntryGr].trigger = trigger.mkString("·")
            // println(s"GAME NUMBER: ${m.gameNo}")
            // println(s"dependA: ${depMap(SNO.plId(m.stNoA)).toString} plA: ${plA._1.toString}  ${plA._2.toString}")
            // println(s"dependB: ${depMap(SNO.plId(m.stNoB)).toString} plB: ${plB._1.toString}  ${plB._2.toString}")
            // println(s"DEPEND: ${depend}")
            // println(s"TRIGGER: ${trigger}")            
          }
        }
      }  
      case CPT_KO  => {
        cop.ko = KoRound.fromTx(tx.ko)
      }   
      case _       => // invalid competition phase
    }
    cop
  }
}

//
// Transfer representation of a competition phase
//
case class CompPhaseTx(
  val name:      String, 
  val coId:      Long, 
  val coPh:      Int, 
  val coPhId:    Int, 
  val coPhTyp:   Int,
  val status:    Int,
  var enabled:   Boolean, 
  var size:      Int, 
  val noPlayers: Int,
  val noWinSets: Int,
  val pants:     ArrayBuffer[String],
  val matches:   ArrayBuffer[MEntryTx], 
  val groups:    ArrayBuffer[GroupTx],
  val ko:        KoRoundTx
)

object CompPhaseTx {
  implicit def rw: RW[CompPhaseTx] = macroRW 
}