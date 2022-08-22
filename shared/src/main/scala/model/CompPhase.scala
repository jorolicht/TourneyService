package shared.model

import scala.util.matching
import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils._
import shared.utils.Constants._
import shared.utils.Routines._

/** CompPhase describes a phase of a competition like first round
 *  intermediate round or final round. Every competition has at least
 *  a final round. Currently only two typs are supported ("group" or "ko") 
 *  number of players always less or equal size
 */

case class CompPhase(val name: String, val coId: Long, val coPhId: Int, val coPhCfg: Int, val coPhTyp: Int,     
                     var status: Int, var enabled: Boolean, var size: Int, var noPlayers: Int, noWinSets: Int = 3) {
  import CompPhase._
  
  var matches      = ArrayBuffer[MEntry]()
  var groups       = ArrayBuffer[Group]()      // groups of the competition (only gr rounds)
  var ko           = new KoRound(0, 0, "", 0)  // ko games of ghe competition (only ko rounds)

  def encode = write[CompPhaseTx](toTx())

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

  def setStatus(value: Int) = { status = value }
  def getStatus             = status

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
      case CPT_GR => {
        val mtch = m.asInstanceOf[MEntryGr]
        if (mtch.grId > 0 & mtch.grId <= groups.length) {
          groups(mtch.grId-1).setMatch(mtch) match {
            case Left(err)  => println(s"Error: set group match: ${err.toString}" )
            case Right(res) => if (res) groups(mtch.grId-1).calc else println("Error: set group match, invalid param")
          }
        } else {
          println("Error: set group match, invalid group Id")
        }
      }  



      case CPT_KO => ko.setMatch(m.asInstanceOf[MEntryKo]) match {
        case Left(err)  => println("Error: set ko match, invalid param")
        case Right(res) => if (res) ko.calc else println("Error: set ko match, invalid param")
      } 
 
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

    coPhTyp match {
      case CPT_KO | CPT_GR => {
        // val mList = (for (m <- matches) yield { if (m.status == MEntry.MS_FIN && (m.round == maxRnd || m.round == (maxRnd-1))) m.gameNo else 0 }).filter(_ != 0)
        // mList.distinct.sorted foreach { g => triggerList ++= resetMatchPropagate(g) } 
        for (i<-0 to matches.length-1) {
          if (matches(i).status == MEntry.MS_FIN || matches(i).status == MEntry.MS_RUN) {
            triggerList ++= resetMatchPropagate(matches(i).gameNo)
          }          
        }
      }
      case _ => {

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

  // target function for descrete optimization
  def getMinOccBestAvg(pant: ParticipantEntry, grps: ArrayBuffer[Group], pantSize: Int, maxRating: Int, maxGrpSize: Int, pantAvgRating: Int): ArrayBuffer[Long] = {
    val result = ArrayBuffer.fill[Long](grps.size)(0)

    for (i <- 0 until grps.size) {
      result(i) = {
        if (grps(i).fillCnt == grps(i).size) {
          0L 
        } else {
          // calculate improvement of average rating 
          val curDiffRating = if (grps(i).avgRating==0) 0 else (pantAvgRating - grps(i).avgRating).abs 
          val newDiffRating = (pantAvgRating - ((grps(i).avgRating * grps(i).fillCnt + pant.effRating) / (grps(i).fillCnt+1))).abs
          val improveRating = maxRating + (curDiffRating - newDiffRating)

          // calculate free level
          val freeGrpLevel = (grps(i).size - grps(i).fillCnt)
          //val freeGrpLevel = (maxGrpSize - grps(i).cnt)

          // calculate occu level
          val occuLevel    = (pantSize - grps(i).occu(pant.club)) 
          println(s"Pant: ${pant} improvementRating: ${improveRating} freeGrpLevel: ${freeGrpLevel } occuLevel: ${occuLevel}")

          ((1000*occuLevel) + freeGrpLevel) * (2 * maxRating) + improveRating 
        }
      }
    }
    result
  }

  // genGrMatchDependencies - generate depend and trigger values
  def genGrMatchDependencies(): Either[Error, Boolean] = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashSet
    import shared.model.Competition._
    try {
      // init map with default value
      val depMap = Map[Long, ListBuffer[Int]]()
      groups.foreach { g => g.pants.foreach { p => if (SNO.valid(p.sno)) { depMap(SNO.plId(p.sno)) = ListBuffer() }}}
      
      // setup list player -> game numbers
      for (m <- matches) {
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
      for (m <- matches) {
        val plA:(ListBuffer[Int], ListBuffer[Int])  = if (SNO.valid(m.stNoA)) {
          depMap(SNO.plId(m.stNoA)).partition(_ <= m.gameNo)
        } else { (ListBuffer(), ListBuffer()) }
        val plB:(ListBuffer[Int], ListBuffer[Int])  = if (SNO.valid(m.stNoB)) {
          depMap(SNO.plId(m.stNoB)).partition(_ <= m.gameNo)
        } else { (ListBuffer(), ListBuffer()) }
        val depend = scala.collection.mutable.HashSet[Int]()
        val trigger = scala.collection.mutable.HashSet[Int]()
      
        if (plA._2.size > 0) trigger += (plA._2).sorted.head
        if (plB._2.size > 0) trigger += (plB._2).sorted.head
        if (plA._1.size > 1) depend += plA._1.sorted.reverse(1)
        if (plB._1.size > 1) depend += plB._1.sorted.reverse(1)
        matches(m.gameNo-1).asInstanceOf[MEntryGr].depend  = depend.mkString("·")
        matches(m.gameNo-1).asInstanceOf[MEntryGr].trigger = trigger.mkString("·")          
      }  
    } catch { case _: Throwable => println("genGrMatchDependencies", s"${this}"); Left(Error("??? genGrMatchDependencies")) }
    Right(true)
  }



  // print readable competition phase - for debug purposes
  override def toString() = {

    def matches2Str() = {
      val str = new StringBuilder("-- MATCHES\n")
      for { m <- matches }  yield { 
        str ++= m.toString + "\n"
      } 
      str.toString
    }

    val str = new StringBuilder(s"${name} (coId:${coId}) coPhId: ${coPhId} coPhCfg: ${CompPhase.cfg2Name(coPhCfg)} typ:${coPhTyp}\n")
    str.append(s"status: ${CompPhase.status2Name(status)} enabled: ${enabled} noPlayers: ${noPlayers} noWinSets: ${noWinSets}\n") 
    str.append(s"${matches2Str()}\n") 

    coPhTyp match {
      case CPT_GR => for (i <- 0 to groups.length-1) str.append(groups(i).toString)
      case CPT_KO => str.append(s"${ko.toString}")
      case _      => str.append(s"UNKNOWN COMPETITION PHASE")
    }
    str.toString
  }  

  def toTx(): CompPhaseTx = {
    CompPhaseTx(name, coId, coPhId, coPhCfg, coPhTyp, status, enabled, size, noPlayers, noWinSets, 
                matches.map(x=>x.toTx), groups.map(g=>g.toTx), ko.toTx) 
  }
}


object CompPhase {
  // Competition Phase Category
  val Category_Start  = 0
  val Category_Winner = 1
  val Category_Looser = 2


  // Competition Phase Type
  val CPT_UNKN = 0
  val CPT_GR   = 1  // Group Phase
  val CPT_KO   = 2  // KO Phase
  val CPT_SW   = 3  // Switzsystem

  // Competition Phase Config (from 0 to 9 old values for compatibility)
  val CPC_UNKN     = -99  
  val CPC_INIT     =   0
  val CPC_VRGR     =   1
  val CPC_ZRGR     =   2
  val CPC_ERGR     =   3
  val CPC_TRGR     =   4
  val CPC_ERBO     =   3   // Endrunde: KO oder Gruppe
  val CPC_TRBO     =   4   // Trostrunde: KO oder Gruppe
  val CPC_LEER     =   5   // LEER
  val CPC_VRKO     =   6   // not yet available
  val CPC_ZRKO     =   7   // not yet available
  val CPC_ERKO     =   8   // only final KO round
  val CPC_TRKO     =   9   // nur Trostrunde KO
  val CPC_GR3to9   = 100   // beliebige Gruppen-Spielphase 3-9er Gruppen
  val CPC_GRPS3    = 101   // Gruppensystem mit 3er
  val CPC_GRPS34   = 102   // Gruppensystem mit 3er und 4er
  val CPC_GRPS4    = 103   // Gruppensystem mit 4er
  val CPC_GRPS45   = 104   // Gruppensystem mit 4er und 5er
  val CPC_GRPS5    = 105   // Gruppensystem mit 5er
  val CPC_GRPS56   = 106   // Gruppensystem mit 5er und 6er
  val CPC_GRPS6    = 107   // Gruppensystem mit 6er
  val CPC_JGJ      = 108   // Gruppe Jeder-gegen-Jeden
  val CPC_KO       = 109   // KO-Spielphase 
  val CPC_SW       = 110   // Switzsystem

  // Competition Phase Status
  val CPS_UNKN = 0   // unknown
  val CPS_AUS  = 1   // Auslosung der Vorrunde, Zwischenrunde, Endrunde, Trostrunde
  val CPS_EIN  = 2   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CPS_FIN  = 3   // Runde/Phase beendet, Auslosung ZR oder ER kann erfolgen
  val CPS_DEP  = 4   // Runde/Phase beendet, dependend competition exists


  implicit val rw: RW[CompPhase] = upickle.default.readwriter[String].bimap[CompPhase](
    x   => write[CompPhaseTx](x.toTx()),   //s"""{ "name", "Hugo" } """,  
    str => fromTx(read[CompPhaseTx](str))
  )

  def get(coId: Long, prevId: Int, coPhCfg: Int, category: Int, name: String, noWinSets: Int, noPlayers: Int): Either[Error, CompPhase] = {  
    // generate new coPhId
    val coPhId = category match {
      case CompPhase.Category_Start  => if (prevId == 0) 1 else 0
      case CompPhase.Category_Winner => if (prevId > 0) (prevId*2 + 1) else 0
      case CompPhase.Category_Looser => if (prevId > 0) (prevId*2) else 0
      case _                         => 0
    }
    if (coPhId == 0) {
      Left(Error("???"))
    } else {
      val coPhTyp  = CompPhase.cfg2typ(coPhCfg)
      val coSize   = coPhTyp match {
        case CPT_GR => noPlayers
        case CPT_KO => genKOSize(noPlayers)
        case CPT_SW => noPlayers  + noPlayers%2
        case _      => 0
      }
     val coph = CompPhase(name, coId, coPhId, coPhCfg, coPhTyp, CPS_UNKN, true, coSize, noPlayers, noWinSets )
     Right(coph)
    }
  }


  def decode(coPhStr: String): Either[Error, CompPhase] = 
    try Right(fromTx(read[CompPhaseTx](coPhStr)))
    catch { case _: Throwable => Left(Error("err0177.decode.CompPhase", coPhStr.take(20))) }

  def fromTx(tx: CompPhaseTx): CompPhase = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashSet
    import shared.model.Competition._

    val cop = new CompPhase(tx.name, tx.coId, tx.coPhId, tx.coPhCfg, tx.coPhTyp, tx.status, tx.enabled, tx.size, tx.noPlayers, tx.noWinSets) 
    cop.matches = tx.matches.map(x=>x.decode)

    cop.coPhTyp match {
      case CPT_GR  => {
        // setup group with draw position information
        val lastPos = tx.groups.foldLeft(1){ (pos, g) =>
          cop.groups = cop.groups :+ Group.fromTx(g, pos)
          pos + g.size 
        }
        val mSize = cop.matches.size
        // hook for calculation trigger and depend, if not present
        if (mSize > 0 && !cop.matches(mSize-1).asInstanceOf[MEntryGr].hasDepend) cop.genGrMatchDependencies()
      }  
      case CPT_KO  => {
        cop.ko = KoRound.fromTx(tx.ko)
      }   
      case _       => // invalid competition phase
    }
    cop
  }

  def cfg2Name(x: Int): String = {
    x match {
      case CPC_UNKN     => "unknown"
      case CPC_INIT     => "READY"
      case CPC_VRGR     => "Gruppen-Vorrunde"
      case CPC_ZRGR     => "Gruppen-Zwischenrunde"
      case CPC_ERGR     => "Gruppen-Endrunde"
      case CPC_TRGR     => "Gruppen-Trostrunde"
      case CPC_ERBO     => "Endrunde"
      case CPC_TRBO     => "Trostdunde"
      case CPC_LEER     => "Leer-Runde"
      case CPC_VRKO     => "KO-Vorrunde"
      case CPC_ZRKO     => "KO-Zwischenrunde"
      case CPC_ERKO     => "KO-Endrunde"
      case CPC_TRKO     => "KO-Trostrunde"
      case CPC_GR3to9   => "Gruppenspiele"
      case CPC_KO       => "KO-Runde"
      case CPC_JGJ      => "Jeder-gegen-Jeden"
      case CPC_GRPS3    => "3er Gruppen"
      case CPC_GRPS34   => "3er und 4er Gruppen"
      case CPC_GRPS4    => "4er Gruppen"
      case CPC_GRPS45   => "4er und 5er Gruppen"
      case CPC_GRPS5    => "5er Gruppen"
      case CPC_GRPS56   => "5er und 6er Gruppen"
      case CPC_GRPS6    => "6er Gruppen"
      case CPC_SW       => "Schweizer System"
      case _            => "unknown"
    }
  }

  def cfg2typ(x:Int): Int = {
    x match {
      case CPC_INIT     => CPT_UNKN 
      case CPC_VRGR     => CPT_GR
      case CPC_ZRGR     => CPT_GR
      case CPC_ERGR     => CPT_GR
      case CPC_TRGR     => CPT_GR
      case CPC_ERBO     => CPT_UNKN
      case CPC_TRBO     => CPT_UNKN
      case CPC_LEER     => CPT_UNKN
      case CPC_VRKO     => CPT_KO
      case CPC_ZRKO     => CPT_KO
      case CPC_ERKO     => CPT_KO
      case CPC_TRKO     => CPT_KO
      case CPC_UNKN     => CPT_UNKN
      case CPC_GR3to9   => CPT_GR
      case CPC_KO       => CPT_KO
      case CPC_JGJ      => CPT_GR
      case CPC_GRPS3    => CPT_GR
      case CPC_GRPS34   => CPT_GR
      case CPC_GRPS4    => CPT_GR
      case CPC_GRPS45   => CPT_GR
      case CPC_GRPS5    => CPT_GR
      case CPC_GRPS56   => CPT_GR
      case CPC_GRPS6    => CPT_GR
      case CPC_SW       => CPT_SW
      case _            => CPT_UNKN
    }    
  }

  def status2Name(x: Int): String = {
    x match {
      case CPS_AUS     => "DRAW(1)" 
      case CPS_EIN     => "INPUT(2)"
      case CPS_FIN     => "FINISHED(3)"
      case CPS_DEP     => "DEPENDEND(4)"
      case CPS_UNKN    => "UNKNOWN(0)"
      case _           => "UNKNOWN(x)"
    }  
  }
}


//
// Transfer representation of a competition phase
//
case class CompPhaseTx(
  val name:      String, 
  val coId:      Long, 
  val coPhId:    Int,
  val coPhCfg:   Int, 
  val coPhTyp:   Int,
  val status:    Int,
  var enabled:   Boolean, 
  var size:      Int, 
  val noPlayers: Int,
  val noWinSets: Int,
  val matches:   ArrayBuffer[MEntryTx], 
  val groups:    ArrayBuffer[GroupTx],
  val ko:        KoRoundTx
)

object CompPhaseTx {
  implicit def rw: RW[CompPhaseTx] = macroRW 
}