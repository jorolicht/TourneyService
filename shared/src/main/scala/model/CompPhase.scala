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
  var matches      = ArrayBuffer[MatchEntry]()
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

  //addMatch with debug print
  def addMatch(m: MatchEntry, prt: (String)=>Unit ): Boolean = {
    coPhTyp match {
      case CPT_GR => if (m.grId > 0 & m.grId <= groups.length) groups(m.grId-1).addMatch(m) else false
      case CPT_KO => ko.addMatch(m, prt); prt(s"addMatch ko.results: ${ko.toString}"); true
      case _      => false
    }
  } 

  //addMatch
  def addMatch(m: MatchEntry): Boolean = {
    coPhTyp match {
      case CPT_GR => if (m.grId > 0 & m.grId <= groups.length) groups(m.grId-1).addMatch(m) else false
      case CPT_KO => ko.addMatch(m) 
      case _      => false
    }
  } 


  // def setMatches(round: Int, matchList: ArrayBuffer[MatchEntry]) = {
  //   for (rnd <- matches.size to round) matches += ArrayBuffer[MatchEntry]()
  //   matches(round) = matchList
  // }

  def resetMatches(): Unit = {
    coPhTyp match {
      case CPT_GR => for (i <- 0 to groups.size-1) groups(i).resetMatch
      case CPT_KO => ko.resetMatch()
      case _      => // do some error handling?
    }
  } 

  def initMatches(round: Int) = {
    coPhTyp match {
      case CPT_GR => // todo
      case CPT_KO => // todo
      case _      => // do some error handling?
    }
  }


  // calculate players position within competition phase
  def calc(grId: Int = -1): Boolean = {
    coPhTyp match {
      case CPT_GR => 
        if (-1 == grId) {
          for (g <- 0 to groups.length-1) { groups(g).calc }
          true 
        } else {
          if (grId > 0 & grId <= groups.length) {
            groups(grId-1).calc
            true
          } else {
            false
          }
        }  
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

  def toTx(): CompPhaseTx = {
    CompPhaseTx(name, coId, coPh, coPhId, coPhTyp, status, enabled, size, noPlayers, noWinSets, 
                pants.map(x=>x.value), matches.map(x=>x.encode), groups.map(g=>g.toTx), ko.toTx) 
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
  val CPS_CFG = 0   // RESET
  val CPS_AUS = 1   // Auslosung der Vorrunde, Zwischenrunde, Endrunde, Trostrunde
  val CPS_EIN = 2   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CPS_FIN = 3   // Runde/Phase beendet, Auslosung ZR oder ER kann erfolgen

  implicit val rw: RW[CompPhase] = upickle.default.readwriter[String].bimap[CompPhase](
    x   => write[CompPhaseTx](x.toTx()),   //s"""{ "name", "Hugo" } """,  
    str => fromTx(read[CompPhaseTx](str))
  )

  def decode(coPhStr: String): Either[Error, CompPhase] = 
    try Right(fromTx(read[CompPhaseTx](coPhStr)))
    catch { case _: Throwable => Left(Error("err0177.decode.CompPhase", coPhStr.take(20))) }

  def fromTx(tx: CompPhaseTx): CompPhase = {
    val cop = new CompPhase(tx.name, tx.coId, tx.coPh, tx.coPhId, tx.coPhTyp, tx.status, tx.enabled, tx.size, tx.noPlayers, tx.noWinSets) 
    cop.pants   = tx.pants.map(x=>SNO(x))
    cop.matches = tx.matches.map(x=>MatchEntry.obify(x))

    cop.coPhTyp match {
      case CPT_GR  => {
        for (g <- tx.groups) { cop.groups = cop.groups :+ Group.fromTx(g) }
      }  
      case CPT_KO  => {
        cop.ko = KoRound.fromTx(tx.ko)
      }   
      case _       => // invalid competition phase
    }
    //for (g <- tx.matches) { cop.groups = cop.groups :+ Group.fromTx(g) }
    cop
  }

}


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
  val pants:      ArrayBuffer[String],
  val matches:   ArrayBuffer[String], 
  val groups:    ArrayBuffer[GroupTx],
  val ko:        KoRoundTx
)

object CompPhaseTx {
  implicit def rw: RW[CompPhaseTx] = macroRW 
}