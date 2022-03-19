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
case class CompPhase(val name: String, val coId: Long, val coPh: Int, val coTyp: Int, var enabled: Boolean, 
                var size: Int, var noPlayers: Int, noWinSets: Int = 3) {
  var groups       = ArrayBuffer[Group]()                                                // groups of the competition (only gr rounds)
  var ko           = new KoRound(size, KoRound.getNoRounds(noPlayers), name, noWinSets)  // ko games of ghe competition (only ko rounds)

  
  def init() = {
    
  }
  def encode(): String = {
    write[CompPhaseTx](toTx())
  }


  // distribute player to the groups (no,size,quali)
  def init(plList: Array[ParticipantEntry], grpCfg: List[(Int,Int,Int)] = List()): Boolean = {
    getSystem match { 
      case CSY_GR => {
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
      case CSY_KO => ko.init(plList); true
      case _      => false
    }
  }  

  //addMatch with debug print
  def addMatch(m: MatchEntry, prt: (String)=>Unit ): Boolean = {
    getSystem match {
      case CSY_GR => if (m.grId > 0 & m.grId <= groups.length) groups(m.grId-1).addMatch(m) else false
      case CSY_KO => ko.addMatch(m, prt); prt(s"addMatch ko.results: ${ko.toString}"); true
      case _      => false
    }
  } 

  //addMatch
  def addMatch(m: MatchEntry): Boolean = {
    getSystem match {
      case CSY_GR => if (m.grId > 0 & m.grId <= groups.length) groups(m.grId-1).addMatch(m) else false
      case CSY_KO => ko.addMatch(m) 
      case _      => false
    }
  } 

  def resetMatches(): Unit = {
    getSystem match {
      case CSY_GR => for (i <- 0 to groups.size-1) groups(i).resetMatch
      case CSY_KO => ko.resetMatch()
      case _      => // do some error handling?
    }
  } 

  def initMatches(round: Int) = {
    getSystem match {
      case CSY_GR => // todo
      case CSY_KO => // todo
      case _      => // do some error handling?
    }
  }


  // calculate players position within competition phase
  def calc(grId: Int = -1): Boolean = {
    getSystem match {
      case CSY_GR => 
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
      case CSY_KO => ko.calc; true
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

    val str   = new StringBuilder(s"${name} (coId:${coId}) ${grPhName(coPh)}(${coPh}) Typ:${coTyp}\n")
    getSystem match {
      case CSY_GR => for (i <- 0 to groups.length-1) str.append(groups(i).toString)
      case CSY_KO => str.append(s"${ko.toString}")
      case _      => str.append(s"UNKNOWN COMPETITION PHASE")
    }
    str.toString
  }  

  def toTx(): CompPhaseTx = {
    var cctx = CompPhaseTx(name, coId, coPh, coTyp, enabled, size, noPlayers, noWinSets) 

    getSystem match {
      case CSY_GR  => for (i <- 0 to groups.length-1) cctx.groups = cctx.groups :+ groups(i).toTx 
      case CSY_KO  => cctx.ko      = ko.toTx
      case _       => // invalid competition phase
    }
    cctx
  }  

  def getSystem = CompPhase.getSystem(coPh) 
}


object CompPhase {
  implicit def rw: RW[CompPhase] = macroRW 
  def decode(coPhStr: String): Either[Error, CompPhase] = 
    try Right(fromTx(read[CompPhaseTx](coPhStr)))
    catch { case _: Throwable => Left(Error("err0177.decode.CompPhase", coPhStr.take(20))) }

  def fromTx(tx: CompPhaseTx): CompPhase = {
    val cop = new CompPhase(tx.name, tx.coId, tx.coPh, tx.coTyp, tx.enabled, tx.size, tx.noPlayers, tx.noWinSets) 
    cop.getSystem match {
      case CSY_GR  => for (g <- tx.groups) { cop.groups = cop.groups :+ Group.fromTx(g) }
      case CSY_KO  => cop.ko = KoRound.fromTx(tx.ko) 
      case _       => // invalid competition phase
    }
    //for (g <- tx.matches) { cop.groups = cop.groups :+ Group.fromTx(g) }
    cop
  }

  def getSystem(coPh: Int): Int = {
    coPh match {
      case CP_VRGR | CP_ZRGR | CP_ERGR | CP_TRGR  => CSY_GR 
      case CP_ERKO | CP_VRKO | CP_ZRKO | CP_TRKO  => CSY_KO
      case _                                      => CSY_UNKN
    }
  }

}


case class CompPhaseTx(
  val name:      String, 
  val coId:      Long, 
  val coPh:      Int, 
  val coTyp:     Int, 
  var enabled:   Boolean, 
  var size:      Int, 
  val noPlayers: Int,
  val noWinSets: Int,

  var groups:     List[GroupTx] = List[GroupTx](),
  var ko:         KoRoundTx  = KoRoundTx(0, 0, "", 0)
)

object CompPhaseTx {
  implicit def rw: RW[CompPhaseTx] = macroRW 
}