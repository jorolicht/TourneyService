package tourn.services

import java.util.UUID

import scala.concurrent.Future
import scala.concurrent.duration._
import play.api.i18n.Messages
import com.google.inject.ImplementedBy

import shared.model._
import shared.utils._
import shared.utils.UpdateTrigger

case class TournSVCEnv(toId: Long, orgDir: String, trigger: Boolean, callerIdent: String = "00000")

/**
  * handles tourney service
  */
trait TourneyService {

  // Player Interface
  def addPlayer(pl: Player)(implicit tse: TournSVCEnv): Future[Either[Error, Player]]
  def setPlayer(pl: Player)(implicit tse: TournSVCEnv): Future[Either[Error, Player]]
  def setPlayerLicence(plId: Long, licence: String)(implicit tse: TournSVCEnv): Future[Either[Error, Player]]
  
  def updPlayers(pls: Seq[Player])(implicit tse: TournSVCEnv): Future[Either[Error, Seq[Player]]]
  def delPlayers(tse: TournSVCEnv): Future[Either[Error, Int]]

  // setPlayerEMail 
  def setPlayerEMail(toId: Long, plId: Long, email: String): Future[Either[Error, Player]]


  //
  // Participant Interface (participant could be Single,Double or Team (future) 
  //
  
  // setPant2Comp maps a participant to a competiton returns Pant2Comp
  def setPant2Comp(p2c: Pant2Comp)(implicit tse :TournSVCEnv):Future[Either[Error, Pant2Comp]]
  // setPant2Comps maps all participants to a competiton returns number of mapped entries
  def setPant2Comps(p2cs: Seq[Pant2Comp])(implicit tse :TournSVCEnv): Future[Either[Error, Int]]

  // delPant2Comp deletes entry, returns number of deleted entries (0 or 1)
  def delPant2Comp(coId: Long, sno: String)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] 

  // delPant2Comps delete all participants of given competition id, if coId=0 deletes all participants from all competition
  def delPant2Comps(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  def getPant2Comps(toId: Long): Future[Either[Error, Seq[Pant2Comp]]]

  def getPantPlace(toId: Long, coId: Long, sno: String): Future[Either[Error, String]]
  def setPantPlace(coId: Long, sno: String, place: String)(implicit tse :TournSVCEnv): Future[Either[Error, Placement]]
  def setPantStatus(coId: Long, sno: String, status: PantStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, PantStatus.Value]]
  def setPantBulkStatus(coId: Long, pantStatus: List[(String, PantStatus.Value)])(implicit tse :TournSVCEnv): Future[Either[Error, Int]]

  //
  // Club Interface
  //
  def setClub(club: Club, merge: Boolean=false)(implicit tcp :TournSVCEnv): Future[Either[Error, Club]]
  def setClubs(club: Seq[Club])(implicit tcp :TournSVCEnv): Future[Either[Error, Seq[Club]]]
  def delClubs()(implicit tcp :TournSVCEnv): Future[Either[Error, Int]]
  
  //
  // Playfield Interface
  //
  def getPlayfields(toId: Long): Future[Either[Error, Seq[Playfield]]]
  def getPlayfield(toId: Long, pfNr: Int): Future[Either[Error, Playfield]]

  def setPlayfield(pf: Playfield)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  def setPlayfields(pfs: Seq[Playfield])(implicit tse :TournSVCEnv) : Future[Either[Error, Int]] 
  def setPfieldInfo(pfi: PfieldInfo)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  def delPlayfields()(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  def delPlayfield(no: Int, code: String, verify: Boolean = false)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]

  //
  // Competition Interface
  //
  def setComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Competition]]
  //def setComps(comps: Seq[Competition])(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Seq[(Long,Int)]]]
  def updComps(comps: Seq[Competition])(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Seq[Competition]]]
  def addComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Competition]]

  def setCompStatus(coId: Long, status: CompStatus.Value)(implicit tse: TournSVCEnv): Future[Either[Error, Boolean]] 
  def setCompRatingLowLevel(coId: Long, level: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]
  def setCompRatingUpperLevel(coId: Long, level: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]

  def delComp(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]
  def delComps()(implicit tse :TournSVCEnv): Future[Either[Error, Int]]

  def getComp(toId: Long, coId: Long): Future[Either[Error, Competition]]
  def getComps(toId: Long): Future[Either[Error, Seq[Competition]]]
  def getCompStatus(toId: Long, coId: Long): Future[Either[Error, CompStatus.Value]]
  def getCompName(toId: Long, coId: Long): Future[Either[Error, String]]

  // Match Interface
  def setMatch(ma: MEntry)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]
  def setMatches(ma: Seq[MEntry])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] 
  def delMatches(coId: Long, coPh: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] 
  def getMatchKo(toId: Long, coId: Long, coPh:Int): Future[Either[Error, Seq[ResultEntry]]] 
  def getMatchGr(toId: Long, coId: Long, coPh:Int, grId: Int): Future[Either[Error, Seq[ResultEntry]]] 

  // Competition Phase Interface
  def setCompPhase(coph: CompPhase)(implicit tcp :TournSVCEnv): Future[Either[Error, Boolean]] 
  def delCompPhases(coId: Long=0)(implicit tcp :TournSVCEnv): Future[Either[Error, Boolean]]  
   

  // Tourney Inteface
  def addTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]
  def delTourney(toId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]
  def delTourney(sDate: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]


  def addTournBase(trnyBase: TournBase)(implicit tse :TournSVCEnv): Future[Either[Error, Tourney]]
  def regTournBase(trnyBase: TournBase)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]

  def saveTourney(toId: Long)(implicit tse :TournSVCEnv): Either[Error, Boolean]
  def syncTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]

  def setTournBase(tb: TournBase)(implicit tse :TournSVCEnv): Future[Either[Error, Tourney]]
  def addTournCTT(ctt: CttTournament, orgDir: String, organizer: String)(implicit msgs: Messages, tse :TournSVCEnv): Future[Either[Error, Tourney]]
  def updTournCTT(ctt: CttTournament, toId: Long)(implicit msgs: Messages, tse :TournSVCEnv): Future[Either[Error,Seq[(Long, Int)]]] 

  def setTournAddress(toId: Long, address: Address): Future[Either[Error, Address]]
  def setTournContact(toId: Long, contact: Contact): Future[Either[Error, Contact]]
  
  //def getTournRun(toId: Long):     Future[Either[Error, TournRun]]
  def getTourney(toId: Long):     Future[Either[Error, Tourney]]

  def getTournPlayers(toId: Long): Future[Either[Error, Seq[Player]]]
  def getTournClubs(toId: Long):   Future[Either[Error, Seq[Club]]]
  def getTournStartdate(toId: Long): Future[Either[Error, Int]]
  

  // Management Interface
  def isAllowed(toId: Long, orgDir: String): Future[Either[Error, Boolean]] 
  def clean(): Unit
  def dump(toId: Long): Future[Either[Error, String]] 

  def trigger(trny: Tourney, trigger: UpdateTrigger): Unit
  def trigger(orgDir: String, trigger: UpdateTrigger): Unit
  def trigger(tse: TournSVCEnv, trigCmd: String): Future[Either[Error,Boolean]]

}

