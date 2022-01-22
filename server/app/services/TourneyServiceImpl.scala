package tourn.services

import java.util.UUID
import javax.inject.Inject
import java.time.Clock

import scala.collection.mutable.HashMap
import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._
import scala.math.{abs, max}

import play.api.{ Environment, Configuration, Logger }
import play.api.i18n.Messages

import shared.model._
import shared.utils.Constants._
import shared.utils.Routines._

import shared.utils.UpdateTrigger
import shared.model.tabletennis._
import shared.utils._
import models.daos.TourneyDAO


    // TIO.getTrny(tse, true).map {
    //   case Left(err)   => Left(err)
    //   case Right(trny) => {

    //     Right(true)
    //   }
    // }


/**
  * Handles actions to file based database
  *
  * @param clock        The clock instance.
  * @param ex           The execution context.
  */
class TourneyServiceImpl @Inject()()(  implicit
    tonyDao: TourneyDAO,
    env:     Environment,
    cfg:     Configuration,
    ex:      ExecutionContext
) extends TourneyService {

  val logger:   Logger = Logger(this.getClass())
  val playerIdStart: Long = 1000
  val clubIdStart: Long = 2000
  val coIdStart: Long = 100


  //
  // Player Routines
  //

  /** addPlayer - adds new player if it does not exist already
   */
  def addPlayer(pl: Player)(implicit tse: TournSVCEnv): Future[Either[Error, Player]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.addPlayer(pl)
    }


  /** setPlayer updates existing player 
   *  if necessary creates new club entry
   */
  def setPlayer(pl: Player)(implicit tse: TournSVCEnv): Future[Either[Error, Player]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.setPlayer(pl)
    }


  /** setPlayers - adds all players to the tourney (and if necessary all clubs)
   */ 
  def setPlayers(pls: Seq[Player])(implicit tse: TournSVCEnv): Future[Either[Error, Seq[(Long,Int)]]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        pls.foreach(player => trny.addClub(player.clubName))
        pls.foreach(player => if (player.id == 0) trny.addPlayer(player) else trny.setPlayer(player))

        val plIdSeq = for { pl <- pls } yield {
          val plId = trny.plNCY2id.getOrElse((pl.lastname, pl.firstname, pl.clubName, pl.birthyear), 0L)
          if (plId > 0) { (plId, trny.players(plId).rid) } else { (0L,0) }
        }
        Right(plIdSeq)
      }
    }  


  def delPlayers(tse: TournSVCEnv): Future[Either[Error, Int]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val plCnt = trny.players.size
        trny.playerIdMax = playerIdStart
        trny.players     = HashMap()
        trny.plNCY2id = HashMap()
        Right(plCnt)
      }              
    }

  /** resetPlayers
   *  deletes all single/double players and player to competition entries 
   */
  def resetPlayers()(implicit tse: TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        trny.playerIdMax = playerIdStart
        trny.players  = HashMap()
        trny.plNCY2id = HashMap()
        trny.plLIC2id = HashMap()
        trny.pl2co    = HashMap()
        Right(true)
      }
    }  


  def setPlayerEMail(toId: Long, plId: Long, email: String): Future[Either[Error, Player]] =
    TIO.get(toId).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        if (trny.players.isDefinedAt(plId)) { 
          trny.players(plId).email = email
          Right(trny.players(plId)) 
        } else { 
          Left(Error("err0024.svc.setPlayerEmail.notFound", plId.toString))
        }
      }  
    }
 
    
  //
  // Participant Routines
  //  

  /** setParticipant2Comp - maps a participant to a competiton returns Participant2Comp
   *                        overwrites existing entry 
   */ 
  def setParticipant2Comp(p2c: Participant2Comp)(implicit tse :TournSVCEnv): Future[Either[Error, Participant2Comp]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        trny.pl2co((p2c.sno, p2c.coId)) = p2c
        Right(p2c)
      }
    }      

  /** setParticipant2Comps - maps all participants to a competiton returns Participant2Comp
   *                         overwrites all existing entry 
   */
  def setParticipant2Comps(p2cs: Seq[Participant2Comp])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val cnt = trny.pl2co.size
        for (p2c <- p2cs) trny.pl2co((p2c.sno, p2c.coId)) = p2c 
        Right(trny.pl2co.size - cnt)
      }
    }     


  /** delParticipant2Comp - delete participant from a competiton returns number of deleted entries
   *               
   */
  def delParticipant2Comp(coId: Long, sno: String)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => if ( trny.pl2co.isDefinedAt((sno, coId)) ) { trny.pl2co -= ((sno,coId)); Right(1) } else Right(0)
    }

  
  /** delParticipant2Comps - del all participants from a competiton returns number of deleted entries
   *                         if coId=0 all entries will be deleted 
   */
  def delParticipant2Comps(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val cnt = trny.pl2co.size
        if (coId == 0L) { trny.pl2co = HashMap() } else { trny.pl2co = trny.pl2co.filter((t) => t._1._2 != coId) }
        Right(cnt - trny.pl2co.size)
      }
    }   

  // getParticipant2Comps - returns all participants mapped to a competition
  def getParticipant2Comps(toId: Long): Future[Either[Error, Seq[Participant2Comp]]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny.pl2co.values.toSeq)
    }

  def getParticipantPlace(toId: Long, coId: Long, sno: String) : Future[Either[Error, String]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if ( trny.pl2co.isDefinedAt((sno,coId)) ) { 
          Right(trny.pl2co((sno,coId)).placement)   
        } else { 
          Left(Error("err0025.svc.getParticipantPlacement", sno, coId.toString)) 
        }
      }  
    }


  def setParticipantPlace(coId: Long, sno: String, place: String)(implicit tse :TournSVCEnv) : Future[Either[Error, Placement]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if ( trny.pl2co.isDefinedAt((sno,coId)) ) { 
          trny.pl2co((sno,coId)).placement = place
          trny.pl2co((sno,coId)).getPlacement()  
        } else { 
          Left(Error("err0026.svc.setParticipantPlacement", sno, coId.toString)) 
        }
      }
    }    


  def setParticipantStatus(coId: Long, sno: String, status: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.setParticipantStatus(coId, sno, status)
    }  


  // 
  // Club Routines
  // 

  /** addClub adds a new Club based on name
   *  if not exist
   */ 

  def addClub(name: String)(implicit tse :TournSVCEnv): Future[Either[Error, Club]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => Right(trny.addClub(name))
    }


  /** addClubs add clubs based on names
   *  if not already exists
   */ 
  def addClubs(names: Seq[String])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val clubMax = trny.clubIdMax
        for (name <- names) trny.addClub(name)
        Right((trny.clubIdMax - clubMax).toInt)
      }  
    }


  /** setClub sets club to new properties
   *  if not exist adds new club
   */ 
  def setClub(club: Club)(implicit tse :TournSVCEnv): Future[Either[Error, Club]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => Right(_setClub(club,trny))
    }

  def _setClub(club: Club, trny: Tourney): Club = {
    val clId = trny.clName2id.getOrElse(club.name, 0L) 
    if (clId == 0) {
      val nclId = trny.clubIdMax + 1
      //logger.info(s"setClub(${club.name}) with id: ${nclId}")
      trny.clubIdMax = nclId
      trny.clubs(nclId) = club
      trny.clName2id(club.name) = nclId
      trny.clubs(nclId).id = nclId
      trny.clubs(nclId)
    } else {
      trny.clubs(clId) = club
      trny.clubs(clId).id = clId
      trny.clubs(clId)
    }  
  }

 /** setClubs sets clubs to new properties
   *  if a club not exist, create it
   */ 
  def setClubs(clubs: Seq[Club])(implicit tse :TournSVCEnv): Future[Either[Error, Seq[(Long,Int)]]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        for (club <- clubs) {
          val clId = trny.clName2id.getOrElse(club.name, 0L)
          if (clId != 0) { trny.clubs(clId) = club } else { _setClub(club, trny) }
        }
        val clIdSeq = for (club <- clubs) yield {
          val clId = trny.clName2id.getOrElse(club.name, 0L)
          if (clId > 0) {
            (clId, trny.clubs(clId).rid)
          } else {
            (0L,0)
          }
        }
        Right(clIdSeq)
      }  
    }

  // delCLubs delete all registered clubs  
  def delClubs()(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val cnt = trny.clubs.size
        trny.clubIdMax = clubIdStart
        trny.clubs     = HashMap()
        trny.clName2id = HashMap()
        Right(cnt)
      }  
    }  


  //
  // Section Playfield Routines
  // 

  /** delivers all used playfields of the tourney */
  def getPlayfields(toId:Long): Future[Either[Error, Seq[Playfield]]] = 
    TIO.get(toId).map {
      case Left(err)   => Left(err)
      case Right(trny) => Right(trny.run.playfields.values.toSeq)
    }  


  /** delivers the playfield with pfnr of the tourney */
  def getPlayfield(toId: Long, pfnr: Int): Future[Either[Error,Playfield]] = 
    TIO.get(toId).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        if (trny.run.playfields.isDefinedAt(pfnr)) {
          Right(trny.run.playfields(pfnr))
        } else {
          Left(Error("err0029.svc.getPlayfield", pfnr.toString))
        }
      }  
    } 

  /** set playfield of the tourney */
  def setPlayfield(pf: Playfield)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => Right(_setPlayfield(pf, trny))
    } 

  def _setPlayfield(pf: Playfield, trny: Tourney): Int = {
    if (pf.used) {
      // add/set playfield
      trny.run.playfields(pf.nr) = pf
      pf.nr
    } else {
      // remove playfield with this code
      val pfSel = trny.run.playfields.filter( _._2.code == pf.code)
      if (pfSel.size > 0) {
        trny.run.playfields = trny.run.playfields.filter( _._2.code != pf.code)
        pfSel.head._2.nr
      } else {
        0
      }
    }    
  }  

  /** set/delete sequence of playfield for tourney */
  def setPlayfields(pfs: Seq[Playfield])(implicit tse :TournSVCEnv): Future[Either[Error, Int]]= 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        var cnt = 0
        for (pf <- pfs) {
          _setPlayfield(pf, trny)
          if (pf.used) { cnt = cnt + 1 }
        }
        Right(cnt)
      }  
    }  


  /** setPfieldInfo
    * 
    * @param  pfi playfield information
    * @param  implicit tse tourney service environment
    * @return playfield number
    */
  def setPfieldInfo(pfi: PfieldInfo)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = {
    import java.time.LocalDateTime
    import java.time.format.DateTimeFormatter

    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        if (pfi.used) {
          val today = LocalDateTime.now()
          val now = s"${today.format(DateTimeFormatter.ofPattern("yyyyMMddhhmmss"))}"
          val playerA = trny.players(pfi.snoA.toLong)
          val playerB = trny.players(pfi.snoB.toLong)
          val compName = trny.comps(pfi.coId).name
          val pf = Playfield(pfi.nr, pfi.used, now, pfi.code, playerA.getName(), playerA.getClub(),
                             playerB.getName(), playerB.getClub(), compName,  pfi.info)
          // add playfield to the map
          trny.run.playfields(pfi.nr) = pf
        } else {
          // remove the entry from the map with same code
          trny.run.playfields = trny.run.playfields.filter( _._2.code != pfi.code)
        }
        Right(pfi.nr)
      }
    }
  }

/** delete all playfield entries of tourney */
def delPlayfields()(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
  TIO.getTrny(tse, true).map {
    case Left(err)   => Left(err)
    case Right(trny) => {
      val cnt = trny.run.playfields.size
      trny.run.playfields = HashMap()
      Right(cnt)
    }    
  }  

  /** delete a playfield based on its code  */
  def delPlayfield(no: Int, code: String, verify: Boolean = false)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val delCandidate = trny.run.playfields.filter( _._2.nr == no)
        if (delCandidate.size == 1) {
          if (!verify | trny.run.playfields(no).code == code) {
            trny.run.playfields = trny.run.playfields.filter( _._2.nr != no) 
            Right(1)
          } else {
            Right(0)
          }
        } else {
          Left(Error("err0127.svc.delPlayfield", s"${no}/${code}"))
        } 
      }    
    }



  // 
  // Competition Routines
  //
  def setComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv): Future[Either[Error, Competition]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (co.id == 0) trny.addComp(co) else trny.setComp(co)
    }

  def setComps(comps: Seq[Competition])(implicit msgs: Messages, tse: TournSVCEnv): Future[Either[Error, Seq[(Long,Int)]]] =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        val mapIdRid = for {
          comp <- comps 
        } yield {
          trny.setComp(comp) match {
            case Left(err)  => (0L,0)
            case Right(co)  => (co.id, co.rid)
          }
        } 
        Right(mapIdRid)
      }
    }

  def setCompStatus(coId: Long, status: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.comps.isDefinedAt(coId)) { 
          trny.comps(coId).status = status
          Right(true) 
        } else {
          Right(false)
        }  
      }  
    }  

  def setCompRatingLowLevel(coId: Long, level: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (trny.comps.isDefinedAt(coId)) { trny.comps(coId).setRatingLowLevel(level) ; Right(true) } else Right(false)
    }  

  def setCompRatingUpperLevel(coId: Long, level: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (trny.comps.isDefinedAt(coId)) { trny.comps(coId).setRatingUpperLevel(level) ; Right(true) } else Right(false)
    } 


  /** delComp delete one competition 
   */
  def delComp(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.delComp(coId)
    }


  /** delComps delete all competitions
   */
  def delComps()(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        val cnt = trny.comps.size
        trny.playerIdMax = playerIdStart
        trny.players     = HashMap()
        trny.plNCY2id = HashMap()   
        trny.comps = HashMap()
        trny.coName2id = HashMap()
        trny.compIdMax = coIdStart
        trny.clubIdMax = clubIdStart
        trny.clubs     = HashMap()
        trny.clName2id = HashMap()
        trny.pl2co = HashMap()
        trny.run.playfields = HashMap()
        Right(cnt)
      }  
    }

  def getComp(toId: Long, coId: Long) : Future[Either[Error, Competition]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (trny.comps.isDefinedAt(coId)) Right(trny.comps(coId)) else Left(Error("err0014.trny.compNotFound", coId.toString))
    }    

  def getComps(toId: Long): Future[Either[Error, Seq[Competition]]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny.comps.values.toSeq)
    }       

  def getCompStatus(toId: Long, coId: Long): Future[Either[Error, Int]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (trny.comps.isDefinedAt(coId)) Right(trny.comps(coId).status) else Left(Error("err0030.svc.getCompStatus", coId.toString) ) 
    }    

  def getCompName(toId: Long, coId: Long): Future[Either[Error, String]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (trny.comps.isDefinedAt(coId)) Right(trny.comps(coId).name) else Left(Error("err0031.svc.getCompName", coId.toString) ) 
    }   


  // 
  // Match Routines
  // 
  def setMatch(ma: MatchEntry)(implicit tse :TournSVCEnv): Future[Either[Error,Boolean]] = 
    //def prt(msg: String) = logger.info(s"setMatch: ${msg}")
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.run.cophs.isDefinedAt((ma.coId, ma.coPh))) {

          val trigCmd = ma.getType() match {
            case CSY_GR => UpdateTrigger("MatchGr", "000000", tse.toId, ma.coId, ma.coPh, ma.grId)
            case CSY_KO => UpdateTrigger("MatchKo", "000000", tse.toId, ma.coId, ma.coPh, 0)
            case _      => UpdateTrigger("Match", tse.toId)
          }

          //logger.info(s"setMatch: ${ma.status} ${ma.coId} ${ma.coPh} ${ma.result.mkString(":")}")
          //logger.info(s"setMatch before: ${trny.run.cophs}")
          //trny.run.cophs((ma.coId, ma.coPh)).addMatch(ma, prt)
          
          // delete playfield, ma.playfield contains playfield code
          trny.run.playfields = trny.run.playfields.filter( _._2.code != ma.playfield)
          trny.run.cophs((ma.coId, ma.coPh)).addMatch(ma)
          //logger.info(s"setMatch after: ${trny.run.cophs}")
          trigger(trny, trigCmd)
          Right(true)
        } else {
          Right(false)
        }
      }
    }  


  def setMatches(mas: Seq[MatchEntry])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = {
    //def prt(msg: String) = logger.info(s"setMatch: ${msg}")
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        val cnt = for {
          ma <- mas
        } yield {
          if (trny.run.cophs.isDefinedAt((ma.coId, ma.coPh))) {
            trny.run.cophs((ma.coId, ma.coPh)).addMatch(ma)
            //trny.run.cophs((ma.coId, ma.coPh)).addMatch(ma, prt)
            1
          }  
        }  
        trigger(trny, UpdateTrigger("Match", tse.toId))
        Right(cnt.length)
      }
    }  
  }    

  /** delMatches delete all matches of a certain competition phase
    * 
    * @param coId
    * @param coPh
    * @param tse
    * @return true if matches of competition phase was deleted otherwise false
    */

  def delMatches(coId: Long, coPh: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.run.cophs.isDefinedAt((coId, coPh))) {
          trny.run.cophs((coId, coPh)).resetMatches()
          trigger(trny, UpdateTrigger("MatchReset", tse.callerIdent, tse.toId, coId, coPh, 0))
          Right(true)
        } else {
          Right(false)
        }
      }
    }


  def getMatchKo(toId: Long, coId: Long, coPh:Int): Future[Either[Error, ResultEntrys]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.run.cophs.isDefinedAt((coId, coPh))) {
          //logger.info(s"getMatchKo: ${coId} ${coPh} /n ${ trny.run.cophs((coId,coPh)).ko.toString}")
          Right(new ResultEntrys(for { re <- trny.run.cophs((coId,coPh)).ko.results.toSeq } yield { re.stringify() } ) )
        } else {
          Right(ResultEntrys(Seq()))   
        }  
      }
    }    

  
  def getMatchGr(toId: Long, coId: Long, coPh:Int, grId: Int):  Future[Either[Error, ResultEntrys]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.run.cophs.isDefinedAt((coId, coPh))) {
          Right(trny.run.cophs((coId,coPh)).groups(grId-1).getResultEntrys())
        } else {
          Right(ResultEntrys(Seq()))   
        }  
      }
    }


  // 
  // Competition Phase Routines
  // 

  /** setCompPhase - set competition phases 
   */
  def setCompPhase(coph: CompPhase)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]  =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        trny.run.cophs((coph.coId, coph.coPh)) = coph
        if (tse.trigger) trigger(trny, UpdateTrigger("CompPhase", tse.callerIdent, tse.toId, coph.coId, 0))
        Right(true)
      }
    }      
  

  /** delCompPhases - reset competition phases  
   */
  def delCompPhases(coId: Long=0)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (coId == 0) {
          trny.run.cophs = HashMap()
        } else {
          for ( (key, coph) <- trny.run.cophs ) if (coph.coId == coId) trny.run.cophs.remove(key)
        }
        if (tse.trigger) trigger(trny, UpdateTrigger("CompPhase", tse.callerIdent, tse.toId, coId, 0))
        Right(true)
      }
    }     

  
  // 
  // Tourney Routines
  //

  /** addTourney adds a tourney, returns tourneyId, error if a tourney with same 
   * startdate and organization already exists 
   */
  def addTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Long]] = 
    if (trny.orgDir != tse.orgDir | trny.id != 0) {
      Future(Left(Error("err0080.access.invalidRights")))
    } else {
      TIO.load(trny.orgDir, trny.startDate).flatMap { 
        case Left(err)   => TIO.add(trny).map {
          case Left(err)    => Left(err)
          case Right(nTrny) => Right(nTrny.id)  
        }
        case Right(trny) => Future( Left(Error("err0064.svc.addTournBase", trny.orgDir, trny.startDate.toString)) )
      }
    }

  /** delTourney deletes tourney from database 
   *  
   */
  def delTourney(toId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = TIO.delete(toId, tse.orgDir)


  /** addTournBase adds a tourney from a tournBase, returns tourney, error if a tourney with same 
    * startdate and organization already exists 
    */
  def addTournBase(trnyBase: TournBase)(implicit tse :TournSVCEnv): Future[Either[Error, Tourney]] = 
    if (trnyBase.orgDir != tse.orgDir | trnyBase.id != 0) {
      Future(Left(Error("err0080.access.invalidRights")))
    } else {
      TIO.load(trnyBase.orgDir, trnyBase.startDate).flatMap { 
        case Left(err)   => TIO.add(trnyBase)                                  
        case Right(trny) => Future( Left(Error("err0064.svc.addTournBase", trnyBase.orgDir, trnyBase.startDate.toString)) )
      }
    }

  /** saveTourney put tourney data to disk
    * 
    */
  def saveTourney(toId: Long)(implicit tse :TournSVCEnv): Either[Error, Boolean] = TIO.save(toId) 


  /** setTournBase updates tourney basic information
    * 
    */ 
  def setTournBase(tb: TournBase)(implicit tse :TournSVCEnv): Future[Either[Error, Tourney]] =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (tse.toId == tb.id) {
          tonyDao.insertOrUpdate(tb)
          trny.name    = tb.name
          trny.endDate = tb.endDate
          trny.typ     = tb.typ
          trny.privat  = tb.privat
          (for {
            contact <- Contact.decode(tb.contact)
            address <- Address.decode(tb.address) 
          } yield { (contact, address) }) match {
            case Left(err)  => Left(err)
            case Right(res) => trny.contact = res._1; trny.address = res._2; Right(trny)
          }
        } else {
          Left(Error("err0032.svc.setTournBase", tb.id.toString))
        }
      }
    }

  /** addTournCTT sets tourney information from ClickTT
    * 
    */     
  def addTournCTT(ctt: CttTournament, orgDir: String, organizer: String, sDate: Int=0, eDate: Int=0,
                  contact: String = "lastname·firstname·phone·email",
                  address: String = "description·country·zip·city·street")(implicit  msgs: Messages, tse :TournSVCEnv): Future[Either[Error, Tourney]] =
  {
    TIO.update(ctt, orgDir, organizer, sDate, eDate, contact, address)
  }                
                  
  // setTournAddress   
  def setTournAddress(toId: Long, addr: Address): Future[Either[Error, Address]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => { trny.address = addr; Right(trny.address) }
    }

  // setTournContact
  def setTournContact(toId: Long, contact: Contact): Future[Either[Error, Contact]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => { trny.contact = contact; Right(trny.contact) }
    }

  def getTournCfg(toId: Long): Future[Either[Error, Tourney]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny)
    }


  def getTournRun(toId: Long) : Future[Either[Error, TournRun]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny.run)
    }

  // getTournPlayer returns all players of the tourney
  def getTournPlayers(toId: Long): Future[Either[Error, Seq[Player]]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny.players.values.toSeq)
    }

  // getTournPlayer returns all clubs of the tourney
  def getTournClubs(toId: Long): Future[Either[Error, Seq[Club]]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny.clubs.values.toSeq)
    }


  //
  // Management Routines
  //  
  def isAllowed(toId: Long, orgDir: String): Future[Either[Error, Boolean]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right((trny.orgDir == orgDir))
    }

  def clean(): Unit = TIO.clean()

  def trigger(tse: TournSVCEnv, trigCmd: String): Future[Either[Error, Boolean]] = {
    import controllers.{ EventActor, ActorRefManager }
    TIO.getTrny(tse).map {
      case Left(err)   => Left(err)
      case Right(trny) => { 
        logger.info(s"trigger -> orgDir: ${trny.orgDir} id: ${trny.id}  cmd: ${trigCmd}")
        EventActor.manager ! ActorRefManager.SendMessage(trny.orgDir, trigCmd)
        Right(true) 
      } 
    }
  }

  def trigger(trny: Tourney, trigger: UpdateTrigger): Unit = {
    import controllers.{ EventActor, ActorRefManager }
    EventActor.manager ! ActorRefManager.SendMessage(trny.orgDir, trigger.toString) 
  }
  
  def trigger(orgDir: String, trigger: UpdateTrigger): Unit = {
    import controllers.{ EventActor, ActorRefManager }
    EventActor.manager ! ActorRefManager.SendMessage(orgDir, trigger.toString) 
  }

}