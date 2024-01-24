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
import shared.model.CompPhase._
import shared.utils.Constants._
import shared.utils.Routines._

import shared.utils.UpdateTrigger
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

  def setPlayer(plId: Long, license: CttLicense)(implicit tse: TournSVCEnv): Future[Either[Error, Player]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.setPlayer(plId, license)
    }



  /** updPlayers - adds all players to the tourney (and if necessary all clubs)
   */ 
  def updPlayers(pls: Seq[Player])(implicit tse: TournSVCEnv): Future[Either[Error, Seq[Player]]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val (lefts, rights) = (for { player <- pls } yield {
          if (player.id == 0) { 
            logger.info(s"${player.lastname} ${player.firstname} ${player.clubName} ${player.getTTR} ")
            trny.addPlayer(player) 
          }
          else { trny.setPlayer(player) }  
        }).partitionMap(identity)
        Right(rights)
      }
    }

 
  def delPlayers(tse: TournSVCEnv): Future[Either[Error, Int]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val plCnt = trny.players.size
        trny.playerIdMax = playerIdStart
        trny.players     = HashMap()
        //trny.license2id  = HashMap()
        trny.player2id   = HashMap()
        trny.pl2co       = HashMap()
        Right(plCnt)
      }              
    }


  def regSingle(coId: Long, pl: Player, pStatus: PantStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, SNO]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.regSingle(coId, pl, pStatus) match {
        case Left(err)     => Left(err)
        case Right(result) => Right(result)
      } 
    }


  def regSingle(coId: Long, pList: List[Player], pStatus: PantStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, List[SNO]]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.regSingle(coId, pList, pStatus) match {
        case Left(err)     => Left(err)
        case Right(result) => Right(result)
      } 
    }


  def regDouble(coId: Long, pls: (Long,Long), pStatus: PantStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, SNO]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.regDouble(coId, pls, pStatus) match {
        case Left(err)     => Left(err)
        case Right(result) => Right(result)
      } 
    }


  def regDouble(coId: Long, ppList: List[(Long,Long)], pStatus: PantStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, List[SNO]]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.regDouble(coId, ppList, pStatus) match {
        case Left(err)     => Left(err)
        case Right(result) => Right(result)
      } 
    }

    
  //
  // Participant Routines
  //  

  /** setPant2Comp - maps a participant to a competiton returns Pant2Comp
   *                        overwrites existing entry 
   */ 
  def setPant2Comp(p2c: Pant2Comp)(implicit tse :TournSVCEnv): Future[Either[Error, Pant2Comp]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        trny.pl2co((p2c.sno, p2c.coId)) = p2c
        Right(p2c)
      }
    }      

  /** setPant2Comps - maps all participants to a competiton returns Pant2Comp
   *                         overwrites all existing entry 
   */
  def setPant2Comps(p2cs: Seq[Pant2Comp])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val cnt = trny.pl2co.size
        for (p2c <- p2cs) trny.pl2co((p2c.sno, p2c.coId)) = p2c 
        Right(trny.pl2co.size - cnt)
      }
    }     


  /** delPant2Comp - delete participant from a competiton returns number of deleted entries
   *               
   */
  def delPant2Comp(coId: Long, sno: String)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => if ( trny.pl2co.isDefinedAt((sno, coId)) ) { trny.pl2co -= ((sno,coId)); Right(1) } else Right(0)
    }

  
  /** delPant2Comps - del all participants from a competiton returns number of deleted entries
   *                  if coId=0 all entries will be deleted 
   */
  def delPant2Comps(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val cnt = trny.pl2co.size
        if (coId == 0L) { trny.pl2co = HashMap() } else { trny.pl2co = trny.pl2co.filter((t) => t._1._2 != coId) }
        Right(cnt - trny.pl2co.size)
      }
    }   

  // getPant2Comps - returns all participants mapped to a competition
  def getPant2Comps(toId: Long): Future[Either[Error, Seq[Pant2Comp]]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny.pl2co.values.toSeq)
    }

  def getPantPlace(toId: Long, coId: Long, sno: SNO) : Future[Either[Error, String]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if ( trny.pl2co.isDefinedAt((sno.value,coId)) ) { 
          Right(trny.pl2co((sno.value,coId)).placement)   
        } else { 
          Left(Error("err0025.svc.getParticipantPlacement", sno.value, coId.toString)) 
        }
      }  
    }


  def setPantPlace(coId: Long, sno: SNO, place: String)(implicit tse :TournSVCEnv) : Future[Either[Error, Placement]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if ( trny.pl2co.isDefinedAt((sno.value,coId)) ) { 
          trny.pl2co((sno.value,coId)).placement = place
          trny.pl2co((sno.value,coId)).getPlacement()  
        } else { 
          Left(Error("err0026.svc.setParticipantPlacement", sno.value, coId.toString)) 
        }
      }
    }    


  def setPantStatus(coId: Long, sno: SNO, status: PantStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, PantStatus.Value]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.setPantStatus(coId, sno, status)
    }  


  def setPantBulkStatus(coId: Long, pantStatus: List[(String, PantStatus.Value)])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.setPantBulkStatus(coId, pantStatus)
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
  def setClub(club: Club, merge: Boolean=false)(implicit tse :TournSVCEnv): Future[Either[Error, Club]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.setClub(club, merge)
    }


 /** setClubs sets clubs to new properties
   *  if a club not exist, create it
   */ 
  def setClubs(clubs: Seq[Club])(implicit tse :TournSVCEnv): Future[Either[Error, Seq[Club]]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val (left, right) = (for (club <- clubs) yield { trny.setClub(club) }).partitionMap(identity)
        Right(right)
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
        trny.club2id = HashMap()
        Right(cnt)
      }  
    }  


  //
  // Section Playfield Routines
  // 

  /** delivers all used playfields of the tourney */
  def getPlayfields(toId: Long): Future[Either[Error, Seq[Playfield]]] = 
    TIO.get(toId).map {
      case Left(err)   => Left(err)
      case Right(trny) => Right(trny.playfields.values.toSeq)
    }  


  /** delivers the playfield with pfnr of the tourney */
  def getPlayfield(toId: Long, pfnr: String): Future[Either[Error, Playfield]] = 
    TIO.get(toId).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.getPlayfield(pfnr)
    }  

  /** set playfield of the tourney */
  def setPlayfield(pf: Playfield)(implicit tse :TournSVCEnv): Future[Either[Error, Unit]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        trny.setPlayfield(pf)
        trigger(tse, "PLAYFIELD")
        Right({})
      }  
    } 

  def setPlayfield(coId: Long, coPhId: Int, game: Int, startTime: String)(implicit tse :TournSVCEnv): Future[Either[Error, Unit]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.setPlayfield(coId, coPhId, game, startTime); trigger(tse, "PLAYFIELD"); Right({})
    } 


  /** set/delete sequence of playfield for tourney */
  def setPlayfields(pfs: Seq[Playfield])(implicit tse :TournSVCEnv): Future[Either[Error, Unit]]= 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.setPlayfields(pfs); trigger(tse, "PLAYFIELD"); Right({})
    }  
  

  /** delete all playfield entries of tourney */
  def delPlayfields()(implicit tse :TournSVCEnv): Future[Either[Error, Unit]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => trny.delPlayfields(); trigger(tse, "PLAYFIELD"); Right({})
    }  

  /** delete a playfield based on playfield number (not secure, as new game could overwrite old entry!)
   *  or based on it's full specification (competition identifier, competition phase identifier, game number)
   */
  def delPlayfield(coId: Long, coPhId: Int, game: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val result = trny.delPlayfield(coId, coPhId, game)
        trigger(tse, "PLAYFIELD")
        Right(result)
      }  
    }
  
  def delPlayfield(pfNo: String)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] =
    TIO.getTrny(tse, true).map {
      case Left(err)   => Left(err)
      case Right(trny) => {
        val result = trny.delPlayfield(pfNo)
        trigger(tse, "PLAYFIELD")
        Right(result)
      }  
    }


  // 
  // Competition Routines
  //
  def setComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv): Future[Either[Error, Competition]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.setComp(co)
    }

  def addComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv): Future[Either[Error, Competition]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.addComp(co)
    }



  def updComps(comps: Seq[Competition])(implicit msgs: Messages, tse: TournSVCEnv): Future[Either[Error, Seq[Competition]]] =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        val (eList, cList) = (comps.map { co => 
          if (co.id == 0) trny.addComp(co) else trny.setComp(co)
        }).partitionMap(identity)  
        Right(cList)
      }
    }

  def setCompStatus(coId: Long, status: CompStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, Unit]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.setCompStatus(coId, status)
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
  def delComp(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Unit]] =
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
        trny.player2id   = HashMap() 

        trny.compIdMax   = coIdStart
        trny.comps       = HashMap()
        trny.comp2id     = HashMap()
        
        trny.clubIdMax   = clubIdStart
        trny.clubs       = HashMap()
        trny.club2id   = HashMap()
        trny.pl2co       = HashMap()
        trny.playfields  = HashMap()
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

  def getCompStatus(toId: Long, coId: Long): Future[Either[Error, CompStatus.Value]] =
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
  // Match Routines (internally used through web-client)
  // 

  def getMatchStatus(toId: Long, coId: Long, coPhId:Int, gameNo: Int): Future[Either[Error, Int]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (!trny.cophs.isDefinedAt((coId, coPhId))) Left(Error("err0222.svc.invalidCompetitionPhase")) 
                           else  Right( trny.cophs((coId, coPhId)).getMatch(gameNo).status )  
    } 

  def inputMatch(toId: Long, coId: Long, coPhId:Int, gameNo: Int, sets: (Int,Int), result: String, info: String, playfield: String, timeStamp: String, overwrite: Boolean=false): Future[Either[Error, List[Int]]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.inputMatch(coId, coPhId, gameNo, sets, result, info, playfield, timeStamp)
    } 


  def resetMatch(coId: Long, coPhId:Int, gameNo: Int, resetPantA: Boolean, resetPantB: Boolean)
                (implicit tse :TournSVCEnv): Future[Either[Error, List[Int]]] =
    TIO.getTrny(tse, true).map { 
      case Left(err)    => Left(err)
      case Right(trny)  => trny.resetMatch(coId, coPhId, gameNo)
    } 

  def resetMatches(coId: Long, coPhId:Int)(implicit tse :TournSVCEnv): Future[Either[Error,List[Int]]] =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.resetMatches(coId, coPhId)
    } 


  // 
  // Match X Routines (internally used through external client)
  // 
  def setXMatch(ma: MEntry)(implicit tse :TournSVCEnv): Future[Either[Error,Boolean]] = 
    //def prt(msg: String) = logger.info(s"setMatch: ${msg}")
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.cophs.isDefinedAt((ma.coId, ma.coPhId))) {

          val trigCmd = ma.coPhTyp match {
            case CompPhaseTyp.GR => {
              val maGr = ma.asInstanceOf[MEntryGr]
              UpdateTrigger("CompPhase", "000000", tse.toId, maGr.coId, maGr.coPhId, maGr.grId)
            }  
            case CompPhaseTyp.KO => UpdateTrigger("CompPhase", "000000", tse.toId, ma.coId, ma.coPhId, 0)
            case _               => UpdateTrigger("Match", tse.toId)
          }

          //logger.info(s"setMatch: ${ma.status} ${ma.coId} ${ma.coPh} ${ma.result.mkString(":")}")
          //logger.info(s"setMatch before: ${trny.cophs}")
          //trny.cophs((ma.coId, ma.coPh)).addMatch(ma, prt)
          
          // delete playfield, ma.playfield contains playfield code
          trny.delPlayfield(ma.coId, ma.coPhId, ma.gameNo)
          trny.cophs((ma.coId, ma.coPhId)).setModel(ma)
          //logger.info(s"setMatch after: ${trny.cophs}")
          trigger(trny.id, trigCmd)
          Right(true)
        } else {
          Right(false)
        }
      }
    }  


  def setXMatches(mas: Seq[MEntry])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] = {
    //def prt(msg: String) = logger.info(s"setMatch: ${msg}")
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        val cnt = for {
          ma <- mas
        } yield {
          if (trny.cophs.isDefinedAt((ma.coId, ma.coPhId))) {
            trny.cophs((ma.coId, ma.coPhId)).setModel(ma)
            1
          }  
        }  
        trigger(trny.id, UpdateTrigger("Match", tse.toId))
        Right(cnt.length)
      }
    }  
  }    

  /** resetXMatches delete all match results of a certain competition phase
    * 
    * @param coId
    * @param coPh
    * @param tse
    * @return true if matches of competition phase was deleted otherwise false
    */

  def resetXMatches(coId: Long, coPhId: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.cophs.isDefinedAt((coId, coPhId))) {
          trny.cophs((coId, coPhId)).resetResults()
          trigger(trny.id, UpdateTrigger("MatchReset", tse.callerIdent, tse.toId, coId, coPhId, 0))
          Right(true)
        } else {
          Right(false)
        }
      }
    }


  // 
  // Referee Routines
  //     
  def getRefereeNote(toId: Long, coId: Long, coPhId:Int, gameNo: Int): Future[Either[Error, RefereeNote]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (trny.cophs.isDefinedAt((coId, coPhId))) {
          try {
            val m = trny.cophs((coId, coPhId)).matches(gameNo-1)
            val playerInfoA = SNO(m.stNoA).getInfo(m.coTyp)(trny) 
            val playerInfoB = SNO(m.stNoB).getInfo(m.coTyp)(trny)

            Right(RefereeNote(
              trny.organizer, trny.name, toId,
              trny.comps(coId).name, coId,
              trny.cophs((coId,coPhId)).name, coPhId,
              gameNo, m.finished,
              trny.cophs((coId, coPhId)).noWinSets,
              playerInfoA._2, playerInfoB._2, 
              playerInfoA._3, playerInfoB._3,
              m.getBalls.to(List)
            ))
          } catch { case _: Throwable => Left(Error("err0219.svc.getReferee.invalid")) }
        } else {
          Left(Error("err0218.svc.getReferee.notFound"))   
        }  
      }
    } 



  // 
  // Competition Phase Routines
  //
  def addCompPhase(coId: Long, name: String)(implicit tse :TournSVCEnv): Future[Either[Error, CompPhase]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.addCompPhase(coId, name) match {
        case Left(err)   => Left(err)
        case Right(coph) => {
          if (tse.trigger) trigger(trny.id, UpdateTrigger("CompPhase", tse.callerIdent, tse.toId, coph.coId, 0))
          Right(coph)
        }
      }
    }

  def updateCompPhaseStatus(coId: Long, coPhId: Int, status: CompPhaseStatus.Value)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.updateCompPhaseStatus(coId, coPhId, status) match {
        case Left(err)   => Left(err)
        case Right(res)  => {
          if (tse.trigger) trigger(trny.id, UpdateTrigger("CompPhase", tse.callerIdent, tse.toId, coId, 0))
          Right(res)
        }
      }
    }


  def delCompPhase(coId: Long, coPhId: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Unit]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.delCompPhase(coId, coPhId) match {
        case Left(err)    => Left(err)
        case Right(res)   => Right({})
      }
    }

  /** setCompPhase - set competition phases 
   */
  def setCompPhase(coph: CompPhase)(implicit tse :TournSVCEnv): Future[Either[Error, Unit]]  =
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.setCompPhase(coph)
    } 

  /** delCompPhases - reset competition phases  
   */
  def delCompPhases(coId: Long=0)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        if (coId == 0) trny.cophs = HashMap()
        else for ( (key, coph) <- trny.cophs ) if (coph.coId == coId) trny.cophs.remove(key)
        if (tse.trigger) trigger(trny.id, UpdateTrigger("CompPhase", tse.callerIdent, tse.toId, coId, 0))
        Right(true)
      }
    }     

  def pubCompPhase(coId: Long, coPhId: Option[Int])(implicit tse :TournSVCEnv): Future[Either[Error, Unit]] = 
    TIO.getTrny(tse, true).map {
      case Left(err)    => Left(err)
      case Right(trny)  => trny.pubCompPhase(coId, coPhId)
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
  def delTourney(toId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, String]] = TIO.delete(toId, tse.orgDir)
  def delTourney(sDate: Int)(implicit tse :TournSVCEnv): Future[Either[Error, String]] = TIO.delete(0L, tse.orgDir, sDate)

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

  /** regTournBase register a tourney from a tournBase, creates new one if trnyBase.id == 0
    * returns id of (new) tourney 
    * returns error if existing tourney.id does not match trnyBase.id or violating access rights 
    */    
  def regTournBase(trnyBase: TournBase)(implicit tse :TournSVCEnv): Future[Either[Error, Long]] = {
    if (trnyBase.orgDir != tse.orgDir) {
      Future(Left(Error("err0080.access.invalidRights")))
    } else if (trnyBase.id != 0) {
      TIO.load(trnyBase.orgDir, trnyBase.startDate).map { 
        case Left(err)   => Left(Error("err0178.svc.regTournBase", s"orgDir: ${trnyBase.orgDir} startDate: ${trnyBase.startDate.toString}"))
        case Right(trny) => if (trny.id != trnyBase.id) Left(Error("err0178.svc.regTournBase", s"invalid tourney identificator: ${trny.id} / ${trnyBase.id}")) else Right(trny.id) 
      }
    } else {
      TIO.load(trnyBase.orgDir, trnyBase.startDate).flatMap { 
        case Left(err)   => TIO.add(trnyBase).map {
          case Left(err)   => Left(err.add("regTournBase"))
          case Right(trny) => Right(trny.id) 
        }                               
        case Right(trny) => Future( Right(trny.id) )
      }
    }
  } 


  /** saveTourney put tourney data to disk
    * 
    */
  def saveTourney(toId: Long)(implicit tse :TournSVCEnv): Either[Error, Boolean] = TIO.save(toId) 

  /** syncTourney replace tourney with new data, put tourney data to disk
    * 
    */  
  def syncTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] = 
    TIO.add(trny).map {
      case Left(err)    => Left(err)
      case Right(nTrny) => TIO.save(nTrny.id)       
    }


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
          trny.typ     = TourneyTyp(tb.typ)
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

  /** updTournCTT sets tourney information from ClickTT
    * 
    */     
  def updTournCTT(ctt: CttTournament, toId: Long)(implicit  msgs: Messages, tse :TournSVCEnv): Future[Either[Error,Seq[(Long, Int)]]] =
    TIO.update(ctt, toId)

  /** addTournCTT sets tourney information from ClickTT
    * 
    */     
  def addTournCTT(ctt: CttTournament, orgDir: String, organizer: String)(implicit  msgs: Messages, tse :TournSVCEnv): Future[Either[Error, Tourney]] = 
    TIO.add(ctt, orgDir, organizer)

                  
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

  def getTourney(toId: Long): Future[Either[Error, Tourney]] = 
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny)
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

  def getTournStartdate(toId: Long): Future[Either[Error, Int]] =
    TIO.get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => Right(trny.startDate)
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

  def dump(toId: Long):  Future[Either[Error, String]] =
    TIO.get(toId).map {
      case Left(err)   => Left(err)
      case Right(trny) => Right(trny.toJson(2))
    }
 

  def trigger(toId: Long, trigger: UpdateTrigger): Unit = {
    import controllers.{ EventActor, ActorRefManager }
    logger.info(s"trigger -> toId: ${toId}  trigger: ${trigger}")
    EventActor.manager ! ActorRefManager.MessageToId(toId, trigger.toString) 
  }

  def trigger(tse: TournSVCEnv, trigCmd: String): Unit = {
    import controllers.{ EventActor, ActorRefManager }
    EventActor.manager ! ActorRefManager.MessageToId(tse.toId, UpdateTrigger(trigCmd, tse.callerIdent, tse.toId, 0L, 0, 0).toString)
  }

}