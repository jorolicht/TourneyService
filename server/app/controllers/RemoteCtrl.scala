package controllers

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}   

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.nio.charset.StandardCharsets._
import javax.inject._

// imports for Silhouette
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{ LogoutEvent, Silhouette }
import utils.auth.DefaultEnv

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent._
import scala.util._
import sys.process._

import play.api.{ Environment, Configuration, Logging }
import play.api.inject.ApplicationLifecycle
import play.api.http.ContentTypes
import play.api.libs.EventSource
import play.api.libs.mailer._
import play.api.libs.json._
import play.api.mvc._
import play.api.i18n._

import models.daos.{ TourneyDAO, LicenseDAO }
import models.User
import shared.model._
import shared.model.tabletennis._
import shared.utils.Routines._
import shared.utils._
import tourn.services._

/** Controller for services regarding Competitions, Players, Playfields, ...
  */
@Singleton
class RemoteCtrl @Inject()
( 
  coco:       ControllerComponents,
  lc:         ApplicationLifecycle,
  licDao:     LicenseDAO,
  mailer:     MailerClient,
  langs:      Langs,
  silhouette: Silhouette[DefaultEnv]
)(
  implicit
  ec:           ExecutionContext,
  env:          Environment,
  cfg:          Configuration,
  tsv:          TourneyService,
  tourneyDao:   TourneyDAO,
  assetsFinder: AssetsFinder
)    
  extends AbstractController(coco) with I18nSupport with Logging
{

  implicit val txFormat   = Json.format[shared.model.TournBase]
  implicit val grtxFormat = Json.format[shared.model.tabletennis.GroupTx]
  implicit val kotxFormat = Json.format[shared.model.tabletennis.KoRoundTx]
  implicit val maFormat   = Json.format[shared.model.tabletennis.Matches]
  implicit val cptxFormat = Json.format[shared.model.CompPhaseTx]


  /** trigger - triggers update of connected clients
   * 
   */
  def trigger(toId: Long, cmd: String) = Action.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
 
    val tse = TournSVCEnv(toId, ctx.orgDir,true)

    tsv.trigger(tse, cmd)
    Future(Ok(true.toString))
  } 


  /** save - triggers save of all tourney data to disk
   * 
   */
  def save(toId: Long) = Action { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    implicit val tse = TournSVCEnv(toId, ctx.orgDir, true)

    logger.debug(s"save ${ctx.orgId} ${ctx.orgDir}")
    tsv.saveTourney(toId) match {
      case Left(err)  => BadRequest(err.add("save").encode)
      case Right(res) => Ok(Return(res).encode)
    }
  }  


  def getTournBase(toId: Long) = Action.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
 
    logger.debug(s"getTournBase ${ctx.orgId} ${ctx.orgDir}")
    tourneyDao.findByPathId(ctx.orgDir, toId).map { _ match {
      case Some(tourney) => Ok(Json.toJson(tourney))
      case None          => BadRequest(Error("err0151.ctrl.getTournBase", "" , "", "getTournBase").encode) 
    }}
  } 


  def regTournBase() = Action(parse.json[TournBase]).async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val tb  = request.body
    val ctx =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"regTournBase  ${ctx.orgId} ${ctx.orgDir}  ${ctx.organizer}  ")

    if (ctx.orgId <= 0 ) {
      logger.info(s"regTournBase: not allowed to create tourney DB")
      Future( BadRequest(Error("err0152.ctrl.regTournBase", "not allowed", "","regTournBase").encode) ) 
    } else {
     
      val tBase = if (tb.organizer == "") {
        tb.copy(orgDir=ctx.orgDir, organizer=ctx.organizer)
      } else {
        tb.copy(orgDir=ctx.orgDir)
      }
      implicit val tse   = TournSVCEnv(tBase.id, ctx.orgDir, true)
      tsv.regTournBase(tBase).map {
        case Left(err)  => {
          logger.error(s"regTournBase:  ${err.encode}")
          BadRequest(err.encode)
        }  
        case Right(id)  => Ok(id.toString)
      }
    }
  }  


  def delTourney(sDate: Int) = Action.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val tb  = request.body
    val ctx =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"delTourney -> orgId: ${ctx.orgId} orgDir ${ctx.orgDir} startDate: ${sDate}")

    if (ctx.orgId <= 0 ) {
      logger.info(s"delTourney: not allowed to delete tourney DB")
      Future( BadRequest(Error("err0152.ctrl.regTourney", "not allowed", "","delTourney").encode) ) 
    } else {
     
      implicit val tse   = TournSVCEnv(0L, ctx.orgDir, true)
      tsv.delTourney(sDate).map {
        case Left(err)  => {
          logger.error(s"delTourney ${err.encode}")
          BadRequest(err.encode)
        }  
        case Right(id)  => Ok(Return(id).encode)
      }
    }
  }  

  /** updComps - update competitions, returns list of added/changed competitions (with updated id/hashKey)
   *
   *  Service: def updComps(comps: Seq[Competition])(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Seq[Competition]]]
   */  
  def updComps(toId: Long, trigger: Boolean=false) = Action.async { implicit request: Request[AnyContent] =>
    val coSeqText: String    = request.body.asText.getOrElse("")
    val msgs:      Messages  = messagesApi.preferred(request)
    val ctx    =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, true)

    logger.info(s"updComps -> toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    try {
      val coSeq = read[Seq[Competition]](coSeqText)
      tsv.updComps(coSeq).map {
        case Left(err)    => { logger.error(s"updComps -> ${err}"); BadRequest(err.encode) }
        case Right(comps) => { Ok(write[Seq[Competition]](comps)) }    
      }
    } catch { case _: Throwable => Future(BadRequest(Error("err0183.ctrl.decode.updComps").encode)) }

  }


  /** setCompStatus - set status of competition
   * 
   * Service: def setCompStatus(coId: Long, status: Int)(implicit tse: TournSVCEnv): Future[Either[Error, Boolean]] 
   */  
  def setCompStatus(toId: Long, coId: Long, status: Int, trigger: Boolean=false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, true)

    logger.info(s"setCompStatus toId:${toId} coId:${coId} status:${status} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    tsv.setCompStatus(coId, status).map {
      case Left(err)  => BadRequest(err.encode)
      case Right(res) => Ok(Return(res).encode)        
    }
  }

  /** delComps - delete competitions
   * 
   * Service: def delComps()(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
   */  
  def delComps(toId: Long, coId: Long=0, trigger: Boolean=false) = Action.async { implicit request: Request[AnyContent]=>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, true)

    logger.info(s"delComps toId:${toId} coId:${coId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    tsv.delComps().map {
      case Left(err)  => BadRequest(err.encode)
      case Right(cnt) => Ok(Return(cnt).encode)     
    } 
  }


  /** updPlayers
   *
   * register players (generates new player or updates player) with external reference
   * returns mapping list of globalPlayerId -> localPlayerId
   * 
   * Service: def updPlayers(pls: Seq[Player])(implicit tse: TournSVCEnv): Future[Either[Error, Seq[Player]]]
   */
  def updPlayers(toId: Long, trigger: Boolean=false) = Action.async { implicit request =>  
    val msgs:  Messages       = messagesApi.preferred(request)
    val playerSeqText: String = request.body.asText.getOrElse("")
    val ctx                   = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)    

    logger.info(s"updPlayers -> toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    try {
      val plSeq = read[Seq[Player]](playerSeqText)
      tsv.updPlayers(plSeq).map {
        case Left(err)      => { logger.error(s"updPlayers -> ${err}"); BadRequest(err.encode) }
        case Right(players) => { Ok(write[Seq[Player]](players)) }    
      }
    } catch { case _: Throwable => Future(BadRequest(Error("err0184.ctrl.decode.updPlayers").encode)) }

  }

  /** delPlayers
   *  
   * Service: def delPlayers(tse: TournSVCEnv): Future[Either[Error, Int]]
   */
  def delPlayers(toId: Long, trigger: Boolean=false) = Action.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx      = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    val tse   = TournSVCEnv(toId, ctx.orgDir, trigger) 

    logger.debug(s"delPlayers toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    tsv.delPlayers(tse).map {
      case Left(err)  => BadRequest(err.encode)
      case Right(cnt) => Ok(Return(cnt).encode) 
    }
  }


  /** setClubs
   * register clubs (generates new club or updates club) with external reference
   * returns map of references (globalClubId -> externalClubId)
   */
  def setClubs(toId: Long, trigger: Boolean=false) = Action.async { implicit request =>
    val msgs       : Messages = messagesApi.preferred(request)
    val seqClubJson: String   = request.body.asText.getOrElse("")
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"setClubs toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)  
    try {
      val p2cSeq = read[Seq[Club]](seqClubJson)
      tsv.setClubs(p2cSeq).map {
        case Left(err)  => BadRequest(err.encode)
        case Right(res) => Ok(write[Seq[Club]](res)) 
      }
    } catch { case _: Throwable => Future(BadRequest(Error("err0186.ctrl.decode.setClubs").encode)) }
  }

  /** delClubs
   *  register clubs (generates new club or updates club) with external reference
   *  returns map of references (globalClubId -> externalClubId)
   *  Service: def delClubs()(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
   */
  def delClubs(toId: Long, trigger: Boolean=false) = Action.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"delClubs toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)  
    
    tsv.delClubs().map {
      case Left(err)  => BadRequest(err.encode)
      case Right(cnt) => Ok(Return(cnt).encode)      
    }   
  }


  /** setParticipant2Comps
   *
   * set/register participants to one or more competitions returns number of mapped entries
   */
  def setParticipant2Comps(toId: Long, trigger: Boolean = false) = Action.async { implicit request =>    
    val msgs      : Messages  = messagesApi.preferred(request)
    val p2cSeqText: String    = request.body.asText.getOrElse("")
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"setParticipant2Comps toId:${toId} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    //setup implicit service call environment 
    //def setParticipant2Comps(p2cs: Seq[Participant2Comp])(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
    implicit val tse = TournSVCEnv(toId, ctx.orgDir, true)

    try {
      val p2cSeq = read[Seq[Participant2Comp]](p2cSeqText)
      tsv.setParticipant2Comps(p2cSeq).map {
        case Left(err)  => BadRequest(err.encode)
        case Right(cnt) => Ok(Return(cnt).encode) 
      }
    } catch { case _: Throwable => Future(BadRequest(Error("err0185.ctrl.decode.setParticipant2Comps").encode)) }
  }


  /** delParticipant2Comps - del all participants from a competiton returns number of deleted entries
   *                         if coId=0 all entries will be deleted 
   * 
   *  Service: delParticipant2Comps(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
   */
  def delParticipant2Comps(toId: Long, coId: Long = 0, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent]=>
    val msgs: Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    
    logger.debug(s"delParticipant2Comps toId:${toId} coId:${coId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)

    tsv.delParticipant2Comps(coId).map {
      case Left(err)  => BadRequest(err.encode)
      case Right(cnt) => Ok(Return(cnt).encode)
    }
  }


  /** setPlayfields
   *
   * resets, sets or updates playfields (depending on param) according to json playfield list
   * checks authorisation (i.e. tourney(toId) belongs to organisator(ctx.orgDir))
   * returns true or false 
   * 
   * Service: def setPlayfields(pfs: Seq[Playfield])(implicit tse :TournSVCEnv) : Future[Either[Error, Int]] 
   */  
  def setPlayfields(toId: Long, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent]  =>
    val msgs: Messages  = messagesApi.preferred(request)
    val reqData = request.body.asText.getOrElse("")
    val ctx     = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs) 

    logger.debug(s"setPlayfields toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    
    Playfield.decSeq(reqData) match {
      case Left(err)    => Future(BadRequest(err.encode))
      case Right(pfSeq) => tsv.setPlayfields(pfSeq).map {
        case Left(err)   => BadRequest(err.encode)
        case Right(cnt)  => Ok(Return(cnt).encode)
      }    
    } 
  } 

  /** delPlayfields delete all playfields returns number of deleted entries
    * Service: def delPlayfields()(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
    *
    * @param toId
    * @param trigger
    */
  def delPlayfields(toId: Long, trigger: Boolean = false) = Action.async { implicit request =>
    val msgs: Messages  = messagesApi.preferred(request)
    val ctx   = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs) 

    logger.debug(s"delPlayfields toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}") 

    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    
    tsv.delPlayfields().map {
      case Left(err)   => BadRequest(err.encode)
      case Right(cnt)  => Ok(Return(cnt).encode)
    }
  } 


  /** setMatches 
   * 
   *  Serivce: def setMatches(ma: Seq[MatchEntry])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] 
   */ 
  def setMatches(toId: Long, trigger: Boolean = false) = Action(parse.json[Matches]).async { implicit request =>
    val msgs:    Messages  = messagesApi.preferred(request)
    val matches  = request.body
    val ctx      = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)

    logger.debug(s"setMatches toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    tsv.setMatches(matches.list.map(x => MatchEntry.obify(x))).map {
      case Left(err)   => BadRequest(err.encode)
      case Right(cnt)  => Ok(Return(cnt).encode)  
    }
  } 

  /** setMatch 
   * 
   *  Service: def setMatch(ma: MatchEntry)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]
   */ 
  def setMatch(toId: Long, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)

    val mEntry = request.body.asText.getOrElse("")
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    logger.debug(s"setMatch toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir} body:${mEntry}")

    tsv.setMatch(MatchEntry.obify(mEntry)).map {
      case Left(err)      => BadRequest(err.encode)
      case Right(result)  => Ok(Return(result).encode)       
    } 
  } 


  /** delMatches
   *  input parameter encoded in request body :
   *  <competition Identification> : <competition Phase Identification>
   * 
   *  Service: def delMatches(coId: Long, coPh: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] 
   */ 
  def delMatches(toId: Long, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:    Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    val param  = Crypto.encParam(request.body.asText.getOrElse("") )
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    
    val coId = param("coId").toLong
    val coPh = param("coPh").toInt

    logger.debug(s"delMatches toId:${toId} coId:${coId} coPh:${coPh} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    tsv.delMatches(coId, coPh).map {
      case Left(err)   => BadRequest(err.encode)
      case Right(cnt)  => Ok(Return(cnt).encode)       
    }    
  } 

  /** setCompPhase - set a competition phase
   * 
   * Service def setCompPhase(coph: CompPhase)(implicit tcp :TournSVCEnv): Future[Either[Error, Boolean]] 
   */
  def setCompPhase(toId: Long, trigger: Boolean = false) = Action(parse.json[CompPhaseTx]).async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val coph  = request.body
    val ctx   = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"setCompPhase toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    tsv.setCompPhase(CompPhase.fromTx(coph)).map {
      case Left(err)  => BadRequest(err.encode)
      case Right(res) => Ok(Return(res).encode) 
    }
  }

  /** delCompPhases reset competition phases
   *
   *  Service: delCompPhases(coId: Long=0)(implicit tcp :TournSVCEnv): Future[Either[Error, Boolean]]
   */
  def delCompPhases(toId: Long, coId: Long = 0L, trigger: Boolean=false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val coph  = request.body
    val ctx   = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"delCompPhases toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    tsv.delCompPhases(coId).map {
      case Left(err)  => BadRequest(err.encode)
      case Right(res) => Ok(Return(res).encode)       
    }
  }

}