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

  val langMap = for (lng <- langs.availables) yield (lng.toLocale.getLanguage, lng.toLocale.getDisplayLanguage(lng.toLocale).capitalize)

  /** trigger - triggers update of connected clients
   * 
   */
  def trigger(toId: Long, cmd: String) = Action.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    tsv.trigger(TournSVCEnv(toId, ctx.orgDir, true), cmd)
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
      case Some(tournBase) => Ok( write[TournBase](tournBase) )  
      case None            => BadRequest(Error("err0151.ctrl.getTournBase", "" , "", "getTournBase").encode) 
    }}
  } 


  def regTournBase() = Action.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val tournBase  = request.body.asText.getOrElse("")
    val ctx        = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"regTournBase  ${ctx.orgId} ${ctx.orgDir}  ${ctx.organizer}  ")

    if (ctx.orgId <= 0 ) {
      logger.info(s"regTournBase: not allowed to create tourney DB")
      Future( BadRequest(Error("err0152.ctrl.regTournBase", "not allowed", "","regTournBase").encode) ) 
    } else {
      try {
        val tb = read[TournBase](tournBase)
        implicit val tse   = TournSVCEnv(tb.id, ctx.orgDir, true)
        val tBase = if (tb.organizer == "") tb.copy(orgDir=ctx.orgDir, organizer=ctx.organizer) else tb.copy(orgDir=ctx.orgDir)

        tsv.regTournBase(tBase).map {
          case Left(err)  => BadRequest(err.encode)
          case Right(id)  => Ok(id.toString)
        }
      } catch { case _: Throwable => Future(BadRequest(Error("err0189.ctrl.decode.TournBase", tournBase.take(10)).encode)) }
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
        case Left(err)    => { logger.error(s"updComps -> Error: ${err}"); BadRequest(err.encode) }
        case Right(comps) => { 
          logger.info(s"updComps -> json: ${write[Seq[Competition]](comps)} result: ${comps.toString}"); 
          Ok(write[Seq[Competition]](comps)) 
        }    
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
    tsv.setCompStatus(coId, CompStatus(status)).map {
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


  /** setPant2Comps
   *
   * set/register participants to one or more competitions returns number of mapped entries
   */
  def setPant2Comps(toId: Long, trigger: Boolean = false) = Action.async { implicit request =>    
    val msgs      : Messages  = messagesApi.preferred(request)
    val p2cSeqText: String    = request.body.asText.getOrElse("")
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"setPant2Comps toId:${toId} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    //setup implicit service call environment 
    //def setPant2Comps(p2cs: Seq[Pant2Comp])(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
    implicit val tse = TournSVCEnv(toId, ctx.orgDir, true)

    try {
      val p2cSeq = read[Seq[Pant2Comp]](p2cSeqText)
      tsv.setPant2Comps(p2cSeq).map {
        case Left(err)  => BadRequest(err.encode)
        case Right(cnt) => Ok(Return(cnt).encode) 
      }
    } catch { case _: Throwable => Future(BadRequest(Error("err0185.ctrl.decode.setPant2Comps").encode)) }
  }


  /** delPant2Comps - del all participants from a competiton returns number of deleted entries
   *                         if coId=0 all entries will be deleted 
   * 
   *  Service: delPant2Comps(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
   */
  def delPant2Comps(toId: Long, coId: Long = 0, trigger: Boolean = false) = Action.async { implicit request =>
    val msgs: Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    
    logger.debug(s"delPant2Comps toId:${toId} coId:${coId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)

    tsv.delPant2Comps(coId).map {
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
    
    try {
      val pfSeq = read[Seq[Playfield]](reqData)
      tsv.setPlayfields(pfSeq).map {
        case Left(err)  => BadRequest(err.encode)
        case Right(cnt) => Ok(Return(cnt).encode) 
      }
    } catch { case _: Throwable => Future(BadRequest(Error("err0051.decode.Playfields").encode)) }

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


  /** setXMatches 
   * 
   *  Serivce: def setXMatches(ma: Seq[MatchEntry])(implicit tse :TournSVCEnv): Future[Either[Error, Int]] 
   */ 
  def setXMatches(toId: Long, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:    Messages  = messagesApi.preferred(request)
    val content  = request.body.asText.getOrElse("")
    val ctx      = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)

    logger.debug(s"setMatches toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    try {
      val matches = read[Seq[MEntryTx]](content).map(_.decode)
      tsv.setXMatches(matches).map {
        case Left(err)   => BadRequest(err.encode)
        case Right(cnt)  => Ok(Return(cnt).encode)  
      }
    } catch { case _: Throwable => Future(BadRequest(Error("err0191.ctrl.decode.MEntry", content.take(10)).encode)) }
  } 

  /** setXMatch 
   * 
   *  Service: def setXMatch(ma: MatchEntry)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]
   */ 
  def setXMatch(toId: Long, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val content  = request.body.asText.getOrElse("")
    val ctx      = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)  
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)  //setup implicit tourney service call environment 

    //logger.debug(s"setMatch toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir} body:${content}")
    try {
      val mEntry = read[MEntryTx](content).decode
      tsv.setXMatch(mEntry).map {
        case Left(err)      => BadRequest(err.encode)
        case Right(result)  => Ok(Return(result).encode)       
      } 
    } catch { case _: Throwable => Future(BadRequest(Error("err0191.ctrl.decode.MEntry", content.take(10)).encode)) }
  } 


  /** resetXMatches
   *  input parameter encoded in request body :
   *  <competition Identification> : <competition Phase Identification>
   * 
   *  Service: def resetXMatches(coId: Long, coPh: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]] 
   */ 
  def resetXMatches(toId: Long, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:    Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    val param  = Crypto.encParam(request.body.asText.getOrElse("") )
    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    
    val coId = param("coId").toLong
    val coPh = param("coPh").toInt

    logger.debug(s"resetXMatches toId:${toId} coId:${coId} coPh:${coPh} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")
    tsv.resetXMatches(coId, coPh).map {
      case Left(err)   => BadRequest(err.encode)
      case Right(cnt)  => Ok(Return(cnt).encode)       
    }    
  } 

  /** getReferee 
   * 
   *  Service: def getReferee returns referee note to enter result (for player)
   */ 
  def getReferee(toId: Long, coId: Long, coPhId: Int, gameNo: Int, nonce:Long, test:Boolean=false) = Action.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val lang = Messages("app.lang")
    val nonceCalc = RefereeNote.nonce(toId, coId, coPhId, gameNo)

    logger.debug(s"getReferee toId:${toId} coId:${coId} coPhId:${coPhId} gameNo:${gameNo} lang: ${lang} nonce: ${nonce} should be ${nonceCalc}")
    
    if (test) {
      val refNote = RefereeNote("TTC Freising", "Stadtmeisterschaft", toId,
                                "Herren A Klasse", coId, "Vorrunde", coPhId,
                                13, false, 3,
                                "Flötzinger, Norbert", "Lichtenegger, Robert",
                                "TTC Erdinger Str", "TTC Finkenstraße", List()) 
      Future(Ok(views.html.referee(refNote.organizer, refNote.winSets, refNote.finished, write[RefereeNote](refNote), lang, langMap)))                         
    } else {
      tsv.getRefereeNote(toId, coId, coPhId, gameNo).map {
        case Left(err)      => Ok(views.html.error(err, lang, langMap))
        case Right(refNote) => {
          if (nonceCalc.toString == nonce.toString) {
            Ok(views.html.referee(refNote.organizer, refNote.winSets, refNote.finished, write[RefereeNote](refNote), lang, langMap))
          } else {
            Ok(views.html.error(Error("err0221.invalidNonce"), lang, langMap))
          } 
        }                     
      }
    }  
  }

  /** setReferee 
   * 
   *  Service: def setReferee sets result according to the referee note
   */ 
  def setReferee(toId: Long, coId: Long, coPhId: Int, gameNo: Int, nonce: Int, test:Boolean=false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val lang = Messages("app.lang")
    val content  = request.body.asText.getOrElse("")

    val nonceCalc = RefereeNote.nonce(toId, coId, coPhId, gameNo)

    logger.debug(s"setReferee toId:${toId} coId:${coId} coPhId:${coPhId} body:${content}")

    if (nonceCalc != nonce) Future(Ok(views.html.error(Error("err0221.invalidNonce"), lang, langMap))) else {
      tsv.inputMatch(toId, coId, coPhId, gameNo, (1,3), "7.6.4.5", "hallo", "33", "", false).map {
        case Left(err)  =>  Ok(views.html.error(Error("err0221.invalidNonce"), lang, langMap)) 
        case Right(res) =>  Ok(views.html.error(Error("err0221.invalidNonce"), lang, langMap)) 
      }
    }  

  }  



  /** setCompPhase - set a competition phase
   * 
   * Service def setCompPhase(coph: CompPhase)(implicit tcp :TournSVCEnv): Future[Either[Error, Boolean]] 
   */
  def setCompPhase(toId: Long, trigger: Boolean = false) = Action.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val cophTx  = request.body.asText.getOrElse("")
    val ctx     = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)

    logger.debug(s"setCompPhase toId:${toId} trigger:${trigger} orgId:${ctx.orgId} orgDir:${ctx.orgDir}")

    //setup implicit service call environment 
    implicit val tse   = TournSVCEnv(toId, ctx.orgDir, trigger)
    CompPhase.decode(cophTx) match {
      case Left(err)   => { 
        // logger.error(s"setCompPhase: ${cophTx}")
        Future(BadRequest(err.encode))
      }  
      case Right(coph) => tsv.setCompPhase(coph).map {
        case Left(err)   => BadRequest(err.encode)
        case Right(res)  => Ok(Return(res).encode)
      }  
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