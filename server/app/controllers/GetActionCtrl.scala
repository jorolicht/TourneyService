package controllers

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.nio.charset.StandardCharsets._
import javax.inject._

// imports for Silhouette
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{ LogoutEvent, Silhouette }
import utils.auth.DefaultEnv

//import SQLiteProfile to connect to sqlite database
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
import shared.utils._
import tourn.services._

/** Controller for services regarding Competitions, Players, Playfields, ...
  */
@Singleton
class GetActionCtrl @Inject()
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

  val mailService  = new MailerService(mailer)

  implicit val matchFormat  = Json.format[Matches]
  implicit val kotxFormat   = Json.format[KoRoundTx]
  implicit val grtxFormat   = Json.format[GroupTx]
  implicit val cophFormat   = Json.format[CompPhaseTx]
  implicit val resFormat    = Json.format[shared.model.tabletennis.ResultEntrys]


  // ---
  // Service based on getAction wrapper 
  // ---
  def getAction(params: String) = silhouette.UserAwareAction.async { implicit request =>
    import tourn.services.Crypto._
    import upickle.default._
    import java.util.Date

    val msgs:  Messages   = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 

    // val date = new Date()

    // verify necessary parameters
    val (pMap, cmd, toId) = (for {
      pM    <- Crypto.encEParam(params)
      cmd   <- Crypto.getEParam(pM, "cmd")
      toId  <- Crypto.getEParam(pM, "toId", 0L)
    } yield { (pM, cmd, toId) }) match {
      case Left(err)  => (new scala.collection.mutable.HashMap[String, String], "error", 0L)
      case Right(res) => (res._1, res._2, res._3)  
    }

    implicit val tse  = TournSVCEnv(toId, ctx.orgDir, true)

    logger.info(s"getAction -> Command: ${cmd}")
    cmd match {

      case "ping" => {
        val addr = request.remoteAddress
        Future(Ok(s"Address: ${addr}"))
      }

      case "getTournCfg" => tsv.getTournCfg(toId).map {
        case Left(err)   => logger.error(s"getTournCfg error: ${err.toString}"); BadRequest(err.add("getTournCfg").encode)
        case Right(trny) => {
          // logger.info(s"getTournCfg -> Id: ${trny.id} Time: ${date.getTime()}")
          Ok(trny.encode())
        }  
      }
  
      case "getTournRun" => tsv.getTournRun(toId).map {
        case Left(err)   => logger.error(s"getTournRun error: ${err.toString}"); BadRequest(err.add("getTournRun").encode)
        case Right(tRun) => {
          // logger.info(s"getTournRun -> Id: ${tRun.id} Time: ${date.getTime()}")
          Ok(tRun.encode())
        }  
      }

      case "findTournBases"   =>  
        tourneyDao.findByStringTypDate(getParam(pMap, "search"), getParam(pMap, "toTyp", 0), getParam(pMap, "toYear", 1970)).map { tBases => {
          // logger.info(s"findByStringTypDate -> ${trnyBases}")
          Ok(TournBase.encSeq(tBases)) 
        }} 
            
      case "getComps"     => tsv.getComps(toId).map { 
        case Left(err)    => logger.error(s"getComps error: ${err.toString}"); BadRequest(err.add("getComps").encode)
        case Right(comps) => Ok(Competition.encSeq(comps)) 
      }


      /** getComp
       * 
       *  Service: def getComp(toId: Long, coId: Long): Future[Either[Error, Competition]]
       */
      case "getComp"   => tsv.getComp(toId, getParam(pMap, "coId", -1L) ).map { 
        case Left(err) => logger.error(s"getComp error: ${err.toString}"); BadRequest(err.add("getComp").encode)
        case Right(co) => Ok(co.encode())
      }


      // getPlayfields returns encoded Playfields or Error
      case "getPlayfields"  => tsv.getPlayfields(toId).map { 
        case Left(err)      => logger.error(s"getPlayfields error: ${err.toString}"); BadRequest(err.add("getPlayfields").encode) 
        case Right(pfields) => Ok(Playfield.encSeq(pfields))
      }

      /** getPlayfield returns encoded playfield or Error
       * 
       *  Service: def getPlayfield(toId: Long, pfNr: Int): Future[Either[Error, Playfield]]
       */ 
      case "getPlayfield"  => tsv.getPlayfield(toId, getParam(pMap, "pfNo", -1)).map { 
        case Left(err)     => logger.error(s"getPlayfield error: ${err.toString}"); BadRequest(err.add("getPlayfield").encode) 
        case Right(pfield) => Ok(pfield.encode())
      }
  

      /** getTournPlayers
       * 
       * Service: def getTournPlayers(toId: Long): Future[Either[Error, Players]]
       */
      case "getTournPlayers"   => tsv.getTournPlayers(toId).map {
        case Left(err)      => logger.error(s"getTournPlayers error: ${err.toString}"); BadRequest(err.add("getTournPlayers").encode) 
        case Right(players) => Ok(Player.encSeq(players))
      }

      // 
      // Tourney Services 
      // 

      /** getTournClubs
       *
       *  Service: def getTournClubs(toId: Long):   Future[Either[Error, Clubs]]
       */
      case "getTournClubs"   => tsv.getTournClubs(toId).map {
        case Left(err)    => BadRequest(err.add("getTournClubs").encode) 
        case Right(clubs) => Ok(Club.encSeq(clubs))
      }      

      case "getTournBases"   => tourneyDao.findByOrgDir(getParam(pMap, "orgDir")).map { trnys => Ok(TournBase.encSeq(trnys)) }

      case "getTournBase"   => {
        tourneyDao.findById(toId).map { result => result match {
          case Some(tB) => Ok(tB.encode())
          case None     => BadRequest( Error("err0133.get.noTourneyFound", toId.toString, "", "getTournBase").encode)
        }} 
      }


      // Service: def getMatchKo(toId: Long, coId: Long, coPh:Int): Future[Either[Error, ResultEntrys]] = 
      case "getMatchKo"   => tsv.getMatchKo(toId, getParam(pMap, "coId", -1L), getParam(pMap, "coPh", -1) ).map { 
        case Left(err)       => BadRequest(err.add("getMatchKo").encode)
        case Right(rEntries) => Ok( Json.toJson(rEntries) ) 
      }  

      case "getMatchGr"   => tsv.getMatchGr(toId, getParam(pMap, "coId", -1L), getParam(pMap, "coPh", -1), getParam(pMap, "grId", -1)).map { 
        case Left(err)       => BadRequest(err.add("getMatchGr").encode)
        case Right(rEntries) => Ok( Json.toJson(rEntries) )         
      }


      // getParticipant2Comps - returns mapping list of participants to competition
      case "getParticipant2Comps"   => tsv.getParticipant2Comps(toId).map {
        case Left(err)   => BadRequest(err.add("getParticipant2Comps").encode)
        case Right(p2cs) => Ok( Participant2Comp.encSeq(p2cs) )
      }

      // getParticipantPlace delivers placement string or error
      case "getParticipantPlace" => tsv.getParticipantPlace(toId, Crypto.getParam(pMap, "coId", -1L), Crypto.getParam(pMap, "sno")).map {
        case Left(err)     => BadRequest(err.add("getParticipantPlace").encode)
        case Right(result) => Ok(result)
      } 

      // fileExists returns tru/false or error
      case "fileExists"   => {
        try {
          val fsep  = java.io.File.separator
          val dList = Crypto.getParam(pMap, "filePath").split(":").map(_.trim).mkString(s"${fsep}") 
          val file  = new java.io.File(s"${env.rootPath}${fsep}public${fsep}content${fsep}${dList}")
          Future( Ok(Return(file.exists).encode) )  
        } catch { case _: Throwable => Future( BadRequest(Error("err0053.get.fileExists", Crypto.getParam(pMap, "filePath")).encode) )}
      }

      case "error"         => {
        logger.error(s"getAction invalid arguments: ${params.toString()}") 
        Future( BadRequest(Error("err0018.get.decode", params.toString()).encode) )
      }  

      case "test"         => Future(Ok(true.toString))
    
      case _              => Future( BadRequest(Error("err0052.get.unkown",cmd).encode) )
    }  
  } 


}