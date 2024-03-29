package controllers

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.nio.charset.StandardCharsets
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
import shared.utils.Routines._
import shared.utils._
import tourn.services._

/** Controller for services regarding Competitions, Players, Playfields, ...
  */
@Singleton
class FileCtrl @Inject()
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

  def chkAccess(ctx: shared.utils.Session): Boolean = (ctx.orgId > 0) | !Crypto.accessCtrl

  def downloadFile(params: String="") = Action.async { implicit request =>
    val msgs:  Messages   = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    val param  = Crypto.encParam(params)

    val toId    = param.getOrElse("toId", "0").toLong
    val dloType = DownloadType(param.getOrElse("dloType","0").toInt)
    logger.info(s"downloadFile -> dloType:${dloType} orgDir:${ctx.orgDir} toId: ${toId}")

    if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights", "", "", "downloadFile").encode)) else {
      dloType match {
        case DownloadType.UNKNOWN   => Future(BadRequest(Error("err0210.downloadFile.invalidType", "", "", "downloadFile").encode)) 
        case DownloadType.Player => {
          Future(Ok("Registrierte Teilnehmer ...").withHeaders("Content-Disposition" -> s"attachment; filename=${msgs(s"download.typ.${dloType.id}")} "))
        }
        case DownloadType.ClickTT => tsv.getTournStartdate(toId).map {
          case Left(err)    => BadRequest(err.encode)
          case Right(sDate) => {
            try {
              val tourneyDir     = s"${env.rootPath}${File.separator}db${File.separator}Tourneys${File.separator}${ctx.orgDir}"
              val cttResPath     = Paths.get(s"${tourneyDir}${File.separator}${sDate}_${toId}_result.xml")
              //BadRequest(Error("err0211.downloadFile.missingResultFile", s"${sDate}_${toId}_result.xml").encode)
              Ok(Files.readAllBytes(cttResPath)).withHeaders("Content-Disposition" -> s"attachment; filename=${msgs(s"download.typ.${dloType.id}")} ")
            } catch { case _: Throwable => BadRequest(Error("err0211.downloadFile.missingResultFile", s"${sDate}_${toId}_result.xml").encode) }
          }
        }
      }
    }   
  } 

  /**  getInvitation - deliver configuration/invitation file for a tourney (unique identified by orgDir and startDate)
    */
  def getInvitation(orgDir: String, startDate: Int) = Action.async { implicit request =>
    import shared.utils.Constants._
    import java.nio.charset.StandardCharsets._

    val msgs:  Messages = messagesApi.preferred(request)
    val trnyDir = cfg.get[String]("server.tourney.dir")
    val baseDir = s"${trnyDir}${File.separator}${orgDir}"
    logger.info(s"getInvitation => orgDir: ${orgDir} startDir: ${startDate}")

    val path = Paths.get(s"${baseDir}${File.separator}${startDate.toString}_invitation.md")
    if (Files.exists(path)) {
      Future( Ok(new String(Files.readAllBytes(path)) )) 
    } else {
      Future( BadRequest(Error("err0035.svc.getCfgFile", s"${orgDir}${startDate.toString}").encode))
    }
  } 


  /**  provideCertTemplFile
    */
  def provideCertTemplFile(params: String) = silhouette.UserAwareAction.async { implicit request: Request[AnyContent] =>
    import shared.utils.Constants._ 
    //import java.nio.charset.StandardCharsets._

    val msgs:   Messages  = messagesApi.preferred(request)
    val ctx     = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param   = Crypto.encParam(params)
    val reqdata = request.body.asText.getOrElse("")

    val orgDir    = param("orgDir")
    
    logger.info(s"provideCertTemplFile: (${orgDir})")

    val contentDir = s"${env.rootPath}${File.separator}public${File.separator}content${File.separator}clubs"
    val certFN = Paths.get(s"${contentDir}${File.separator}${orgDir}${File.separator}certificate.png")

    if (!Files.exists(certFN)) {
      val dummyFN = Paths.get(s"${contentDir}${File.separator}dummy${File.separator}certificate.png")
      Files.copy(dummyFN, certFN, StandardCopyOption.REPLACE_EXISTING)
    } 
    Future( Ok(Return(true).encode) )
  }



  /**  genCertFile - generates a certificate for a user 
   *                 in directeory public/content/clubs/<club>/certs/certificate_<toId>_<coId>_<sno>.html
   */
  def genCertFile(params: String) = Action.async { implicit request: Request[AnyContent] =>
    import shared.utils.Constants._
    import tourn.services.Crypto._
    import cats.data.EitherT

    //import java.nio.charset.StandardCharsets

    val msgs:   Messages  = messagesApi.preferred(request)
    val ctx     = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param   = Crypto.encParam(params)
    val reqdata = request.body.asText.getOrElse("")

    // val orgDir  = param("orgDir")
    // val toId    = param("toId").mkString.toLong
    // val coId    = param("coId").mkString.toLong
    // val sno     = param("sno")
    // val tTyp    = param("tTyp")

   // verify necessary parameters
    val (pMap, orgDir, toId, coId, sno, tTyp, valid) = (for {
      pM       <- encEParam(params)
      orgDir   <- getEParam(pM, "orgDir")
      toId     <- getEParam(pM, "toId", 0L)
      coId     <- getEParam(pM, "coId", 0L)
      sno      <- getEParam(pM, "sno")
      tTyp     <- getEParam(pM, "tTyp", 0)
    } yield { (pM, orgDir, toId, coId, sno, tTyp) }) match {
      case Left(err)  => (new scala.collection.mutable.HashMap[String, String], "error", 0L, 0L, "", TourneyTyp.UNKN, false)
      case Right(res) => (res._1, res._2, res._3, res._4, res._5, TourneyTyp(res._6), true)  
    }

    if (!valid) { 
      logger.error(s"genCertFile invalid parameter")
      Future( Ok(Return(false).encode) ) 
    } else {
      val contentDir = s"${env.rootPath}${File.separator}public${File.separator}content${File.separator}clubs"
      val certPic    = Paths.get(s"${contentDir}${File.separator}${orgDir}${File.separator}certificate.png")
      val certDir    = s"${contentDir}${File.separator}${orgDir}${File.separator}certs"
      val certHtml   = s"${certDir}${File.separator}Certificate_${toId}_${coId}_${sno}.html"

      // take club specific certificate file else take standard file for this tourney category
      val certData = if (Files.exists(certPic)) {
        reqdata.replace("CERTIFICATE_FILE", s"${orgDir}${File.separator}certificate.png")
      } else {  
        reqdata.replace("CERTIFICATE_FILE", s"dummy${File.separator}${tTyp.toString}_certificate.png")
      }  

      if (!Files.exists(Paths.get(certDir))) Files.createDirectories(Paths.get(certDir))
      Files.write(Paths.get(certHtml), certData.getBytes(StandardCharsets.UTF_8))
      Future( Ok(Return(true).encode) )
    }  
  }
 

  /**  uploadFile calculate file name (for saveing) from additional
    *  parameter "uptype"
    */
  def uploadFile(params: String="") = Action(parse.multipartFormData) { implicit request =>
    val msgs:  Messages   = messagesApi.preferred(request)

    // verify necessary parameters
    val (pMap, toId, sDate, uplType) = (for {
      pM          <- Crypto.encEParam(params)
      toId        <- Crypto.getEParam(pM, "toId", 0L)
      sDate       <- Crypto.getEParam(pM, "sDate")
      uplTypeVal  <- Crypto.getEParam(pM, "uplType", 0)
    } yield { (pM, toId, sDate, uplTypeVal) }) match {
      case Left(err)  => (new scala.collection.mutable.HashMap[String, String], 0L, "", UploadType.UNKNOWN)
      case Right(res) => (res._1, res._2, res._3, UploadType(res._4))  
    }

    val ctx          = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))
    implicit val tse = TournSVCEnv(toId, ctx.orgDir, true)
    
    logger.info(s"uploadFile: toId:${toId} uplType: ${uplType} orgDir: ${ctx.orgDir}")
    
    // set content (club specific) and tourney directories 
    val tourneyDir = s"${env.rootPath}${File.separator}db${File.separator}Tourneys${File.separator}${ctx.orgDir}"
    val contentDir = s"${env.rootPath}${File.separator}public${File.separator}content${File.separator}clubs${File.separator}${ctx.orgDir}"

    if (ctx.orgId <= 0) {
      BadRequest(Error("err0202.upload.file.orgId").encode)
    } else {
      // generate path if necessary     
      Files.createDirectories(Paths.get(s"${tourneyDir}${File.separator}tmp${File.separator}"))
      request.body.file("file")
        .map( file => {
          // only get the last part of the filename
          // otherwise someone can send a path like ../../home/foo/bar.txt 
          // to write to other files on the system

          val filename    = Paths.get(file.filename).getFileName
          val tmpFilename = s"${tourneyDir}${File.separator}tmp${File.separator}${filename}"
          val fileSize    = file.fileSize
          val UploadSizeLimit = 3000000
          if   (fileSize > UploadSizeLimit) BadRequest(Error("err0200.upload.file.limit").encode) 
          else {
            file.ref.copyTo(Paths.get(s"${tourneyDir}${File.separator}tmp${File.separator}${filename}"), replace = true)
            processUpload(ctx.orgDir, ctx.organizer, tourneyDir, contentDir, tmpFilename, uplType, toId, sDate)
          }})
        .getOrElse( BadRequest(shared.utils.Error("err0203.upload.file.command").encode) )
    } 
  }  


  def processUpload(orgDir: String, organizer: String, tourneyDir: String, contentDir: String, filename: String, uplType: UploadType.Value, toId: Long, sDate: String)
                   (implicit msgs: Messages, tse : TournSVCEnv): play.api.mvc.Result = {
    import shared.utils.Constants._  

    val fNameArr = filename.split("\\.")
    val fExt     = if (fNameArr.length > 1) fNameArr(fNameArr.length-1).toLowerCase else ""
    
    logger.info(s"processUpload: uplType: ${uplType} tourneyDir: ${tourneyDir} contentDir: ${contentDir} filename: ${filename} fExt: ${fExt}")

    if (!UploadType.checkExtension(fExt, uplType)) BadRequest(Error("err0201.upload.file.notsupported").encode) 
    else uplType match {
      case UploadType.Invite => { // invitation file
        Files.copy(Paths.get(filename), Paths.get(s"${tourneyDir}${File.separator}${sDate}_invitation.md"), StandardCopyOption.REPLACE_EXISTING)
        Ok(Return(toId).encode)
      }
      case UploadType.Logo => { // logo picture/file
        Files.copy(Paths.get(filename), Paths.get(s"${tourneyDir}${File.separator}logo.${fExt}"), StandardCopyOption.REPLACE_EXISTING)
        Ok(Return(toId).encode)  
      }
      case UploadType.Cert => { // certificate picture/file
        val desFN  = Paths.get(s"${tourneyDir}${File.separator}${sDate}_certificate.${fExt}")
        val contFN = Paths.get(s"${contentDir}${File.separator}certificate.${fExt}")

        Files.copy(Paths.get(filename), desFN, StandardCopyOption.REPLACE_EXISTING)
        Files.copy(Paths.get(filename), contFN, StandardCopyOption.REPLACE_EXISTING)
        Ok(Return(toId).encode)  
      }
      case UploadType.Banner => { // club banner  picture/file
        val contFN = Paths.get(s"${contentDir}${File.separator}banner.${fExt}")
        Files.copy(Paths.get(filename), contFN, StandardCopyOption.REPLACE_EXISTING)
        Ok(Return(toId).encode)   
      }
      case UploadType.UNKNOWN => BadRequest(Error("err0128.upload.file", "invalid extension type").encode)
    }
  }


  /**  sendCttFile calculate file name (for saveing) from additional
    *  parameter "uptype"
    */
  def sendCttFile(params: String="") = Action.async(parse.multipartFormData) { implicit request =>
    import shared.utils.Constants._ 
    import upickle.default._
    
    val msgs:  Messages   = messagesApi.preferred(request)

    //logger.info(s"sendCttFile")

    // verify necessary parameters
    val (pMap, toId, sDate, mode) = (for {
      pM     <- Crypto.encEParam(params)
      toId   <- Crypto.getEParam(pM, "toId", 0L)
      sDate  <- Crypto.getEParam(pM, "sDate")
      mode   <- Crypto.getEParam(pM, "uplMode", 0)
    } yield { (pM, toId, sDate, mode) }) match {
      case Left(err)  => (new scala.collection.mutable.HashMap[String, String], 0L, "", 0)
      case Right(res) => (res._1, res._2, res._3, res._4)  
    }

    val uplMode = UploadMode(mode)

    val ctx           = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))
    implicit val tse  = TournSVCEnv(toId, ctx.orgDir, true)
    
    logger.info(s"sendCttFile: toId:${toId} uplMode: ${uplMode.id} orgDir: ${ctx.orgDir}")
    
    // set content (club specific) and tourney directories 
    val tourneyDir = s"${env.rootPath}${File.separator}db${File.separator}Tourneys${File.separator}${ctx.orgDir}"
    val contentDir = s"${env.rootPath}${File.separator}public${File.separator}content${File.separator}clubs${File.separator}${ctx.orgDir}"

    if (ctx.orgId <= 0) {
      Future( BadRequest(Error("err0202.upload.file.orgId").encode) )
    } else {
      // generate path if necessary     
      Files.createDirectories(Paths.get(s"${tourneyDir}${File.separator}tmp${File.separator}"))
      
      request.body.file("file").map { file => 
        // only get the last part of the filename
        // otherwise someone can send a path like ../../home/foo/bar.txt 
        // to write to other files on the system

        val filename = Paths.get(file.filename).getFileName
        val desFN    = s"${tourneyDir}/${sDate}_${toId}_participants.xml"
        val fileSize = file.fileSize
        val UploadSizeLimit = 3000000
        if   (fileSize > UploadSizeLimit) { Future( BadRequest(Error("err0200.upload.file.limit").encode) ) } 
        else {
          file.ref.copyTo(Paths.get(desFN), replace = true)
          CttService.load(desFN) match {
            case Left(err)       => Future(BadRequest(err.encode))
            case Right(cttTrny)  => uplMode match {
              case UploadMode.Update    => 
                if (toId == 0) Future(BadRequest(Error("err0199.upload.file.tourneyId").encode) )
                else tsv.updTournCTT(cttTrny, toId).map { 
                  case Left(err)     => BadRequest(err.encode)
                  case Right(result) => Ok( write[Seq[(Long,Int)]] (result) ) 
                }    
              
              case UploadMode.New => tsv.addTournCTT(cttTrny, ctx.orgDir, ctx.organizer).map { 
                case Left(err)   => BadRequest(err.encode)
                case Right(trny) => Ok(write[(Long,String)] ((trny.id, trny.name)) )
              }

              case UploadMode.UNKNOWN => Future(BadRequest(Error("err0204.upload.file.mode").encode))
            }
          }
        }
      }.getOrElse { Future( BadRequest(shared.utils.Error("err0203.upload.file.command").encode) ) }
    } 
  }

}