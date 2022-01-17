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

  def downloadFile(params: String="") = Action.async { implicit request =>
    val msgs:  Messages   = messagesApi.preferred(request)
    val ctx =  Crypto.getSessionFromCookie(request.cookies.get("TuSe"), msgs)
    val param  = Crypto.encParam(params)

    val toId  = param("toId").toLong
    val dType = param("dType").toLong

    logger.info(s"download: (${dType},${ctx.orgDir},${toId})")

    tsv.isAllowed(toId, ctx.orgDir).map { 
      case Left(err)  => BadRequest(err.encode)
      case Right(res) => res match {
        case true  => Ok("File download").withHeaders("Content-Disposition" -> s"attachment; filename=${msgs(s"download.typ.${dType}")} ")
        case false => BadRequest(Error("err0145.user.download").encode)
      }
    }    
  } 

  /**  getCfgFile deliver configuration file for a tourney (unique identified by orgDir and startDate)
    */
  def getCfgFile(orgDir: String, startDate: Int, fileType: String) = Action.async { implicit request =>
    import shared.utils.Constants._
    import java.nio.charset.StandardCharsets._

    val msgs:  Messages = messagesApi.preferred(request)
    val trnyDir = cfg.get[String]("server.tourney.dir")
    val baseDir = s"${trnyDir}${File.separator}${orgDir}"

    logger.info(s"getCfgFile: (${orgDir}, ${startDate}, ${fileType})")
    fileType match {
      case ULD_INVIT => {
        val path = Paths.get(s"${baseDir}${File.separator}${startDate.toString}_invitation.md")
        if (Files.exists(path)) {
          Future(Ok(new String(Files.readAllBytes(path)) )) 
        } else {
          Future( BadRequest(Error("err0035.svc.getCfgFile", s"${orgDir}${startDate.toString}").encode))
        }
      }
      case _        => Future( BadRequest(Error("err0035.svc.getCfgFile", s"${orgDir}${startDate.toString}").encode) )
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
    //import java.nio.charset.StandardCharsets

    val msgs:   Messages  = messagesApi.preferred(request)
    val ctx     = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param   = Crypto.encParam(params)
    val reqdata = request.body.asText.getOrElse("")

    val orgDir  = param("orgDir")
    val toId    = param("toId").mkString.toLong
    val coId    = param("coId").mkString.toLong
    val sno     = param("sno")

    logger.info(s"genCertFile (orgDir/toId/coId/sno: ${orgDir}/${toId}/${coId}/${sno})")

    val contentDir = s"${env.rootPath}${File.separator}public${File.separator}content${File.separator}clubs"
    val certPic = Paths.get(s"${contentDir}${File.separator}${orgDir}${File.separator}certificate.png")
    val certDir =  s"${contentDir}${File.separator}${orgDir}${File.separator}certs"
    val certHtml = s"${certDir}${File.separator}Certificate_${toId}_${coId}_${sno}.html"

    if (!Files.exists(certPic)) {
      val dummyFN = Paths.get(s"${contentDir}${File.separator}dummy${File.separator}certificate.png")
      Files.copy(dummyFN, certPic, StandardCopyOption.REPLACE_EXISTING)
    } 

    if (!Files.exists(Paths.get(certDir))) Files.createDirectories(Paths.get(certDir))
    Files.write(Paths.get(certHtml), reqdata.getBytes(StandardCharsets.UTF_8))
    Future( Ok(Return(true).encode) )
  }
 

  /**  uploadFile calculate file name (for saveing) from additional
    *  parameter "uptype"
    */
  def uploadFile(params: String="") = Action.async(parse.multipartFormData) { implicit request =>
    val msgs:  Messages   = messagesApi.preferred(request)
    val param     = request.body.asFormUrlEncoded
    val toId      = param("toId").mkString.toLong
    val uptype    = param("uptype").mkString.toInt
    val sdate     = param("sdate").mkString.toInt
    val edate     = param("edate").mkString.toInt
    val ctx       = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))

    implicit val tse  = TournSVCEnv(toId, ctx.orgDir, true)

    // generate path if necessary
    if (ctx.orgId <= 0) {
      Future( BadRequest(Error("err0128.upload.file", "invalid orgId").encode) )
    } else {  
      val baseDir = s"${env.rootPath}${File.separator}db${File.separator}Tourneys${File.separator}${ctx.orgDir}"
      val contentDir = s"${env.rootPath}${File.separator}public${File.separator}content${File.separator}clubs${File.separator}${ctx.orgDir}"
      logger.info(s"upload: (${uptype}/${ctx.orgDir}/${sdate}/${edate})")
      Files.createDirectories(Paths.get(s"${baseDir}${File.separator}tmp${File.separator}"))
      
      request.body.file("file").map { file => 
          // only get the last part of the filename
          // otherwise someone can send a path like ../../home/foo/bar.txt 
          // to write to other files on the system
          val filename    = Paths.get(file.filename).getFileName
          val fileSize    = file.fileSize
          val UploadSizeLimit = 3000000
          if (fileSize > UploadSizeLimit) {
            Future( BadRequest(Error("err0128.upload.file", "fileSize").encode) )
          } else {
            file.ref.copyTo(Paths.get(s"${baseDir}${File.separator}tmp${File.separator}${filename}"), replace = true)
            Future( BadRequest(Error("err0128.upload.file", "fileSize").encode) )
            //processUpload(ctx.orgDir, ctx.organizer, baseDir, contentDir, s"${baseDir}${File.separator}tmp${File.separator}${filename}", uptype, sdate, edate, toId )
          }
      }.getOrElse { 
          Future( BadRequest(shared.utils.Error("err0128.upload.file","file Command").encode) ) 
      }



    } 
  }  


  def processUpload(orgDir: String, organizer: String, baseDir: String, 
                    contentDir: String, filename: String, utype: Int, sDate: Int, eDate:Int, toId: Long)
                   (implicit msgs: Messages, tse : TournSVCEnv): Future[play.api.mvc.Result] = {
  
                 

    // matchExtension - check whether required upload type
    //                  matches with its extension
    def matchExtension(ext: String, uloadTyp: Int): Boolean = {
      uloadTyp match {
        case 1 => (ext == "xml") 
        case 2 => (ext == "md") 
        case 3 => (ext == "png") | (ext == "jpg") | (ext == "gif") 
        case 4 => (ext == "png")
        case 5 => (ext == "csv")
        case 6 => (ext == "png")
        case _ => false
      }
    }

    val fNameArr = filename.split("\\.")
    val fExt     = if (fNameArr.length > 1) fNameArr(fNameArr.length-1).toLowerCase else ""
    
    if (!matchExtension(fExt, utype)) {
      Future( BadRequest(Error("err0128.upload.file", "Dateityp wird nicht unterstützt").encode) )
    } else {
      utype match {
        case 1 => {
          val desFN = s"${baseDir}/${sDate}_participants.xml"
          Files.copy(Paths.get(filename), Paths.get(desFN), StandardCopyOption.REPLACE_EXISTING)
          tsv.addTournCTT(CttService.load(desFN), orgDir, organizer, sDate, eDate).map { 
            case Left(err)   => BadRequest(err.encode)
            case Right(trny) => Ok(trny.encode())
          }  
        }

        case 2 => { // invitation file
          Files.copy(Paths.get(filename), Paths.get(s"${baseDir}/${sDate}_invitation.md"), StandardCopyOption.REPLACE_EXISTING)
          Future(Ok(Return(true).encode))
        }
        case 3 => { // logo picture/file
          val desFN = s"${baseDir}/logo.${fExt}"
          Files.copy(Paths.get(filename), Paths.get(desFN), StandardCopyOption.REPLACE_EXISTING)
          Future(Ok(Return(true).encode))   
        }
        case 4 => { // certificate picture/file
          val desFN  = Paths.get(s"${baseDir}${File.separator}${sDate}_certificate.${fExt}")
          val contFN = Paths.get(s"${contentDir}${File.separator}certificate.${fExt}")

          Files.copy(Paths.get(filename), desFN, StandardCopyOption.REPLACE_EXISTING)
          Files.copy(Paths.get(filename), contFN, StandardCopyOption.REPLACE_EXISTING)
          Future(Ok(Return(true).encode))   
        }
        
        case 6 => { // club banner  picture/file
          val contFN = Paths.get(s"${contentDir}${File.separator}banner.${fExt}")
          Files.copy(Paths.get(filename), contFN, StandardCopyOption.REPLACE_EXISTING)
          Future(Ok(Return(true).encode))   
        }

        case _ =>  Future( BadRequest(Error("err0128.upload.file", "invalid extension type").encode) )
      }
    }
  }


}