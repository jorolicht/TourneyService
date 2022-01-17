package controllers

import javax.inject._
import scala.concurrent._

// imports for Silhouette
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{ LogoutEvent, Silhouette }

import play.api.{ Environment, Configuration, Logging }
import play.api.mvc._
import play.api.i18n.{ I18nSupport, Messages, Langs, Lang }
import play.api.libs.json.Json
import play.api.routing._
import play.api.libs.mailer._

import models.User
import models.daos.{ LicenseDAO, TourneyDAO, UserDAO }
import utils.auth.DefaultEnv
import shared.model._
import shared.utils.{ Error, Return }
import tourn.services._


/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class AdminCtrl @Inject()(

    licDao:     LicenseDAO,
    userDao:    UserDAO,
    coco:       ControllerComponents,  
    mailer:     MailerClient,
    langs:      Langs,
    silhouette: Silhouette[DefaultEnv]
)(
    implicit
    exco:         ExecutionContext,
    env:          Environment,
    cfg:          Configuration,
    tsv:          TourneyService,
    tourneyDao:   TourneyDAO,
    assetsFinder: AssetsFinder
) extends AbstractController(coco) with I18nSupport with Logging {
  
  val mailService               = new MailerService(mailer)
  val lang: Lang                = langs.availables.head

  // --------------------------------------------------------------------------
  //
  // Administration (doLogin, requestLicencse) 
  //
  // --------------------------------------------------------------------------

  /**  uploadFile calculate file name (for saveing) from additional
    *  parameter "uptype"
    */
  def uploadLic = silhouette.SecuredAction(parse.multipartFormData) { implicit request =>
    import scala.io.Source
    import java.io.File
    import java.nio.file.{Files, Path, StandardCopyOption}
    
    val u: User  = request.identity
    val email    = u.email.getOrElse("")
    
    val licFile = s"${env.rootPath}/license.csv"
    
    def delFile(fName: String) = {
      val f = new File(fName)
      if (f.exists()) f.delete()
    }  
    delFile(licFile)
    
    if (u.email.getOrElse("") != "robert.lichtenegger@googlemail.com") {
      BadRequest(Error("err0129.access.admin").encode)  
    } else {
      request.body.file("file").map { f =>
        import java.io.File
        //f.ref.moveTo(new File(licFile))
        f.ref.moveFileTo(new File(licFile))

        val lines = Source.fromFile(licFile, "UTF-8").getLines().toList
        
        for(line <- lines) {
          val entry = line.split(";")
          //licDao.createLicense(entry(1), entry(4), entry(3), entry(2), entry(6), entry(7), false)
          //createLicense(club: String, licStr: String, email: String, name: String, password: String, reqTStamp: String, updAllowed: Boolean)
        }
        
        //delFile(licFile)
        logger.info(s"uploadLic ok")
        Ok(Return(s"upload ${lines.length} entries success").encode)
      }
      .getOrElse {
        logger.info(s"uploadLic failed")
        BadRequest(Error("rr0130.ctrl.uploadLic").encode)  
      }      
    }
  }
  
  /**  export License file
    *  
    */
  def exportLic = silhouette.SecuredAction { implicit request =>
      import java.io.File
      import java.nio.file.{Files, Path, StandardCopyOption}
      
      val u: User  = request.identity
      val email    = u.email.getOrElse("")

      logger.info(s"export License")  

      Ok.sendFile(
        content = new java.io.File(s"${env.rootPath}/db/License.csv"),
        inline = false,
        //fileName = _ => Some("License.csv") //Change for 2.8
        fileName = _ => "License.csv" 
      )
  }    

  /**
   *   admin service interface
   */
  def exportDB(params: String) = silhouette.UserAwareAction.async { implicit request =>
    val msgs:  Messages   = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param  = Crypto.encParam(params)

    val toId = param("toId").toLong

    implicit val tse  = TournSVCEnv(toId, ctx.orgDir, true)

    logger.info(s"expDatabase(${param})")
    Future(Ok(""))
  }

}