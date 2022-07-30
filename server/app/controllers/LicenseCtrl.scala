package controllers

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption }
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
import shared.utils.Routines._
import shared.utils.{ Error, Return }
import tourn.services._


/** 
 * Controller for user license services
 */
@Singleton
class LicenseCtrl @Inject()
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
  assetsFinder: AssetsFinder
)    
  extends AbstractController(coco) with I18nSupport with Logging
{

  val mailService  = new MailerService(mailer)
  implicit val licsFormat  = Json.format[shared.model.Licenses]


  /** request request a license - return license string
   *
   */ 
  def request(params: String) = silhouette.UserAwareAction.async { implicit request: Request[AnyContent] =>
    import upickle.default._
    import java.util.UUID
    import org.mindrot.jbcrypt.BCrypt
    import tourn.services.DateTime

    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val data   = request.body.asText.getOrElse("")
    //val param  = Crypto.encParam(params)
 
    logger.info(s"request(${params}) data: ${data}") 
    try {
      val licReq = LicRequest.obify(data)
      val cpnReq = urify(licReq.club)

      licDao.findLicenseByOrgDir(cpnReq).flatMap { res => res match {
        case Some(lic) => Future(BadRequest(Error("err0100.licrequest.inuse").encode))
        case None      => {
          // generate new license
          val licStr = Crypto.genLicense(cpnReq, licReq.fullVersion)
          val reqDate = tourn.services.DateTime.getDateNow
          
          logger.info(s"request password transfer/hash: ${data}") 
          val hash = BCrypt.hashpw(licReq.password, BCrypt.gensalt(11))
          logger.info(s"request transfer/hash: ${licReq.password} // ${hash}") 

          val newLic = License.fromRequest(licReq, UUID.randomUUID().toString(), cpnReq, licStr, hash, reqDate)

          licDao.createLicense(newLic).flatMap ( lic => {
            if (lic.id > 0 && lic.licStr == licStr) {
              //generate default files and directories              
              val srcDir = Paths.get(cfg.get[String]("server.clubs.dir"), "dummy", "banner.png")
              val tarDir = Paths.get(cfg.get[String]("server.clubs.dir"), cpnReq, "banner.png")
              Files.createDirectories(tarDir.getParent())
              Files.copy(srcDir, tarDir, StandardCopyOption.REPLACE_EXISTING)  
              logger.info(s"request(${params}) successfull") 
              Future(Ok( write((licStr, lic.id, cpnReq)) ))
            } else {
              logger.error(s"request(${params}) error could not create license data: ${data}") 
              Future(BadRequest(Error("err0101.licrequest.database").encode))
            }
          }) 
        }
      }}  
    } catch { case _: Throwable => {
      logger.error(s"request(${params}) error exception: ${data}") 
      Future( BadRequest(Error("err0102.licrequest.format").encode) )
    }}
  }


  /**
   * delete a license
   */ 
  def delete(params: String) = silhouette.UserAwareAction.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param  = Crypto.encParam(params)
    val data   = request.body.asText.getOrElse("")
    
    logger.info(s"delete(${param}): data:${data}") 

    val liId = param("liId").toLong
    licDao.delete(liId).map { 
      cnt => Ok(Return(cnt).encode)
    }.recover { case e => 
      BadRequest(Error("err0103.license.delete", liId.toString).encode)
    }
  }  


  /** update a license
   * 
   */ 
  def update(params: String) = silhouette.UserAwareAction.async { implicit request: Request[AnyContent] =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param  = Crypto.encParam(params)
    val data   = request.body.asText.getOrElse("")
    
    logger.info(s"updaste(${param}): data:${data}") 
    val lic = License.obify(data)
    License.decode(data) match {
      case Left(err)  => Future( BadRequest(err.encode) )
      case Right(lic) => licDao.update(lic).map {
        cnt => Ok( Return(cnt).encode ) 
      }.recover { case e => 
        BadRequest(Error("err0104.license.update", lic.toString).encode) 
      }
    }
  }  


  /** getAll - returns all licenses
   * 
   */ 
  def getAll(params: String) = silhouette.UserAwareAction.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param  = Crypto.encParam(params)
    
    if (!ctx.admin) {
      Future( BadRequest(Error("err0105.notallowed").encode) )
    } else {
      licDao.list().map { licList => {
        val lics = for { lic <- licList } yield { lic.stringify() }
        Ok( Json.toJson(new Licenses(lics) ) )
      }}
    }
  }


  /** getOwn - return license of current user (mask password)
   * 
   */ 
  def getOwn(params: String="") = silhouette.UserAwareAction.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    
    licDao.findLicenseByOrgDir(ctx.orgDir).map { lic => lic match {
      case Some(license) => Ok(license.copy(password="XXX").encode())
      case None          => BadRequest(Error("err0105.notallowed").encode)
    }}
  }


  /** has - check whether club has already a license
   * 
   * @param params: encoded URL parameter club, version
   */ 
  def has(params: String) = silhouette.UserAwareAction.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    //val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param  = Crypto.encParam(params)
    
    val club    = param("club")
    val orgDir  = urify(club)
    logger.info(s"has(${club}) ${orgDir}")
    
    licDao.findLicenseByOrgDir(orgDir).map(lic => lic match {
      case Some(license) => Ok(Return(true).encode)
      case None          => Ok(Return(false).encode)
    })
  }

  /** available - check whether club has already a license
   * 
   * @param params: encoded URL parameter club, version
   */ 
  def available(params: String) = silhouette.UserAwareAction.async { implicit request =>
    val msgs:  Messages  = messagesApi.preferred(request)
    //val ctx    = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param  = Crypto.encParam(params)
    
    val club    = param("club")
    val orgDir  = urify(club)
    
    licDao.findLicenseByOrgDir(orgDir).map(lic => lic match {
      case Some(license) => {
        logger.info(s"available: ${orgDir} already inuse")
        Ok(Return(false).encode)
      }  
      case None          => Ok(Return(true).encode)
    })
  }


  /** sendVerifyCode - sends user the verification code
   * 
   * @param params: encoded URL parameter code, version
   */ 
  def sendVerifyCode(params: String) = silhouette.UserAwareAction.async { implicit request =>
    import views.html.component.EmailCode

    val msgs:  Messages  = messagesApi.preferred(request)
    val param  = Crypto.encParam(params)
    
    val lang = msgs("app.lang")
    val code = param("code")
    val name = param("name").split(",").map(_.trim).reverse.mkString(" ") 
    val email = param("email")
    logger.info(s"sendVerifyCode(${code}) to: ${name} with: ${email}")

    try {
      val code2send = "%04d".format((code.toInt * 19) % 10000)
      val mail = Email(
        msgs("email.code.subject"),
        msgs("email.code.from"),
        Seq(s"$name <$email>"),
        bodyText = Some(EmailCode(name, code2send, lang, false).toString),
        bodyHtml = Some(EmailCode(name, code2send, lang, true).toString)
      )
      mailer.send(mail)
      Future( Ok(Return(true).encode) )
    } catch { case e: Exception => Future( BadRequest(Error("err0106.ctrl.sendVerifyCode", email).encode) )  }
  }


  /** sendLicense - sends club(email) the license code
   * 
   * @param params: encoded URL parameter code, version
   */ 
  def sendLicense(params: String) = silhouette.UserAwareAction.async { implicit request =>
    import views.html.component.EmailLicense
    import org.apache.commons.mail.EmailException

    val msgs:  Messages  = messagesApi.preferred(request)    
    val lang = msgs("app.lang")

    // verify necessary parameters
    val (orgId, invoice, errMsg, valid) = (for {
      pMap  <- Crypto.encEParam(params)
      orgId <- Crypto.getEParam(pMap, "orgId", 0L)
      invoice <- Crypto.getEParam(pMap, "invoice") 
    } yield { (orgId, invoice) }) match {
      case Left(err)  => (0L, "", err, false)
      case Right(res) => (res._1, res._2,"", true)  
    }

    if (!valid) {
      logger.error(s"sendLicense -> decoding error: ${errMsg} with ${params}")
      Future( BadRequest(Error("err0107.sendEmail.parameter").encode) )
    } else {
      licDao.findLicenseById(orgId).map(_ match {
        case Some(license) => {
          try {
            val invoicePath = cfg.get[String]("server.clubs.dir") + File.separator + license.orgDir + File.separator + invoice
            val name = license.name.split(",").map(_.trim).reverse.mkString(" ")
            val mail = Email(
              msgs("email.license.subject"),
              msgs("email.license.from"),
              Seq(s"${name} <${license.email}>"),
              attachments = if (invoice!="") Seq(AttachmentFile(msgs("invoice.email.name"), new File(invoicePath))) else Seq(),          
              bodyText = Some(EmailLicense(name, license.licStr, license.fullVersion, false).toString),
              bodyHtml = Some(EmailLicense(name, license.licStr, license.fullVersion, true).toString)
            )
            mailer.send(mail)
            Ok(Return(true).encode)
          } catch { 
            case e: EmailException => BadRequest(Error("err0108.sendEmail.exception", e.getMessage()).encode ) 
            case _: Throwable      => BadRequest(Error("err0109.sendEmail.service").encode) 
          }
        }  
        case None          => BadRequest(Error("eerr0110.sendEmail.clubNotfound").encode)
      })
    }
  }



  /** invoice - generates invoice (pdf file) puts it to the club directory
   *  
   * @param params url encode parameter for contact, address, orgId, orgDir 
   * 
   */
  def invoice(params: String) = Action { implicit request: Request[AnyContent] =>
    import sys.process._
    import java.time.LocalDate
    import java.time.format.DateTimeFormatter
    import java.io.File
    import java.nio.file.{Paths, Files, StandardCopyOption}
    import java.nio.charset.StandardCharsets

    import shared.model.Contact
    import shared.model.Address
    import shared.model.Invoice
    import shared.model.InvoiceItem
    val msgs: Messages  = messagesApi.preferred(request)

    // verify necessary parameters
    //val pMap  = Crypto.encParam(params)
    val (orgDir, orgId, address, contact, errMsg, valid) = (for {
      pMap <- Crypto.encEParam(params)
      dir  <- Crypto.getEParam(pMap, "orgDir")    
      id   <- Crypto.getEParam(pMap, "orgId", 0L) 
      addr <- Address.decode(Crypto.getParam(pMap, "address")) 
      cont <- Contact.decode(Crypto.getParam(pMap, "contact"))
    } yield { (dir, id, addr, cont) }) match {
      case Left(err)  => ("", 0L, Address("","","","",""), Contact("","","",""), err, false)
      case Right(res) => (res._1, res._2, res._3, res._4,"", true)  
    }

    if (!valid) {
      logger.error(s"invoice -> decoding error: ${errMsg} with ${params}")
      BadRequest(Error("err0112.decode.req.invoice").encode)
    } else {
      /* Setup Invoice
      ** calculate invoice number and save it to invoice directory
      */
      val today = LocalDate.now()
      val year = today.getYear

      val invoiceDate = today.format(DateTimeFormatter.ofPattern("yyyyMMdd"))
      val path2Info = Paths.get(cfg.get[String]("server.invoice.dir"), year.toString, "Info.txt")
      val invoiceNumber = if (!Files.exists(path2Info))  {
        Files.createDirectories(path2Info.getParent())
        year*10000 + (year%100) + 1
      } else {
        val content = {
          try    Files.readString(path2Info)
          catch  { case _: Throwable => "?" }        
        }
        content.toIntOption.getOrElse(year*10000 + (year%100)) + 1
      }

      Files.write(path2Info, invoiceNumber.toString.getBytes(StandardCharsets.UTF_8))
      val invoicePdfName  = s"${invoiceNumber}_Invoice_" + "%04d".format(orgId) + ".pdf"
      val invoiceHtmlName = s"${invoiceNumber}_Invoice_" + "%04d".format(orgId) + ".html"

      val invoicePdfPath  = Paths.get(cfg.get[String]("server.invoice.dir"), year.toString, invoicePdfName)
      val invoiceHtmlPath  = Paths.get(cfg.get[String]("server.invoice.dir"), year.toString, invoiceHtmlName)

      val inDate   = invoiceDate.toIntOption.getOrElse(19700101)
      val total    = msgs("invoice.item1.price").toDoubleOption.getOrElse(0.0)
      val vat      = msgs("invoice.vat").toDoubleOption.getOrElse(0.0)
      val currency = msgs("invoice.currency")
      val inPos1   = InvoiceItem(msgs("invoice.item1.description"), total, msgs("invoice.currency"))
      val invoice  = new Invoice(contact, address, invoiceNumber.toString, inDate, currency, total, vat, Seq(inPos1) )

      Files.write(invoiceHtmlPath, views.html.component.InvoicePage(invoice).toString.getBytes(StandardCharsets.UTF_8))
      
      val genPdfCmd = cfg.get[String]("server.genpdf.cmd")
      val genPdf = s"${genPdfCmd} ${invoiceHtmlPath} ${invoicePdfPath}"
      val genResult = genPdf.!

      if (genResult != 0) {
        logger.error(s"invoice: generation pdf failed: ${genPdf}")
        BadRequest(Error("err0111.generate.PdfInvoice").encode)
      } else {
        val invoiceClubPath = Paths.get(cfg.get[String]("server.clubs.dir"), orgDir, invoicePdfName)
        if (!Files.exists(invoiceClubPath.getParent())) Files.createDirectories(invoiceClubPath.getParent())
        Files.copy(invoicePdfPath, invoiceClubPath, StandardCopyOption.REPLACE_EXISTING) 
        Ok(Return(invoicePdfName).encode)
      }
    }
  }

  /** getEmail
    * 
    * @param params
    */
  def getEmail(params: String="") = silhouette.UserAwareAction.async { implicit request =>
    import shared.utils.Routines._

    logger.info(s"getEmail(params=${params})")

    val msgs: Messages  = messagesApi.preferred(request)
    val param   = Crypto.encParam(params)
    val licCode = param("licCode")

    // search license with licCode
    val lic = licDao.findLicenseByLicCode(licCode)

    lic.map { _ match {
      case Some(license) => Ok(Return(license.email).encode)
      case None          => BadRequest(Error("err0190.ctrl.lic.notfound ", licCode).encode) 
    }}
  }


}