package controllers

import controllers.ActorRefManager._
import javax.inject._

// imports for Silhouette
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{ LogoutEvent, Silhouette }
import utils.auth.DefaultEnv

// imports for akka
import akka.actor._
import akka.dispatch._
import akka.stream._
import akka.stream.scaladsl._


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
import play.api.libs.json.Json
import play.api.cache._
import play.api.mvc._
import play.api.i18n._

import models.daos.{ TourneyDAO, LicenseDAO }
import models.User
import shared.utils._
import shared.model._
import tourn.services.{ DateTime, Crypto, TourneyService, CttService, CttTournament }
import tests._


/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class TestCtrl @Inject()(
  testCache:         SyncCacheApi, 
  licDao:            LicenseDAO,  
  cc:                ControllerComponents,
  lc:                ApplicationLifecycle,
  langs:             Langs,
  system:            ActorSystem,
  silhouette:        Silhouette[DefaultEnv]
)(implicit
  ec:                ExecutionContext,
  env:               Environment,
  cfg:               Configuration,
  tonyDao:           TourneyDAO,
  tsv:               TourneyService,
  assetsFinder:      AssetsFinder
) extends AbstractController(cc)
  with I18nSupport with Logging {

  import shared.utils._


  def exec(params: String) = silhouette.UserAwareAction.async { implicit request: Request[AnyContent] =>
    import tourn.services.Crypto._
    import upickle.default._
    import cats.data.EitherT

    def accessError(cmd: String) = Error("err0080.access.invalidRights", "", "", cmd).encode
    def chkAccess(ctx: shared.utils.Session): Boolean = (ctx.orgId > 0) | !Crypto.accessCtrl

    val msgs: Messages = messagesApi.preferred(request)
    val ctx     = getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param   = encParam(params)
    val reqData = request.body.asText.getOrElse("")

    Future(Ok(Return(true).encode))
  }  




  /** trigger - triggers connected clients
   * 
   */
  def trigger(id: String="", toId: Long=0L,  msg: String="Hallo") = Action.async { implicit request =>
    import controllers.{ EventActor, ActorRefManager }
    val msgs:  Messages  = messagesApi.preferred(request)
    logger.debug(s"trigger -> id: ${id} toId: ${toId} msg: ${msg}")
    if (toId!=0) {
      EventActor.manager ! ActorRefManager.MessageToId(toId, msg) 
    } else {
      if (id!="") {
        EventActor.manager ! ActorRefManager.SendMessage(id, msg) 
      } else {
        EventActor.manager ! ActorRefManager.Clean() 
      }
           
    }
    Future(Ok("true"))
  }
    

  def test_cache(name: String, p1: String="", p2: String="", p3: String="") = {
    logger.info(s"Test ${name}: started") 
    p1 match {
      case "set" => testCache.set(p2,p3); Future(Ok(s"Test ${name} ${p1}: succeeded"))
      case "get" => {
        val res = testCache.get[String](p2).getOrElse("wrong")
        logger.info(s"Test ${name}: ${res}") 
        Future(Ok(res))
      }
      case _     => Future(Ok(s"Test ${name} ${p1}: failed"))
    }
  }


  def cmd(toId: Long, coId: Long = 0) = Action.async { implicit request: Request[AnyContent]=>
    val msgs: Messages  = messagesApi.preferred(request)
    val cont  = request.body.asText.getOrElse("default")   
    logger.info(s"TestCtrl.cmd(${toId}/${coId}): ${cont}") 
    Future(Ok(s"ok -> toId:${toId} coId:${coId}  cont:${cont} "))
  }


  def sendError() = Action { implicit request: Request[AnyContent]=>
    //import play.api.mvc.Results._
    val msgs: Messages  = messagesApi.preferred(request)
    val cont  = request.body.asText.getOrElse("default")   
    logger.info(s"TestCtrl.sendError ${cont}") 
    BadRequest(s"ok ->  cont:${cont} ")
  }


  def test_trigger(orgdir: String, toid: Long) = {
    EventActor.manager ! SendMessage(orgdir, s"Hallo#UPS#${toid}")
    Future(Ok(s"TEST trigger"))
  } 


  def test_sendMsg(toid: Long) = {
    EventActor.manager ! SendMessage(s"PF_${toid}", s"Hallo#UPS#${toid}")
    Future(Ok(s"TEST sendMsg"))
  } 


  /**
   *  test getLicenseFromAuth
   */
  def getLicenseFromAuth = Action { implicit request =>
    val licCode = tourn.services.Crypto.getLicenseCodeFromBasicAuth(request.headers.get("Authorization"))
    Ok(s"License Code: ${licCode}")
  }  

  /**
   *  test sha1 encoding
   */
  def sha1 = Action { implicit request =>
    import java.nio.charset.StandardCharsets
    val value1 = "abcd"
    Ok(Crypto.toSHA1(value1.getBytes(StandardCharsets.UTF_8)))
  }  


  /**
   *  test base64 encoding
   */  
  def base64 = Action { implicit request =>
    import java.util.Base64
    import java.nio.charset.StandardCharsets

    val value = "abcdÄÖl;,"
    //val value1 = Crypto.toSHA1(value.getBytes(StandardCharsets.UTF_8))

    // logger.info(s"SHA1:   ${value1}") 
    val result = new String(Base64.getEncoder.encode(value.getBytes(StandardCharsets.UTF_8)))
    // logger.info(s"Base64: ${result}") 
    val bts = result.getBytes(StandardCharsets.UTF_8)
    bts.map { x =>
      val uByte = if (x < 0) {
        x & 0xff
      } else {
        x.toInt
      }
      logger.info(s"${uByte}")  
    }  
    logger.info(s"TestCtrl.base64: ${result}") 
    Ok(result)
  }

  
  /**
   *  test authentication
   */
  def authentication = silhouette.UserAwareAction { implicit request =>
    val ctx = tourn.services.Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))
    logger.info(s"TestCtrl.authentication (${ctx})")
    Ok(s"${ctx.orgId}<#>ClubId: ${ctx.orgId}")
  }   

  /**
   *  test dump
   */
  def dump(toId: Long) = Action.async { implicit request =>
    logger.info(s"TestCtrl.dump -> toId: ${toId}")
    tsv.dump(toId).map {
      case Left(err)     => Ok(err.toString)
      case Right(result) => Ok(result)
    }
  }

  /**
   *  test authentication
   */
  def getLanguage = silhouette.UserAwareAction { implicit request =>
    val lang = Messages("appmain.lang")    
    
    val ctx = tourn.services.Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    logger.info(s"TestCtrl.getLanguage (${ctx})")
    Ok(s"${ctx.orgId}<#>ClubId: ${ctx.orgId} Language: $lang")
  }
  

  def waitAll[T](futures: Seq[Future[T]]) = {
    import scala.util.{ Failure, Success, Try }
    def lift[T](futures: Seq[Future[T]]) = futures.map(_.map { Success(_) }.recover { case t => Failure(t) })
    // having neutralized exception completions through the lifting, .sequence can now be used
    Future.sequence(lift(futures)) 
  }


  /**
   *  invoice
   * 
   */  

    /*
      var recipient:   Contact,
      var address:     Address,
      var number:      String,
      var date:        Int,         // format yyyymmdd
      val currency:    String,
      var total:       Double,
      var vat:         Double,
      var items:       Seq[InvoiceItem]
    */

  def invoice = Action { implicit request => 
    import sys.process._
    import java.time.LocalDate
    import java.io.File
    import java.nio.file.{Paths, Files, StandardCopyOption}
    import java.nio.charset.StandardCharsets

    import shared.model.Contact
    import shared.model.Address
    import shared.model.Invoice
    import shared.model.InvoiceItem
    
    val msgs: Messages  = messagesApi.preferred(request)
    val licenseId       = 47
    val orgDir          = "demooooo"

    val customer  = Contact("Mustermann","Max","0151-41222","robert.lichtenegger@icloud.com")
    val address   = Address("description", "German", "85356","Freising","Finkenstr. 36a")
    val inPos1    = InvoiceItem("Turnierprogramm Lizenz(full)", 39.50, "EUR")

    /*
    ** calculate invoice number and save it to invoice directory
    */
    val year = LocalDate.now.getYear
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
    val invoicePdfName  = s"${invoiceNumber}_Invoice_" + "%04d".format(licenseId) + ".pdf"
    val invoiceHtmlName = s"${invoiceNumber}_Invoice_" + "%04d".format(licenseId) + ".html"

    val invoicePdfPath  = Paths.get(cfg.get[String]("server.invoice.dir"), year.toString, invoicePdfName)
    val invoiceHtmlPath  = Paths.get(cfg.get[String]("server.invoice.dir"), year.toString, invoiceHtmlName)

    val invoice = new Invoice(customer, address, "20210031", 20201231, "EUR", 39.50, 19.00, Seq(inPos1) )
    Files.write(invoiceHtmlPath, views.html.component.InvoicePage(invoice).toString.getBytes(StandardCharsets.UTF_8))
    
    val genPdfCmd = cfg.get[String]("server.genpdf.cmd")
    val genPdf = s"${genPdfCmd} ${invoiceHtmlPath} ${invoicePdfPath}"
    val genResult = genPdf.!

    if (genResult != 0) {
      logger.error(s"invoice: generation pdf failed: ${genPdf}")
      BadRequest("error.generate.PdfInvoice")
    } else {
      val invoiceClubPath = Paths.get(cfg.get[String]("server.clubs.dir"), orgDir, invoicePdfName)
      if (!Files.exists(invoiceClubPath.getParent())) Files.createDirectories(invoiceClubPath.getParent())
      Files.copy(invoicePdfPath, invoiceClubPath, StandardCopyOption.REPLACE_EXISTING) 
      Ok(invoicePdfName)
    }
  }



}    