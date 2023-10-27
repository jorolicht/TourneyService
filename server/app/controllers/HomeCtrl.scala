package controllers

import javax.inject._

// imports for akka
import akka.actor._
import akka.dispatch._
import akka.stream._
import akka.stream.scaladsl._
import controllers.ActorRefManager._

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.impl.providers._
import models.services.UserService
import utils.auth.DefaultEnv

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent._
import scala.util._
import sys.process._

import play.api.{ Environment, Configuration, Logging }
import play.api.i18n.{ I18nSupport, Messages, Langs, Lang }
import play.api.inject.ApplicationLifecycle
import play.api.http.ContentTypes
import play.api.libs.EventSource
import play.api.libs.json.{JsValue, Json }

import play.api.mvc._
import play.Application
import play.twirl.api.HtmlFormat
import play.twirl.api.Html

import play.api.mvc.MultipartFormData.FilePart
import play.core.parsers.Multipart.FileInfo

import models.daos.{ TourneyDAO, LicenseDAO, UserDAO }
import models.{ User, LocalUser }
import shared.model.{ License, LicRequest, Sidebar, SidebarEntry, SidebarConfig}
import shared.utils._
import tourn.services.{ Crypto, MailerService }


/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class HomeCtrl @Inject()(
    env:                    Environment,
    cfg:                    Configuration,
    tourneyDao:             TourneyDAO,
    licDao:                 LicenseDAO,
    usrDao:                 UserDAO,
    cc:                     ControllerComponents,
    silhouette:             Silhouette[DefaultEnv],
    userService:            UserService,
    authInfoRepository:     AuthInfoRepository,
    socialProviderRegistry: SocialProviderRegistry,
    langs:                  Langs
)(
    implicit
    ec:                ExecutionContext,
    assetsFinder:      AssetsFinder
) extends AbstractController(cc) 
  with I18nSupport with Logging {
   
  val lS = for (lng <- langs.availables) yield (lng.toLocale.getLanguage, lng.toLocale.getDisplayLanguage(lng.toLocale).capitalize)


  /** start - main entry point for the application
   *
   * @param ucName name of usecase to execute
   * @param ucParam parameter for the uscase
   * @param ucInfo additional info for usecase
   *  
   */

  def start(ucName: String="", ucParam: String="", ucInfo: String="") = Action { implicit request =>
    import tourn.services.Crypto._
    logger.info(s"start(${ucName},${ucParam},${ucInfo})")
    val ctx = getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))

    if (ctx.orgId <= 0) {
      Ok(views.html.start(ucName, ucParam, ucInfo, genSessionBase64(), lS, rml)).discardingCookies(DiscardingCookie("TuSe"))
    } else {
      Ok(views.html.start(ucName, ucParam, ucInfo, genSessionBase64(ctx), lS, rml))
    }
  }
   
  def register() =  Action { implicit request =>
    import tourn.services.Crypto._
    val ctx = getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))

    val ucName  = "HomeRegister"
    val ucParam = "freeLicense"
    val ucInfo  = "INIT"

    if (ctx.orgId <= 0) {
      Ok(views.html.start(ucName, ucParam, ucInfo, genSessionBase64(), lS, rml)).discardingCookies(DiscardingCookie("TuSe"))
    } else {
      Ok(views.html.start(ucName, ucParam, ucInfo, genSessionBase64(ctx), lS, rml))
    }
  }


  def playfield(orgdir: String, date: Int) = silhouette.UserAwareAction.async { implicit request =>
    tourneyDao.findByPathDate(orgdir,date).map { tony => tony match {
      case Some(tr) => Ok(views.html.playfields(tr.id.toString))
      case None     => Redirect(routes.HomeCtrl.start("HomeMain", "ErrorCode", "err0089.ctrl.noMatchingTourney"))
    }}
  }


  def getIpAddress() = Action { implicit request =>
    //import scala.jdk.CollectionConverters
    import collection.JavaConverters._
    import java.net._

    val enumeration = NetworkInterface.getNetworkInterfaces.asScala.toSeq

    val ipAddresses = enumeration.flatMap(p =>
      p.getInetAddresses.asScala.toSeq
    )

    val address = ipAddresses.find { address =>
      val host = address.getHostAddress
      host.contains(".") && !address.isLoopbackAddress
    }.getOrElse(InetAddress.getLocalHost)

    Ok(address.getHostAddress)  
  }

  /** qrcode generation
   *
   * generates qrcode together with tourney data
   * which can be used as notice/poster for tourney master
   */
  def qrcode(orgdir: String, date: Int, url: String) = silhouette.UserAwareAction.async { implicit request =>
    import tourn.services.Crypto._
    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets    
    import shared.utils.Routines._
    val lang = Messages("app.lang")

    tourneyDao.findByPathDate(orgdir, date).map { tony => tony match {
      case Some(tr) => {
        val logoPath = Paths.get(cfg.get[String]("server.clubs.dir"), orgdir, "logo.png") 
        val link = s"${url}/club/${orgdir}/${date}/home"

        val dateDesc: String = if (tr.startDate == tr.endDate) {
          int2date(tr.startDate, lang)
        } else {
          Messages("date.range", int2date(tr.startDate, lang), int2date(tr.endDate, lang))
        }
        Ok(views.html.qrcode(tr.name, dateDesc, tr.organizer, link, orgdir, lang, Files.exists(logoPath) ))
      }
      case None     =>  {  
        val ctx = tourn.services.Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))
        Ok(views.html.start("HomeMain", "ErrorCode", "err0089.ctrl.noMatchingTourney", genSessionBase64(ctx), lS, rml))
      }
    }}  
  }

  // test qrcode generation
  def qrco(url: String) = silhouette.UserAwareAction.async { implicit request =>
    val mode = Messages("app.mode")
    Future( Ok(views.html.qrcode("Weihnachtsturnier 2020", "6. - 12. Dezember 2020", "TTC Freising",  s"${url}/org", "demo", "de", false)))
  }


  def tourney(orgdir: String, date: Int) = silhouette.UserAwareAction.async { implicit request =>
    import tourn.services.Crypto._
    val ctx = tourn.services.Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))
    tourneyDao.findByPathDate(orgdir,date).map { tony => tony match {
      case Some(tr) => Ok(views.html.start(s"InfoSchedule", s"${tr.id}", "", genSessionBase64(ctx), lS, rml))
      case None     => Redirect(routes.HomeCtrl.start("HomeMain", "ErrorCode", "err0089.ctrl.noMatchingTourney"))
    }}
  }  


  def reloadTourney(orgdir: String, date: Int, ucName: String = "HomeMain") = Action { implicit request =>
    val ctx = tourn.services.Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request))    
    
    logger.info(s"reloadTourney(${orgdir},${date},${ucName})")
    Ok(views.html.start(ucName, "", "", tourn.services.Crypto.genSessionBase64(ctx), lS, tourn.services.Crypto.rml))  
  }


  // --------------------------------------------------------------------------
  //
  //  Services(API) - return JSON
  //
  // --------------------------------------------------------------------------
  /**
    * redirect to the last refered URL with new language
    */
  def setLang(lang: String) = Action { implicit request =>
    val referedURL = request.headers.get("referer").getOrElse("/")
    Redirect(referedURL).withLang(Lang(lang))
  }

  /**
    *  generate messages in Json format
    */
  def getMessages = Action { implicit request =>
    val jssMsgs = new JssMessages(messagesApi.messages)
    Ok(jssMsgs.allMessagesJson)
  }
  

  def getErrCodes(lang: String) = Action  { implicit request =>
    val jssMsgs = new JssMessages(messagesApi.messages)
    val errMsgs = jssMsgs.allMessages(lang)

    //logger.info(s"getErrMsg: ${errMsgs.filter(x => x._1.startsWith("err"))}")
    Ok(Json.toJson(errMsgs.filter(x => x._1.startsWith("err")) )) 
  }


  /** content send a file
    */
  def content(p1: String, p2: String, p3: String, p4: String, p5: String, p6: String) = Action { implicit request =>
    //val contentPath = cfg.get[String]("content.directory.path")
    //val home = s"${env.rootPath}"
        
    val fName: String = genPath(env.rootPath.toString, "public", "content", p1, p2, p3, p4, p5, p6)
    val file = new java.io.File(fName)
    if (file.exists) {
      logger.info(s"content: send file ($fName)")
      Ok.sendFile(file)
    } else {
      val relfName = genPath("public", "content", p1, p2, p3, p4, p5, p6)
      logger.info(s"content: file not found ($fName)")
       BadRequest(Error("err0137.user.fileNotFound",relfName).encode)
      //Ok("").withCookies(Cookie("TuSeError", tourn.services.Crypto.genErrorCookie(Messages("error.message.fileNotFound",relfName)))).bakeCookies()
    }        
  }
  
  
  /** exist - check whether file exists
    */
  def exist(p1: String, p2: String, p3: String, p4: String, p5: String, p6: String) = Action { implicit request =>
    val fName: String = genPath(env.rootPath.toString, "public", "content", p1, p2, p3, p4, p5, p6)
    val file = new java.io.File(fName)
    Ok(Return(file.exists).encode)
  } 

  
  /** uploadFile get file name (for saveing) from additional
    *  parameter "fname"
    */
  def upload = Action(parse.multipartFormData) { implicit request =>
    val uploadPath = cfg.get[String]("server.upload.dir")

    val param    = request.body.asFormUrlEncoded
    val tarFName = param("fname").mkString

    logger.info(s"upload: $tarFName")

    request.body.file("file").map { f =>
      import java.io.File
      val filename    = f.filename
      val contentType = f.contentType
      //f.ref.moveTo(new File(s"$uploadPath$tarFName"))
      f.ref.moveFileTo(new File(s"$uploadPath$tarFName"))
      logger.info(s"filename = ${filename}, contentType = ${contentType}")
      Ok(Return(true).encode)
    }
    .getOrElse { Ok(Return(false).encode)}
  }
 
  
  // --------------------------------------------------------------------------
  //
  //  Utility routines
  //
  // --------------------------------------------------------------------------  

  // genPath
  def genPath(name: String*): String = {
    import java.io.File
    name.filter(_!="").mkString(File.separator).replace("${File.separator}${File.separator}","${File.separator}") 
  }
  

  /**
    *  trigger - sends a message through server send event mechanism to the client
    */  
  def trigger(id: String, toId:Long=0L, init:Boolean=false) = Action { implicit request =>
    if (init) {
      logger.info(s"initTrigger id:${id}")
      val source  = Source.actorRef[String](32, OverflowStrategy.dropHead).watchTermination() { 
        case (actorRef, terminate) =>  EventActor.manager ! Register(id, actorRef)
                                      terminate.onComplete(_ => EventActor.manager ! UnRegister(id, actorRef))
                                      actorRef
      }
      Ok.chunked(source via EventSource.flow).as(ContentTypes.EVENT_STREAM)
    } else {
      logger.info(s"setTrigger id:${id} toId:${toId}")
      EventActor.manager ! RegToId(id, toId)
      Ok(Return(true).encode)
    }  
  }  

  /**
   *  console
   */
  def console = silhouette.UserAwareAction { implicit request =>
    Redirect(routes.HomeCtrl.start("HomeConsole", "", ""))
  }


  /**
   *  getCtx of call
   */
  def getCtx = silhouette.UserAwareAction { implicit request =>
    val lang = Messages("app.lang")

    val ctx = tourn.services.Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    
    logger.info(s"HomeCtr.getCtx: lang:${lang } ctx:${ctx.toString}")
    Ok(s"""{ "lang" = ${lang}, "userId": "${ctx.userId}", "orgId": ${ctx.orgId}, "orgDir": "${ctx.orgDir}" } """)
  }

  def ping = silhouette.UserAwareAction { implicit request => Ok(true.toString) }

  def version = silhouette.UserAwareAction { implicit request => Ok(Messages("app.version")) }
}


/**
 * @param allMessagesData All the messages of the application, as a map of (lang -> map(key -> message pattern)). As it
 *                        is the case in Play, JsMessages assumes that “default” messages are indexed by the `"default"`
 *                        and `"default.play"` language codes.
 */
class JssMessages(allMessagesData: Map[String, Map[String, String]]) {

  // Message patterns have to escape quotes using double quotes, here we unescape them because we don’t support using quotes to escape format elements
  // TODO Also remove subformats
  private val allMessagesUnescaped: Map[String, Map[String, String]] =
    // allMessagesData.mapValues(_.mapValues(_.replace("''", "'")))
    allMessagesData.map{ case (k1: String , v1: Map[String,String]) => {
      (k1,v1.map {
        case (k2: String, v2: String) => (k2, v2.replace("''", "'")) 
      }) 
    }}
    
    // case (k,v) => ((k,v))) 

    //
    //  (k, v.map((k2,v2) => (k2, v2.replace("''", "'"))))
    //)

  /**
   * Messages for each available lang of the application.
   *
   * The message corresponding to a given key is found by searching in the
   * following locations, in order: the language (e.g. in the `conf/messages.fr-FR` file), the language
   * country (e.g. `conf/messages.fr`), the application default messages (`conf/messages`) and the
   * Play default messages.
   */
  lazy val allMessages: Map[String, Map[String, String]] = for ((lang, msgs) <- allMessagesUnescaped) yield {
    lang match {
      // Do not merge with "default" if its "default.play"
      case "default.play" => lang -> allMessagesUnescaped.getOrElse("default.play", Map.empty)
      case _ => lang -> (
        allMessagesUnescaped.getOrElse("default.play", Map.empty) ++
        allMessagesUnescaped.getOrElse("default", Map.empty) ++
        extractCountry(lang).flatMap(country => allMessagesUnescaped.get(country)).getOrElse(Map.empty) ++
        msgs
      )
    }
  }

  /**
   * Same as `allMessages`, but as a JSON value.
   */
  final val allMessagesJson: JsValue = Json.toJson(allMessages)
  private def extractCountry(lang: String): Option[String] = if (lang.contains("-")) Some(lang.split("-")(0)) else None
}

/**
 * Actor Manager for triggering clients waiting on events on an event/tournament
 */
class ActorRefManager extends Actor {
  import scala.collection.mutable.Set
  import scala.collection.mutable.HashMap
  
  private val aRefMap = new HashMap[String, ActorRef]
  private val toIdMap = new HashMap[Long, Set[String]]

  def receive = {
    case Register(id, actorRef)   => aRefMap(id) = actorRef
    case UnRegister(id, actorRef) => {
      aRefMap.remove(id)
      //println(s"unregister id: ${id} map: ${toIdMap}")
    }  
    case SendMessage(id, message) => if (aRefMap.contains(id)) aRefMap(id) ! message

    case MessageToId(toId, msg)   => toIdMap(toId).foreach( aRefMap(_) ! msg  )
    case RegToId(id, toId)        => {
      for ((k,s) <- toIdMap) { toIdMap(k) -= id }
      if (toId != 0) {
        if (!toIdMap.isDefinedAt(toId)) toIdMap(toId) = Set.empty[String]
        toIdMap(toId) += id
      }  
      //println(s"register: toId:${toId} id: ${id} map: ${toIdMap}")
    } 
    case Clean()                    => {
      val curTime = System.currentTimeMillis().toLong
      for ((k,s) <- toIdMap) { toIdMap(k).foreach { elem => {
        val diff: Long = (curTime - elem.substring(2).toLong)/1000
        if (diff > 86400) toIdMap(k) -= elem
        //println(s"clean: tStamp: ${tStamp} difference: ${(curTime-tStamp)/1000} sec")
      }}}
      for ((k,s) <- toIdMap) { if (s.isEmpty) toIdMap.remove(k) } 
      println(s"clean trigger: curTime: ${curTime} map: ${toIdMap}")
    }  
  }
}

object ActorRefManager {
  def props: Props = Props[ActorRefManager]()
  case class SendMessage(id: String, message: String)
  case class Register(id: String, actorRef: ActorRef)
  case class UnRegister(id: String, actorRef: ActorRef)

  case class MessageToId(toId: Long, message: String)
  case class RegToId(id: String, toId: Long)
  case class Clean()
}

object EventActor {
  val system  = ActorSystem("SystemInit")  
  var manager = system.actorOf(ActorRefManager.props, name = "EventManager")
}