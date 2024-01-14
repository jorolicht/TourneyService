package scalajs

import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer
import scala.util.{Success, Failure}

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.logging._
import upickle.default._

// tourney service imports
import shared.model._
import shared.utils._

import scalajs.usecase.dialog.DlgBox
import scalajs.usecase.component._
import scalajs.usecase.info._
import scalajs.usecase.organize._
import scalajs.usecase.info._
import scalajs.usecase.admin._
import scalajs.usecase.home._
import scalajs.usecase._
import scalajs.service._


object AppEnv extends BasicHtml
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp=UseCaseParam("APP", "app", "App", "app", getMessage _ ) 
  
  var logger:Logger = org.scalajs.logging.NullLogger
  var messages      = Map[String,Map[String,String]]()
  var prompt        = scala.collection.mutable.ArrayBuffer[String]()
  var lang          = "de"
  def msgs          = messages(lang)
  var userCtx       = Session.get(true)
  var home          = ""
  var serverAddress = "127.0.0.1"
  var csrf          = "" 
  var callerId      = CallerIdent("000000")
  var status        = AppStatus.load
  var mockupMode    = false   // enable mockup usecase execution
  var debugMode     = false   // enable debug mode

  def getDebugLevel():Option[String] =  {
    getLocalStorage("AppEnv.LogLevel") match {
       case "error" => Some("error")
       case "warn"  => Some("warn")
       case "info"  => Some("info")
       case "debug" => Some("debug")
       case _       => None
    }
  }  
  
  def setDebugLevel(value: String=""): Unit=  {
    debugMode = true
    value match {
       case "error" => setLocalStorage("AppEnv.LogLevel", "error");  logger = new org.scalajs.logging.ScalaConsoleLogger(Level.Error)
       case "warn"  => setLocalStorage("AppEnv.LogLevel", "warn");   logger = new org.scalajs.logging.ScalaConsoleLogger(Level.Warn)
       case "info"  => setLocalStorage("AppEnv.LogLevel", "info");   logger = new org.scalajs.logging.ScalaConsoleLogger(Level.Info) 
       case "debug" => setLocalStorage("AppEnv.LogLevel", "debug");  logger = new org.scalajs.logging.ScalaConsoleLogger(Level.Debug)
       case _       => setLocalStorage("AppEnv.LogLevel", "");       logger = org.scalajs.logging.NullLogger; debugMode = false 
    }
  } 

  def debug(func: => String, msg: =>String) = logger.debug(s"${func}-> ${msg}")
  def info(func:  => String, msg: =>String) = logger.info(s"${func}-> ${msg}")
  def warn(func:  => String, msg: =>String) = logger.warn(s"${func}-> ${msg}")
  def error(func: => String, msg: =>String) = logger.error(s"${func}-> ${msg}")



  def initMockup() = { mockupMode = getLocalStorage("AppEnv.Mockup").toBooleanOption.getOrElse(false) }
  def setMockup(value: Boolean) = { setLocalStorage("AppEnv.Mockup", value.toString); mockupMode=value }
  def getMockup()  = mockupMode

  def initContext(context: String, csrfToken: String=""): Boolean = {
    import java.nio.charset.StandardCharsets
    import org.encoding.Base64._
    val date = new js.Date

    val result = try {
      userCtx = read[Session](new String(context.toByteArray, StandardCharsets.UTF_8))
      true
    } catch { case _: Throwable => {
      error("initContext", "couldn't read user context")
      userCtx = Session.get(true) 
      false
    }}

    callerId = CallerIdent(date.getTime.toString.takeRight(6))
    debug("initContext", s"callerId: ${callerId.toString}")

    if (csrfToken != "") csrf = csrfToken
    result
  }

  def initHome(): Future[Boolean] = {
    home = dom.window.location.protocol + "//" + dom.window.location.host
    if (dom.window.location.host.startsWith("localhost")) 
      getIpAddress.map {
        case Left(err)     => { serverAddress = home; false }
        case Right(ipAddr) => { serverAddress = dom.window.location.protocol + "//" + dom.window.location.host.replace("localhost", ipAddr); true }
      }
    else { serverAddress = home; Future(true) }
  }

  def resetContext(): Unit = { 
    userCtx = Session.get(userCtx.runModeLocal) 
  }

  def getOrganizer   : String  = { userCtx.organizer }
  def getOrgDir      : String  = { userCtx.orgDir }
  def getOrgId       : Long    = { userCtx.orgId } 
  def getEmail       : String  = { userCtx.email }
  def getFullName    : String  = { userCtx.fullName }
  def isRunModeLocal : Boolean = { userCtx.runModeLocal }
  def isAdmin()      : Boolean = { userCtx.admin }
  def getCsrf()      : String  = csrf

  def setToId(toId: Long) = { 
    status.toId = toId
    ctrlSidebar(status.ucName, status.toId)
    setHeader()
  }


  // ctrlSidebar - initialize sidebar for normal user, organizer or admin
  def ctrlSidebar(ucName: String, toId: Long): Unit = {
    val validToId  = (toId > 0)

    val validOrgId = (AppEnv.getOrgId > 0)
    val admin      = AppEnv.isAdmin

    //AppEnv.debug("ctrlSidebar", s"ucName: ${ucName} toId: ${status.toId}")

    // show normal entries (not for organizer)
    setVisibleByAttr_("sbentry", "HomeSearch", !validOrgId)
    setVisibleByAttr_("sbentry", "InfoDisabled", false)
    setVisibleByAttr_("sbentry", "InfoEnabled",  !validOrgId & validToId )

    // special entries for organizer
    setVisibleByAttr_("sbentry", "OrganizeTourney", validOrgId)
    setVisibleByAttr_("sbentry", "OrganizeCompetition", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizePlayer", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizePlayfield", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizeCertificate", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizeReport", validOrgId & validToId)

    setVisibleByAttr_("sbentry","Admin", admin)
    if (ucName != "" & ucName != "HomeMain") {
      try markSBEntry(ucName)
      catch { case _: Throwable => AppEnv.warn("setSidebar", s"ucName: ${ucName}")}
    } 
    ()
  }
  

  def setStatus(name: String, param: String, info: String) = { status.ucName = name; status.ucParam = param; status.ucInfo = info }
  def getStatus  = status
  def saveStatus = status.save


  /** initStateHandler handles history state and unload
    * 
    */
  def initStateHandler() = {
    dom.window.addEventListener("popstate", (evt: dom.PopStateEvent) => eventPopstate(evt) , true)
    dom.window.addEventListener("beforeunload", (evt: dom.BeforeUnloadEvent) => eventBeforeUnload(evt) , true)
  }

  // eventBeforeUnload
  def eventBeforeUnload(event: dom.BeforeUnloadEvent): Unit = {
    //AppEnv.setLocalStorage("BeforeUNLOAD","XXX")
    //debug("eventBeforeUnload", s"event")
  }

  // eventPopstate
  def eventPopstate(e: dom.PopStateEvent): Unit = {
    if (e.state != null) {
      val uc = read[(String,String,String)](e.state.toString)
      val url = s"${home}/start?ucName=${uc._1}&ucParam=${uc._2}&ucInfo=${uc._3}"
      dom.window.history.replaceState(e.state.toString, getMsg("title"), url)
      App.execUseCase(uc._1, uc._2, uc._3, false) //dom.window.location.href = url
    }
  }

  def setHistory(ucName: String, ucParam: String, ucInfo: String): Unit = {
    val url = s"${home}/start?ucName=${ucName}&ucParam=${ucParam}&ucInfo=${ucInfo}"
    dom.window.history.replaceState(write((ucName, ucParam, ucInfo)), getMsg("title"), url)
  } 


  /** init messages from server when necessary
   *  
   *  Update when message "appmain.date" contains new/different value
   *  than last value
   */  
  def initMessages(lastUpdate: String, language: String): Future[Boolean] = {
    val localMessages = getLocalStorage("AppEnv.Messages")
    val localUpdate   = getLocalStorage("AppEnv.LastUpdate")
    
    lang = language

    if (localMessages == "" | (getLocalStorage("AppEnv.LogLevel") == "debug") | localUpdate != lastUpdate) {
      Ajax.get("/getMessages").map(_.responseText).map(content => {
        setLocalStorage("AppEnv.Messages", content)
        setLocalStorage("AppEnv.LastUpdate", lastUpdate)
        messages = read[ Map[String,Map[String,String]]] (content)
        println(s"loadMessages => update(${localUpdate}/${lastUpdate})")
        true
      })
    } else {
      messages = read[ Map[String,Map[String,String]]] (localMessages)
      println(s"loadMessages => local messages loaded (${localUpdate}/${lastUpdate})")
      Future(true)
    }
  }


  // getMessage
  def getMessage(key: String, args: String*): String = try {
    var m = messages(lang)(key)
    args.zipWithIndex.foreach{ case(x,i) => m = m.replace(s"{${i}}",x) }
    m
  } catch { case _: Throwable => println(s"getMessage: key ${key} args:${args} failed"); key }

  // getLang
  def getLang: String = lang

  //getVersion
  def getVersion(): String = getMessage("app.version")

  /** getLocalStorage
   *
   * @param name of storage element
   *
   */
  def getLocalStorage(name: String): String = {
    try {
      val s = dom.window.localStorage.getItem(name) 
      if (s == null) "" else s
    } catch { case _: Throwable => "" }
  }

  def getLocalStorage(name: String, default: Boolean): Boolean = {
    try {
      val s = dom.window.localStorage.getItem(name) 
      if (s == null) false else s.toBooleanOption.getOrElse(false)
    } catch { case _: Throwable => false }
  }

  def getArrayBuffer(name: String): Either[Error, ArrayBuffer[String]] = {
    try {
      Right(read[ArrayBuffer[String]](dom.window.localStorage.getItem(name) ))
    } catch { case _: Throwable => Left(Error("err0198.getArrayBiffer", name)) }
  }


  /** setLocalStorage
   *
   * @param name of storage element
   * @param content new valueo storage element
   */
  def setLocalStorage(name: String, content: String): Unit = {
    try dom.window.localStorage.setItem(name, content)
    catch { case _: Throwable => () }
  }

  def setLocalStorage(name: String, value: Boolean): Unit = {
    try dom.window.localStorage.setItem(name, value.toString)
    catch { case _: Throwable => () }
  }

  def setArrayBuffer(name: String, value: ArrayBuffer[String]): Unit = {
    try dom.window.localStorage.setItem(name, write[ArrayBuffer[String]](value))
    catch { case _: Throwable => () }
  }


  /** initCookie - check settings of cookie show cookie dialog
   * 
   */
  def initCookie(withCookie: Boolean): Future[Boolean] =
    if (withCookie && !getLocalStorage("AppEnv.Cookie", false)) {
      DlgBox.standard(getMessage("dlg.box.cookietitle"), getMessage("dlg.box.cookiemessage"), Seq("ok"))
        .flatMap { _ match {
          case 1 => setLocalStorage("AppEnv.Cookie", true);   Future(true)
          case _ => println("initCookie => cookies not wanted"); Future(false)
      }}
    } else {
      Future(true)
    }

  def getHome(value: String): String = {
    val (proto, host) = if (value.startsWith("https://")) {
      ("https://", value.substring(8))
    } else if (value.startsWith("http://")) {
      ("http://", value.substring(7))
    } else {
      ("", value)
    }
    val index = host.indexOf("/")
    if (index > 0) {
      proto + host.substring(0, index)
    } else {
      proto + host
    }
  }  

}