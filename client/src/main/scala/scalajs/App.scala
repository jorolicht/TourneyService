package scalajs

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"

import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._


// tourney service imports
import shared.utils.Constants._ 
import shared.utils.Routines._
import shared.model._
import shared.utils._

import scalajs.usecase.dialog.DlgBox
import scalajs.usecase.component._
import scalajs.usecase.organize._
import scalajs.usecase.info._
import scalajs.usecase.admin._
import scalajs.usecase.home._
import scalajs.usecase._
import scalajs.service._

import scalajs.usecase.organize.OrganizeTourney

@JSExportTopLevel("App")
object App extends BasicHtml with TourneySvc
{
  //this: BasicHtml =>
  implicit val ucp = UseCase.defaultParam  // UseCaseParam("APP", "app", "App", "app", AppAppEnv.getMessage _ )

  // List of uscases
  var ucList = List(HomeMain, HomeSetting, HomeSearch, HomeLogin, HomeRegister, HomeDemo, HomeMockup,  
                    InfoEnabled, InfoDisabled, InfoCertificate, InfoCompetition, InfoPlayer, 
                    InfoPlayfield, InfoResult, InfoSchedule,
                    OrganizeCertificate, OrganizeCompetition, OrganizePlayer, 
                    OrganizePlayfield, OrganizeReport, OrganizeTourney,
                    OrganizeCompetitionDraw, OrganizeCompetitionInput, 
                    OrganizeCompetitionView, OrganizeCompetitionReferee,
                    OrganizeReport, 
                    AdminDatabase, AdminLicense 
                   )


  val ucMap  = new scala.collection.mutable.HashMap[String, UseCase]
  
  var tourney       = Tourney.init
  var tourneyUpdate = false
  var serverLess    = false

 /** error - entry point of application
   *
   * @param msgCode       - error code
   * @param in1           - first insert for error message
   * @param in2           - 2nd insert for error message
   * @param callStack     - list of all called functions
   * @param lang          - language code
   */  
  @JSExport
  def error(msgCode: String, in1: String, in2: String, callStack: String, lang: String): Unit = {  
    errLog(s"msgCode:${msgCode} in1:${in1} in2:${in2} callStack:${callStack} lang:${lang}")
  }            


  /** main - entry point of application
   *
   * @param name       - encodes action
   * @param context    - session context including user
   * @param language   - 2chars encoded language
   * @param csrfToken  - from server
   * @param lastUpdate - date in string format yyyy-mm-dd from message file (server side)
   */  
  @JSExport
  def start(ucName: String, ucParam: String, ucInfo: String, usrCtx: String, 
            language: String, lastUpdate: String, version: String, csrfToken: String): Unit = {     

    println(s"Startup ucName:${ucName} ucParam:${ucParam} ucInfo:${ucInfo} lang:${language} version: ${version} update:${lastUpdate}")
    setVisible("Info", false)
    

    // initialize debug/mockup and logger
    AppEnv.initMockup()
    AppEnv.initContext(usrCtx, csrfToken)
    AppEnv.initStateHandler()

    // initialize all usecases
    ucList foreach { uc => ucMap.addOne(uc.name, uc) }

    val initOk = for {
      homeSet       <- AppEnv.initHome()
      msgsLoaded    <- AppEnv.initMessages(lastUpdate, language)
      cookieAllowed <- AppEnv.initCookie(
                         if (msgsLoaded) AppEnv.getMessage("config.cookie.confirmation").toBooleanOption.getOrElse(true) else true
                       ) 
    } yield {
      cookieAllowed & msgsLoaded & homeSet
    }  

    // regularly update database
    dom.window.setInterval(() => {
      //println(s"Update Database: ${App.tourneyUpdate}") 
      if (tourneyUpdate) { tourneyUpdate= true; saveLocalTourney(tourney) }
    }, 10000)

    
    val result = initOk.map { _ match {
      // do the actual work and exec the usecase
      case true  => {
        // initialize header with de
        setHeader()
        // set debug level
        AppEnv.setDebugLevel(getOrDefault(AppEnv.getLocalStorage("AppEnv.LogLevel"), AppEnv.getMessage("config.LogLevel")))

        // initialize tourney if basic usecase is called
        if (ucName == "HomeMain") setLocalTourney(Tourney.init) else loadLocalTourney()
        initTrigger(App.tourney.id, AppEnv.callerId, receiveSrvCmd)      
        execUseCase(ucName, ucParam, ucInfo)
      }  
      case false => dom.window.location.href = s"${AppEnv.home}"  
    }}
    ()
  }

  /** execUseCase exec usecase with ucName and ucParam
    *
    * @param ucName
    * @param ucParam
    */
  def execUseCase(ucName: String, ucParam: String="", ucInfo: String="", setHistory: Boolean = true, reload: Boolean = false): Unit = {
    
    try   {      
      val home = dom.window.location.protocol + "//" + dom.window.location.host
      val url = s"${home}/start?ucName=${ucName}&ucParam=${ucParam}&ucInfo=${ucInfo}"
      if (setHistory) {
        dom.window.history.pushState(write((ucName,ucParam,ucInfo)), getMsg("title"), url)
      } else {
        dom.window.history.replaceState(write((ucName, ucParam, ucInfo)), getMsg("title"), url)
      }

      setFooter()
      setHeader()
      showResult(false)
      val (eucName, eucParam, eucInfo) = if (!ctrlAccess(ucName, AppEnv.userCtx)) {
        ("HomeMain", "ErrorCode", "err0085.access.insufficientRights")
      } else if ((AppEnv.getMockup) ) {
        ("HomeMockup", write((ucName, ucParam)), ucInfo)
      } else {
        (ucName, ucParam, ucInfo)
      }
      AppEnv.setStatus(eucName, eucParam, eucInfo)
      if (ucMap(eucName).sidebar) AppEnv.ctrlSidebar(eucName, tourney.id)
      ucMap(eucName).render(eucParam, eucInfo, reload)

    }
    catch { case _: Throwable => HomeMain.render("Error", getError(Error("err0098.usecase.unknown", ucName))) }
  }

  @JSExport
  def playfields(toIdStr: String, language: String, csrfToken: String, lastUpdate: String): Unit = {

    // receiveUpdate - command called from sse - updates fullscreen display
    def receiveUpdate(e: dom.MessageEvent) = {   
      debug("receiveUpdate", s"receiveUpdate event: ${e.data.toString}")
      e.data.toString match {
        case UpdateTrigger(cmd, caId, toId, coId, coPh, grId) => if (toId == tourney.id && caId != AppEnv.callerId) cmd match {
          case "PLAYFIELD" => syncPlayfields map {
            case Left(err)   => error("syncPlayfields", getError(err))
            case Right(res)  => {
              val pf     = tourney.playfields.values.filter(_.coCode != (0L,0)).toSeq
              val pfInfo = tourney.playfields.values.filter(_.coCode == (0L,0)).toSeq
              setMainContent(clientviews.playfield.html.Fullscreen(pf, pfInfo, AppEnv.msgs))
            }
          }
          case _           => debug("receiveUpdate", s"invalid trigger cmd: ${cmd}  toId: ${toId} caId: ${caId} coPh: ${coPh}")
        } else                debug("receiveUpdate", s"invalid trigger check toId: ${toId} and caId: ${caId}")
        case _             => error("receiveUpdate", s"invalid trigger: ${e.data.toString}")
      }
    }
    
    val date = new js.Date

    println(s"START: playfields -> toId: ${toIdStr} language: ${language}")
    AppEnv.callerId = CallerIdent(date.getTime.toString.takeRight(6))
    val toId = toIdStr.toLongOption.getOrElse(0L)
    if (!initTrigger(toId, AppEnv.callerId, receiveUpdate)) println(s"ERROR: initTrigger") else {
      AppEnv.initMessages(lastUpdate, language) map { _ =>
        AppEnv.setDebugLevel(getOrDefault(AppEnv.getLocalStorage("AppEnv.LogLevel"), AppEnv.getMessage("config.LogLevel")))
        getTourney(toId).map {
          case Left(err)    => println(s"ERROR: getTourney -> ${getError(err)}")
          case Right(trny)  => {
            tourney = trny
            syncPlayfields map {
              case Left(err)  => println(s"ERROR: syncPlayfields -> ${getError(err)}")
              case Right(res) => {
                setHtml(gE("PlayfieldTitle"), getMsg("header.title.playfields", tourney.organizer, tourney.name))
                val pf     = tourney.playfields.values.filter(_.coCode != (0L,0)).toSeq
                val pfInfo = tourney.playfields.values.filter(_.coCode == (0L,0)).toSeq
                setMainContent(clientviews.playfield.html.Fullscreen(pf, pfInfo, AppEnv.msgs))
              }  
            }
          }
        }
      }
    }
  }


  /** ctrlAccess - returns true if valid access
   * 
   */ 
  def ctrlAccess(ucName: String, ctx: Session): Boolean = {
    //debug("ctrlAccess", s"ucName: ${ucName} ctx: ${ctx.orgId}")
    ucName match {
      case s if s.startsWith("Organize") => ctx.orgId > 0
      case s if s.startsWith("Admin")    => ctx.admin
      case _                             => true
    }
  }  


  /** initTrigger - setup server sent event
   * 
   * @param id   unique trigger identificator based on milliseconds and random number
   *             if empty then setup new one
   * @param toId tourney identifier to watch for changes
   */ 
  def initTrigger(toId: Long, id: CallerIdent, trigFun:(dom.MessageEvent) => Unit): Boolean = if (toId > 0) {
    import scala.util.Random
    try {
      val trigCmd = s"/trigger?id=${toId}_${id}"
      if (global.window.EventSource.toString != "undefined") {
        debug("initTrigger", trigCmd)
        //setup server send events
        var sse = new dom.raw.EventSource(trigCmd)  
        sse.onmessage = trigFun
        true
      } else { error("initTrigger", trigCmd); false }
    } catch { case e: Throwable => error("initTrigger", s"error: ${e.toString}"); false }
  } else false

  // // setTrigger for a special toId
  // def setTrigger(toId: Long): Future[Either[Error, Boolean]]= {
  //   val id = AppEnv.getLocalStorage("AppEnv.trigger")
  //   val path = s"/trigger?id=${id}&toId=${toId}&init=false"
  //   Ajax.get(path).map(_.responseText)
  //     .map(res => Return.decode2Boolean(res, "setTrigger"))
  //     .recover({
  //       case dom.ext.AjaxException(req) => Left( Error.decode(req.responseText, s"text: ${req.responseText.take(40)} path: ${path}", "setTrigger") )
  //       case _: Throwable               => Left( Error("err0007.ajax.getJson", path, "unspecified exception", "setTrigger") ) 
  //     })
  // }  


  def receiveSrvCmd(e: dom.MessageEvent) = {    
    debug("receive", s"server command: ${e.data.toString}")
    e.data.toString match {
      case UpdateTrigger(cmdName, ident, toId, coId, coPh, grId) => {
        val updt = UpdateTrigger(cmdName, ident, toId, coId, coPh, grId)
        if (toId == tourney.getToId & CallerIdent(ident) != AppEnv.callerId) {
          cmdName match {
            case "All"              => for { x <- updateTourney(toId)          } yield updViews(updt) 
            case "Tourney"          => for { x <- updateTourney(toId)          } yield updViews(updt) 
            case "Competition"      => for { x <- updateCompetition(toId)      } yield updViews(updt) 
            case "Club"             => for { x <- updateClub(toId)             } yield updViews(updt) 
            case "Player"           => for { x <- updatePlayer(toId)           } yield updViews(updt) 
            case "Pant2Comp"        => for { x <- updatePant2Comp(toId)        } yield updViews(updt) 
            case "PLAYFIELD"        => for { x <- updatePlayfield(toId)        } yield updViews(updt) 
            case "CompPhase"        => updateCompPhase(toId, coId, coPh).map { case _ => updViews(updt) }
            case "MatchReset"       => resetResults(coId,coPh); updViews(updt)
            case _                  => debug("receive", s"unknown trigger command: ${e.data.toString}")
          }
        }
      }  
      case cmd if cmd.startsWith("Cmd_")    => { val cmdArr = cmd.split("_");  execCmd(cmdArr(1), cmdArr(2))  }
      case cmd if cmd.startsWith("Test_")   => { val cmdArr = cmd.split("_");  execTest(cmdArr(1), cmdArr(2))  }
      case _                                => debug("receive", s"unknown command: ${e.data.toString}")
    }
  }


  def execCmd(arg1: String, arg2: String): Unit = {
    debug("execCmd", s"arg1: ${arg1} arg2: ${arg2}")
  }

  def execTest(arg1: String, arg2: String): Unit = {
    debug("execTest", s"arg1: ${arg1} arg2: ${arg2}")
    execUseCase("HomeTest", arg1, arg2)
  }  


  def resetResults(coId: Long, coPh: Int): Unit = 
    if (tourney.cophs.isDefinedAt((coId, coPh))) {
      tourney.cophs((coId, coPh)).resetResults
      saveLocalTourney(tourney)
    }

  def updatePlayfield(toId: Long): Future[Boolean] = {
    getAction("getPlayfields", App.tourney.id, "").map {
      case Left(err)     => error("updatePlayfield", getError(err)); false
      case Right(pfSeq)  => {
        try {
          val pfS = read[Seq[Playfield]](pfSeq)
          tourney.playfields = collection.mutable.HashMap( pfS.map { p => { p.nr -> p }} : _*)
          saveLocalTourney(tourney)
          true
        } catch { case _:Throwable => false}
      }
    }
  }    
      
  def updatePant2Comp(toId: Long): Future[Boolean] = 
    getPant2Comps(toId).map {
      case Left(err)     => error("updatePant2Comp", getError(err)); false
      case Right(p2cSeq) => { 
        tourney.pl2co   = collection.mutable.HashMap( p2cSeq.map { p2c => (p2c.sno, p2c.coId) -> p2c } :_*)
        saveLocalTourney(tourney)
        true
      }
    }

  // updates competition phase, returns Error or status of the competition phase  
  def updateCompPhase(toId: Long, coId: Long, coPhId:Int): Future[Either[Error, CompPhaseStatus.Value]] = 
    getCompPhase(coId, coPhId).map {
      case Left(err)   => Left(err)
      case Right(coph) => {
        tourney.cophs((coId, coPhId)) = coph
        saveLocalTourney(tourney)
        Right(coph.status)
      }
    }



  def updateCompetition(toId: Long): Future[Boolean] = 
    getComps(toId).map {
      case Left(err)    => error("updateCompetition", getError(err)); false
      case Right(comps) => {
        tourney.comps = collection.mutable.HashMap( comps.map { co => co.id -> co } :_* )
        saveLocalTourney(tourney)
        true
      }
    }

  def updateClub(toId: Long): Future[Boolean]  = 
    getTournClubs(toId).map {
      case Left(err)     => error("updateClub", getError(err)); false
      case Right(clubs) => {
        tourney.clubs   = collection.mutable.HashMap( clubs.map { cl => cl.id -> cl } :_* ) 
        saveLocalTourney(tourney)
        true
      }
    }

  def updatePlayer(toId: Long): Future[Boolean]  = 
    getTournPlayers(toId).map {
      case Left(err)      => error("updatePlayer", getError(err)); false
      case Right(players) => {
        tourney.players   = collection.mutable.HashMap( players.map { pl => pl.id -> pl } :_* )
        saveLocalTourney(tourney)
        true
      }
    }

  def updateTourney(toId: Long): Future[Boolean] = 
    getTourney(toId).map {
      case Left(err)   => { error("updateTourney", getError(err)); false }  
      case Right(trny) => { setLocalTourney(trny); true }
    }


  // perform view update when necessary
  def updViews(upd: UpdateTrigger) = 
    try   ucMap(AppEnv.getStatus.ucName).update("", upd)
    catch { case _: Throwable => error("updViews", s"unknown current action: ${AppEnv.getStatus}")}


  def loadRemoteTourney(toId: Long): Future[Either[Error, Boolean]] = 
    if (toId == 0) {
      resetLocalTourney()
      Future(Right(true))
    } else {
      getTourney(toId).map { 
        case Left(err)   => AppEnv.error(s"getTourney", getErrStack(err)); resetLocalTourney(); Left(err)
        case Right(trny) => setLocalTourney(trny); Right(true) 
      }
    }

  def loadLocalTourney(): Unit = {
    Tourney.decode(AppEnv.getLocalStorage("AppEnv.Tourney")) match {
      case Left(err)   => AppEnv.error(s"loadLocalTourney", getErrStack(err)); setLocalTourney(Tourney.init)
      case Right(trny) => tourney = trny
    } 
  }

  def setLocalTourney(trny: Tourney): Unit = {
    println(s"setLocalTourney", s"Tourney id: ${trny.id} / ${tourney.id}  name: ${trny.name}")
    tourney = trny
    saveLocalTourney(trny)
  }

  def saveLocalTourney(trny: Tourney): Unit = {
    AppEnv.setToId(trny.id)
    try AppEnv.setLocalStorage("AppEnv.Tourney", write(trny))
    catch { case _: Throwable => AppEnv.error("saveLocalTourney(error)", "couldn't write to local storage") }    
  }
  
  def resetLocalTourney(): Unit = { tourney = Tourney.init; saveLocalTourney(tourney) }

}