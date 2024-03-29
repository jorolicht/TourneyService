package addon.test

/*
** test -s compphase -n 1
**
*/
import org.rogach.scallop._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}


//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service imports
import shared.utils._
import shared.utils.Routines._

import shared.model._
import shared.utils.Constants._

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


@JSExportTopLevel("Start")
object AddonMain extends TestUseCase("AddonMain") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  // execute console/addon command
  def execute(cmd: String): Future[Boolean] = {   
    class ConfMain(arguments: Seq[String]) extends ScallopConf(List("log", "dump")) {
      override def onError(e: Throwable): Unit = e match {
        case _ => 
      }
      version("TourneyService 1.2.3 (c) 2023 Robert Lichtenegger")
      banner("""Usage: <command> <arguments>
              | 
              |Commands:
              |
              |  load - load tourney as an organizer
              |  log  - set log level
              |  show - show/dump tourney objects
              |  sync - purge local tourney data to server
              |  save - purge/save server db to disk
              |  set  - set DOM Elments
              |  test - test various Web dialogs and internal tourney functionality
              |
              |Options:
              |""".stripMargin)
      footer("\nFor all other tricks, consult the documentation!")
      verify()
    }

    val args  = cmd.split(" ")
    val args1 = args.patch(0,Nil,1)
    args(0).toLowerCase match {
      case "log"   => cmdLog(args1)
      case "show"  => cmdShow(args1)
      case "sync"  => cmdSync(); Future(true)
      case "set"   => cmdSet(args1)
      case "test"  => cmdTest(args1)
      case "save"  => cmdSave()
      case "load"  => cmdLoad(args1(0))
      case _       => { val conf = new ConfMain(Seq()); conf.printHelp(); Future(true) }
    }
  }



  /** log command
   * 
   */ 
  def cmdLog(args: Array[String]): Future[Boolean] = {

    class ConfLog(arguments: Seq[String]) extends ScallopConf(arguments) {
      override def onError(e: Throwable): Unit = e match {
        case _ => printHelp()
      }
      //version("TourneyService 1.2.3 (c) 2022 Robert Lichtenegger")
      banner("""Usage: log --level [off|error|warn|info|debug]
                |setting the log level, output displayed on javascript console
                |Options:
                |""".stripMargin)

      val level = choice(name="level", choices= Seq("off", "error", "warn", "info", "debug"))
      verify()
    }    

    val conf = new ConfLog(args)
    val level = conf.level.getOrElse("show") 
    level match {
      case "error" | "warn" | "info" | "debug"  => AppEnv.setDebugLevel(level)
      case "off"   => AppEnv.setDebugLevel("")
      case "show"  => ()
    }
    setOutput(s"Current LogLevel: ${AppEnv.getDebugLevel.getOrElse("UNKNOWN")}")
    Future(true)
  }


  /** set command
   * 
   */ 
  def cmdSet(args: Array[String]): Future[Boolean] = {

    class ConfSet(arguments: Seq[String]) extends ScallopConf(arguments) {

      override def onError(e: Throwable): Unit = e match {
        case _ => printHelp()
      }

      banner("""
        |usage: set <element> [<args>]
        |""".stripMargin)

      footer("\n'show help lists available elements.\n")


      val display = new Subcommand("display") {
        val style = choice(name="style", choices= Seq("none", "block"))
        val toId  = opt[Long](name="toId", required = true)
        val id    = opt[String](name="id", required = true)
      }   
      addSubcommand(display)

      verify()
    }

    val conf   = new ConfSet(args)
    conf.subcommand match {
      case Some(conf.display) => {
        val style = conf.display.style.getOrElse("style ???")
        val id = conf.display.id.getOrElse("id ???")
        val toId = conf.display.toId.getOrElse(185L)

        setLoginLoad(toId).map {
          case Left(err)  => false
          case Right(res) => {
            AddonMain.setOutput(s"START set display style ${style} ${id}")
            gE(id).style.setProperty("display", style)
            true
          }
        } 
      }
 
      case _                  => println(s"no subcommand"); Future(false)
    }
  }


  /** show command
   * 
   */ 
  def cmdShow(args: Array[String]): Future[Boolean] = {

    class ConfShow(arguments: Seq[String]) extends ScallopConf(arguments) {

      override def onError(e: Throwable): Unit = e match {
        case _ => printHelp()
      }

      banner("""
        |usage: show <command> [<args>]
        |""".stripMargin)

      footer("\n'show help lists available subcommands.\n")


      val tourney = new Subcommand("tourney", "tourn") {
        val toId    = opt[Long](name="toId", required = true)
      }
      val comp = new Subcommand("comp", "competition") {
        val coId = opt[Long](name="coId", required = true)
      }
      val coph = new Subcommand("phase", "coph") {
        val coId   = opt[Long](name="coId", required = true)
        val coPhId = opt[Int](name="coPhId", required = true)
      }      
      addSubcommand(tourney)
      addSubcommand(comp)
      addSubcommand(coph)

      verify()
    }
    
    val conf   = new ConfShow(args)
    conf.subcommand match {
      case Some(conf.tourney) => {
        val toId = conf.tourney.toId.getOrElse(0L)
        setOutput(s"START show tourney -> toId: ${toId}")
        setLoginLoad(toId).map {
          case Left(err)  => false
          case Right(res) => setOutput(s"SUCCESS show tourney: \n ${App.tourney.toString()}"); true
        }
      }  
      case Some(conf.comp)    => {
        val coId   = conf.coph.coId.getOrElse(0L)
        println(s"subcommand comp -> coId: ${coId}")
        if (!App.tourney.comps.contains(coId)) {
          error("showCompetition", s"competition coId: ${coId} not found")
        } else {
          println(s"${App.tourney.prtComp(coId, AppEnv.getMessage)}")
        } 
        Future(false)
      }  
      case Some(conf.coph)   => {
        val coId   = conf.coph.coId.getOrElse(0L)
        val coPhId = conf.coph.coPhId.getOrElse(0)
        println(s"subcommand coph -> coId: ${coId} coPhId: ${coPhId}")
        if (!App.tourney.cophs.contains((coId, coPhId))) {
          error("showCompPhase", s"competition phase coId: ${coId} coPhId: ${coPhId} not found")
        } else {
          println(s"${App.tourney.cophs((coId, coPhId)).toString}")
        }
         Future(false)
      }  
      case _                  => println(s"no subcommand");  Future(false)
    }
  }

  
  /** test command
   * 
   */ 
  def cmdTest(args: Array[String]): Future[Boolean] = {
    import addon.test._
    class ConfTest(arguments: Seq[String]) extends ScallopConf(arguments) {
      override def onError(e: Throwable): Unit = e match {
        case _ => printHelp()
      }
      version("TourneyService 1.0.3 (c) 2023 Robert Lichtenegger")
      banner("""Usage: test --scope [type|tourney|competition|compphase|player|basic|ctt|referee|download|match] --param <value> --number <number>
              |   Options:
              |""".stripMargin)

      val scope  = choice(name="scope", choices=Seq("type", "tourney", "comp", "coph", "player", "basic", "dialog", "ctt", "referee", "download", "match"))
      val number = opt[Int](name="number")
      val param  = opt[String](name="param")

      val toId  = opt[Long](name="toId")
      val coId  = opt[Long](name="coId")
      val plId  = opt[Long](name="plId")
      val phase = opt[Int](name="phase")
      val game  = opt[Int](name="game")
      
      verify()
    }

    val conf   = new ConfTest(args)
    val scope  = conf.scope.getOrElse("basic")
    val number = conf.number.getOrElse(0)
    val param  = conf.param.getOrElse("")
    val toId   = conf.toId.getOrElse(0L)
    val plId   = conf.plId.getOrElse(0L)
    val coId   = conf.coId.getOrElse(0L)
    val phase  = conf.phase.getOrElse(0)
    val game   = conf.game.getOrElse(0)

    scope match {
      case "basic"     => AddonBasic.execTest(number, param)
      case "dialog"    => AddonDialog.execTest(number, toId, plId, param)
      case "coph"      => AddonCompPhase.execTest(number, toId, coId, phase, param)
      case "ctt"       => AddonCtt.execTest(number, param);       Future(false)
      case "player"    => AddonPlayer.execTest(number, toId, coId, phase, plId, param)
      case "referee"   => AddonReferee.execTest(number, toId, coId, phase, param)
      case "match"     => AddonMatch.execTest(number, toId, coId, phase, game, param)
      case "download"  => AddonDownload.execTest(number, param);  Future(false)
      case "comp"      => AddonComp.execTest(number, toId, coId, phase, param)
      case _           => Future(false)
    }
  }


  /** save command
   * 
   */ 
  def cmdSave() = {
    val toId = App.tourney.id

    setOutput("START save")
    if (toId == 0) {
      setOutput(s"ERROR save: saving tourney ${toId} not possible"); Future(false)
    } else {
      saveTourney(toId) map {
        case Left(err)  => setOutput(s"ERROR save: failed with ${err.msgCode}"); false
        case Right(res) => setOutput(s"SUCCSESS save: tourney toId=${toId}"); true
      }  
    }
  }

  /** sync command
   * 
   */ 
  def cmdSync() = {
    val toId = App.tourney.id
    if (toId == 0) {
      info("sync", s"ERROR: sync tourney ${toId} not possible")
    } else {
      syncTourney(toId) map {
        case Left(err)  => error("sync", s"sync tourney ${toId} failed with: ${err.msgCode}")
        case Right(res) => info("save", s"SUCCESS: tourney toId: ${toId} synced")
      }  
    }
  }


  /** load command
   * 
   */ 
  def cmdLoad(toIdStr: String): Future[Boolean] = {
    val toId = toIdStr.toLongOption.getOrElse(0L)

    setOutput("START load")
    setLoginLoad(toIdStr.toLongOption.getOrElse(0L)).map {
      case Left(err)   => false
      case Right(res)  => println(s"TOURNEY: \n ${App.tourney.toString()}"); true 
    }
  }


  def setLoginLoad(toId: Long): Future[Either[Error, Boolean]] = {
    import cats.data.EitherT
    import cats.implicits._ 
    
    if (toId <= 0) {
      setOutput(s"ERROR load: loading tourney ${toId} not possible"); Future(Left(Error("Wrong_toId")))
    } else {
      (for {
        pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
        coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw._2))
        result    <- EitherT(App.loadRemoteTourney(toId))
      } yield { (result, pw) }).value.map {
        case Left(err)    => addOutput(s"ERROR load: ${err}"); Left(err)
        case Right(res)   => {
          addOutput(s"SUCCESS load: Tourney ${toId} loaded")
          Right(true)
        } 
      } 
    }
  }


  def addOutput(msg: String) = {
    import org.scalajs.dom.document
    import org.scalajs.dom.raw.HTMLElement
    val elt = gE("DlgPrompt__OutputText")
    if (elt == null) println(s"addOutput ${msg}") else {
      val content = elt.innerText
      elt.innerText = s"${content}\n${msg}"
    }
  }  

  def setOutput(msg: String) = {
    val elt = gE("DlgPrompt__OutputText")
    if (elt == null) println(s"setOutput ${msg}")
    else             setHtml(elt, msg)
  }  

  @JSExport
  def console(): Unit = {
    // execute first command if set
    // then reseet command
    val command = getData(gE("DemoButton"), "command", "")
    if (command != "") setData(gE("DemoButton"), "command", "")

    prompt(gM("home.main.prompt"), command).map {
      case Left(err)  => {} //println(s"Invalid Console Command or Cancel ${err}")
      case Right(cmd) => execute(cmd).map { x => println(s"EXECUTED: ${cmd} RESULT: ${x}"); console() }
    }
  }
  
  def prompt(title: String, command: String) : Future[Either[String, String]] = {
    import org.scalajs.dom.raw.HTMLElement
    import scalajs.usecase.dialog.DlgPrompt
    import scala.collection.mutable.ArrayBuffer

    val maxLen = 50
    var pPosition = -1
    var pHistory  = AppEnv.getArrayBuffer("AppEnv.prompt") match {
      case Left(err) => ArrayBuffer[String]()
      case Right(aB) => aB
    }

    def actionEvent(key: String, elem: HTMLElement, event: dom.Event) = {
      key match {
        case "Up"   => {
          pPosition = (pPosition + 1) % pHistory.length
          DlgPrompt.set(pHistory(pPosition))
          //println(s"UP: ${pPosition} value: ${pHistory(pPosition)}")
        }
        case "Down" => {
          if (pPosition >= 0) pPosition = pPosition - 1
          if (pPosition >=0 ) DlgPrompt.set(pHistory(pPosition)) else DlgPrompt.set("")
          //println(s"Down: ${pPosition} value: ${pHistory(pPosition)}")
        }
      }
    }

    
    if (command != "") {
      pHistory.insert(0, command.replaceAll(";", " --").replaceAll("="," "))
      if (pHistory.length > maxLen) pHistory.remove(maxLen, 1)
    } 

    val initVal = if (pHistory.length > 0) { pPosition = 0; pHistory(0) } else { pPosition = -1; "" } 

    DlgPrompt.show(title, initVal, actionEvent) map {
      case Left(err)    => Left(err)
      case Right(input) => {
        // save history
        pHistory.insert(0, input)
        if (pHistory.length > maxLen) pHistory.remove(maxLen, 1)
        AppEnv.setArrayBuffer("AppEnv.prompt", pHistory) 
        Right(input)
      }  
    }
    

  }

 
}