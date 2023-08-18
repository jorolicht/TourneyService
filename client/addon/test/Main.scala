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
              |  log  - set log level
              |  show - show/dump tourney objects
              |  sync - purge local tourney data to server 
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
      case "test"  => cmdTest(args1)
      case "save"  => cmdSave()
      case "load"  => cmdLoad(args1(0))
      case _      => { val conf = new ConfMain(Seq()); conf.printHelp(); Future(true) }
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
        val toid    = opt[Long](name="toid", required = true)
      }
      val comp = new Subcommand("comp", "competition") {
        val coid = opt[Long](name="coid", required = true)
      }
      val phase = new Subcommand("phase", "compphase") {
        val coid = opt[Long](name="coid", required = true)
        val phid = opt[Int](name="phid", required = true)
      }      
      addSubcommand(tourney)
      addSubcommand(comp)
      addSubcommand(phase)

      verify()
    }
    
    val conf   = new ConfShow(args)
    conf.subcommand match {
      case Some(conf.tourney) => {
        val toId = conf.tourney.toid.getOrElse(0L)
        showTourney(toId)
      }  
      case Some(conf.comp)    => {
        val coId   = conf.phase.coid.getOrElse(0L)
        println(s"subcommand comp coId: ${coId}")
        if (!App.tourney.comps.contains(coId)) {
          error("showCompetition", s"competition coId: ${coId} not found")
        } else {
          println(s"${App.tourney.prtComp(coId, AppEnv.getMessage)}")
        } 
        Future(false)
      }  
      case Some(conf.phase)   => {
        val coId   = conf.phase.coid.getOrElse(0L)
        val coPhId = conf.phase.phid.getOrElse(0)
        println(s"subcommand phase coid: ${coId} phid: ${coPhId}")
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
      banner("""Usage: test --scope [type|tourney|competition|compphase|player|basic|ctt|player] --param <value> --number <number>
              |   Options:
              |""".stripMargin)

      val scope  = choice(name="scope", choices=Seq("type", "tourney", "competition", "compphase", "player", "basic", "dialog", "ctt", "download"))
      val number = opt[Int](name="number")
      val param  = opt[String](name="param")

      val toId  = opt[Long](name="toId")
      val coId  = opt[Long](name="coId")
      val plId  = opt[Long](name="plId")
      
      verify()
    }

    val conf   = new ConfTest(args)
    val scope  = conf.scope.getOrElse("unknown")
    val number = conf.number.getOrElse(0)
    val param  = conf.param.getOrElse("")
    val toId   = conf.toId.getOrElse(0L)
    val plId   = conf.plId.getOrElse(0L)
    val coId   = conf.coId.getOrElse(0L)

    scope match {
      case "basic"     => AddonBasic.execTest(number, param);     Future(false)
      case "dialog"    => AddonDialog.execTest(number, toId, plId, param)
      case "compphase" => AddonCompPhase.execTest(number, param); Future(false)
      case "ctt"       => AddonCtt.execTest(number, param);       Future(false)
      case "player"    => AddonPlayer.execTest(number, toId, plId, param)
      case "download"  => AddonDownload.execTest(number, param);  Future(false)
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
    import cats.data.EitherT
    import cats.implicits._ 
    val toId = toIdStr.toLongOption.getOrElse(0L)

    setOutput("START load")
    if (toId == 0) {
      setOutput(s"ERROR load: loading tourney ${toId} not possible"); Future(false)
    } else {
      (for {
        pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
        coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
        result    <- EitherT(App.loadRemoteTourney(toId))
      } yield { (result, pw) }).value.map {
        case Left(err)    => addOutput(s"ERROR load: ${err}"); false
        case Right(res)   => {
          addOutput(s"SUCCESS load: Tourney ${toIdStr} loaded")
          println(s"TOURNEY: \n ${App.tourney.toString()}")
          true
        } 
      } 
    }
  }

  def addOutput(msg: String) = {
    import org.scalajs.dom.document
    import org.scalajs.dom.raw.HTMLElement
    val elt = document.getElementById("DlgPrompt__OutputText").asInstanceOf[HTMLElement]
    val content = elt.innerText
    elt.innerText = s"${content}\n${msg}"
  }  

  def setOutput(msg: String) = {
    setHtml(gE("DlgPrompt__OutputText"), msg)
  }  


  @JSExport
  def console(): Unit = prompt(getMsg_("home.main.prompt")).map {
    case Left(err)  => println("invalid command/cancel")
    case Right(cmd) => execute(cmd).map {
      case true  => console()
      case false => console()
    } 
  } 
  
  def prompt(title: String) : Future[Either[String, String]] = {
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



  /*
  **
  **  HELPER
  **
  */

  // show tourney
  def showTourney(toId: Long): Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    setOutput("START show tourney")
    if (toId == 0) {
      setOutput(s"ERROR show tourney: loading tourney ${toId} not possible")
      Future(false)
    } else {
      (for {
        pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
        coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
        result    <- EitherT(App.loadRemoteTourney(toId))
      } yield { (result, pw) }).value.map {
        case Left(err)    => setOutput(s"ERROR show tourney: error message: ${err}"); false
        case Right(res)   => setOutput(s"SUCCESS show tourney: \n ${App.tourney.toString()}"); true
      } 
    }
  }

 
}

