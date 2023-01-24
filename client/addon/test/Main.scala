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

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


@JSExportTopLevel("Start")
object AddonMain extends TestUseCase("AddonMain") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
   /** log command
    * 
    */ 
  def cmdLog(args: Array[String]) = {

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
    println(s"Current LogLevel: ${AppEnv.getDebugLevel.getOrElse("UNKNOWN")}")
  }

  
  /** test command
   * 
   */ 
  def cmdTest(args: Array[String]) = {
    import addon.test._
    class ConfTest(arguments: Seq[String]) extends ScallopConf(arguments) {
      override def onError(e: Throwable): Unit = e match {
        case _ => printHelp()
      }
      //version("TourneyService 1.2.3 (c) 2022 Robert Lichtenegger")
      banner("""Usage: test --scope [type|tourney|competition|compphase|player|basic] --param <value> --number <number>
              |Options:
              |""".stripMargin)

      val scope  = choice(name="scope", choices=Seq("type", "tourney", "competition", "compphase", "player", "basic", "dialog"))
      val number = opt[Int](name="number")
      val param  = opt[String](name="param")
      verify()
    }

    val conf   = new ConfTest(args)
    val scope  = conf.scope.getOrElse("unknown")
    val number = conf.number.getOrElse(0)
    val param  = conf.param.getOrElse("")

    scope match {
      case "basic"     => AddonBasic.execTest(number, param)
      case "dialog"    => AddonDialog.execTest(number, param)
      case "compphase" => AddonCompPhase.execTest(number, param)
      case _           => ()
    }
  }


  /** show command
   * 
   */ 
  def cmdShow(args: Array[String]) = {

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
        println(s"subcommand tourney toid: ${toId}")
        AddonCmds.showTourney(toId)
      }  
      case Some(conf.comp)    => {
        val coId   = conf.phase.coid.getOrElse(0L)
        println(s"subcommand comp coid: ${coId}")
        AddonCmds.showCompetition(coId) 
      }  
      case Some(conf.phase)   => {
        val coId   = conf.phase.coid.getOrElse(0L)
        val coPhId = conf.phase.phid.getOrElse(0)
        println(s"subcommand phase coid: ${coId} phid: ${coPhId}")
        AddonCmds.showCompPhase(coId, coPhId) 
      }  
      case _                  => println(s"no subcommand")
    }
  }



 
}

