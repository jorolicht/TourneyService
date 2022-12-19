package scalajs.usecase.home

import org.rogach.scallop._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.yamlijs.YAML                  // facade for yaml
import org.highlight._                   // highlight.org
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import shared.utils._
import shared.model._


import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


class ConfMain(arguments: Seq[String]) extends ScallopConf(List("log", "dump")) {
  override def onError(e: Throwable): Unit = e match {
    case _ => 
  }
  version("TourneyService 1.2.3 (c) 2022 Robert Lichtenegger")
  banner("""Usage: <command> <arguments>
           | 
           |Commands:
           |
           |  log  - set log level
           |  dump - dump tourney objects
           |
           |Options:
           |""".stripMargin)
  footer("\nFor all other tricks, consult the documentation!")
  verify()
}


class ConfDump(arguments: Seq[String]) extends ScallopConf(arguments) {
  override def onError(e: Throwable): Unit = e match {
    case _ => printHelp()
  }
  version("TourneyService 1.2.3 (c) 2022 Robert Lichtenegger")
  banner("""Usage: dump --obj [tourney|competition|compphase|player]
          |dump object of current tourney
          |Options:
          |""".stripMargin)

  val obj    = choice(Seq("tourney", "competition", "compphase", "player"))
  val competiton = opt[Int](descr = "competition identifier[Long]")
  val phase = opt[Int](descr = "competition phase identifier[Int]")
  verify()
}


object Console 
{

  def execute(cmd: String): Unit = {   
    val args  = cmd.split(" ")
    val args1 = args.patch(0,Nil,1)
    args(0).toLowerCase match {
      case "log"  => cmdLog(args1)
      case "test" => addon.test.AddonMain.cmdTest(args1)
      case "dump" => {
        val conf = new ConfDump(args1)
      } 

      case _      => {
        val conf = new ConfMain(Seq())
        conf.printHelp()
        //println("apples are: " + conf.level())
      }
    }
  }  

 
  // log command
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

}  
