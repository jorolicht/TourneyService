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
           |  show - show/dump tourney objects
           |  sync - purge local tourney data to server 
           |
           |Options:
           |""".stripMargin)
  footer("\nFor all other tricks, consult the documentation!")
  verify()
}



object Console 
{

  def execute(cmd: String): Unit = {   
    val args  = cmd.split(" ")
    val args1 = args.patch(0,Nil,1)
    args(0).toLowerCase match {
      case "log"   => addon.test.AddonMain.cmdLog(args1)
      case "test"  => addon.test.AddonMain.cmdTest(args1)
      case "show"  => addon.test.AddonMain.cmdShow(args1)
      case "save"  => addon.test.AddonCmds.save()
      case "sync"  => addon.test.AddonCmds.sync()
      case "load"  => addon.test.AddonCmds.load(args1(0))

      case _      => {
        val conf = new ConfMain(Seq())
        conf.printHelp()
      }
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

    val initVal = if (pHistory.length > 0) { 
      pPosition = 0
      pHistory(0) 
    } else {
      pPosition = -1
      ""
    }  

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
