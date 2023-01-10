package addon.test

/*
** test -s compphase -n 1
**
**
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
  
  // Commands

  @JSExport def showCompPhase()                   = AddonCmds.showCompPhase()
  @JSExport def showTourney                       = AddonCmds.showTourney()
  
 // Organize Competition tests
  @JSExport def testOrgCompDraw(param: String)    = AddonOrgComp.testDraw(TNP("testDraw", param))
  @JSExport def testOrgCompDrawKo(param: String)  = AddonOrgComp.testDrawKo(TNP("testDrawKo", param))
  @JSExport def testOrgCompInputKo(param: String) = AddonOrgComp.testInputKo(TNP("testInputKo", param))
  @JSExport def testMatchEncode(param: String)    = AddonOrgComp.testMatchEncode(TNP("testMatchEncode", param))

  // Organize Competition Match tests
  @JSExport def testMatchList(coId: String, coPhId: String) = AddonOrgComp.testMatchList("testMatchList", coId.toLong, coPhId.toInt)
  @JSExport def testPlayerRun(coId: String, coPhId: String) = AddonOrgComp.testPlayerRun("testPlayerRun", coId.toLong, coPhId.toInt)


  

  /** execute command
   * 
   */ 
  def cmdExecute(args: Array[String]) = {
    try {
      args(0) match {
        case "load"            => AddonCmds.load(args(1))
        case "getDebug"        => println(s"Debug Level: ${AppEnv.getDebugLevel.getOrElse("not set")}") 
        case "setDebug"        => AppEnv.setDebugLevel(args(1))
        case "testCompEncode"  => AddonComp.testEncode(args(1))
        case "testShowSubMenu" => AddonSidebar.testShowSubMenu("testShowSubMenu", args(1))
      }

    } catch { case _:Throwable => println(s"Exception cmdStart: ${args.mkString(" ")}")}
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



}

