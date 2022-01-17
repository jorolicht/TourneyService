package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom._                 // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import shared.model._
import shared.utils._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs._

@JSExportTopLevel("HomeConsole")
object HomeConsole extends UseCase("HomeConsole") with TourneySvc with CmdParseSvc
{
  def isAllDigits(x: String) = x forall Character.isDigit

  override def sidebar: Boolean = false
  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = DlgInput.show(getMsg("title"), getMsg("btn.submit"), this.name)

  override def actionEvent(key: String, elem: raw.HTMLElement, event: dom.Event) = {  
    key match {
      case "CLICK"    => exec( DlgInput.get.split(" "):_* )
      case "ENTER"    => exec( DlgInput.get.split(" "):_* )
      case "ArrowUp"  => exec( AppEnv.getLocalStorage("AppEnv.Console").split("__"):_* )
      case _          => {}
    }
  }

  def add(content: String) = DlgInput.add(content)

  def exec(args: String*) = {
    AppEnv.setLocalStorage("AppEnv.Console", args.mkString("__"))
    args(0) match {
      case "save"   => cmd_save(args:_*)
      case "test"   => cmd_test(args:_*)
      case "debug"  => cmd_debug(args:_*)
      case "mockup" => cmd_mockup(args:_*)
      case "clear"  => DlgInput.clear; DlgInput.set("")
      case "exit"   => cmd_exit(args:_*)
      case _        => {
        DlgInput.set(
        """ 
          | ----------------------- Available Commands -----------------------
          |
          |  debug  - set debug configuration (output on JavaScript-Console)
          |  test   - execute test
          |  mockup - set mockup mode
          |  clear  - reset console/input
          |  save   - save tourney data to file/disk
          |  exit   - exit console 
        """.stripMargin  
        ) 
        DlgInput.clear
      }  
    }
    DlgInput.focus
  }


  /** test command
    * 
    * @param args
    */
  def cmd_save(args: String*): Unit = {

    def showHelp = {
      DlgInput.set(s""" NAME:        save - save tourney to file
                      |
                      | USAGE:       save <tourney Id>
                      |
                      | DESCRIPTION: save tourney with identifier to file
                      |             
                      |""".stripMargin)
      DlgInput.clear
    }

    // read params
    // getParam return tupel ()
    val command = getParam(List(args:_*), List("save"))

    debug("save", s"${command._1.head}")
    if ( isAllDigits(command._1.head) & command._1.size == 1) {
      val toId = command._1.head.toLongOption.getOrElse(0L)
      saveTourney(toId).map {
        case Left(err)  => DlgInput.set(s"Error: ${getError(err)}")  
        case Right(res) => DlgInput.set(s"Result: ${res.toString}")  
      }
      DlgInput.clear
    } else {
      showHelp
    }
  }  


  /** test command
    * 
    * @param args
    */
  def cmd_test(args: String*): Unit = {

    def showHelp = {
      DlgInput.set(s""" NAME:        test - execute test case
                      |
                      | USAGE:       debug [OPTION]... [TESTCASE]...
                      |
                      | DESCRIPTION: either set a level option or a command
                      |   
                      |   OPTION: -o, --option <testoption> 
                      |
                      |   TESTCASE:  name of test              
                      |""".stripMargin)
      DlgInput.clear
    }


    // read params
    val command = getParam(List(args:_*), List("test"))
    val option  = getOptArg(command._1, "option")
    val test    = getParam(option._1)

    if ( (command._2 != None) & isSet(test._2)) {
      test._2.map( TestConsole.start( _ ,option._2.getOrElse("")) )
      DlgInput.clear
    } else {
      showHelp
    }
  }  
    

  /** exit command
    * 
    * @param args
    */
  def cmd_exit(args: String*): Unit = {  
    DlgInput.set("")
    DlgInput.clear
    DlgInput.hide
  }
  
  
  /** mockup command
    * 
    * @param args
    */
  def cmd_mockup(args: String*): Unit = {

    def showHelp = {
      DlgInput.set(s""" NAME:        mockup - show or set mockup status
                      |
                      | USAGE:       debug [COMMAND]...
                      |
                      | DESCRIPTION: set or show mockup mode via command
                      |
                      |   COMMAND: "on" | "off" | "show"                   
                      |""".stripMargin)
      DlgInput.clear
    }

    // read params
    val command = getParam(List(args:_*), List("mockup"))
    val param   = getParam(command._1, List("on", "off", "show"))

    if ( (command._2 != None) &  (param._2 != None) & (param._1.isEmpty)  ) {
      param._2.map { _ match {
        case "off"  => AppEnv.setMockup(false); DlgInput.set("mockup mode switched OFF")
        case "on"   => AppEnv.setMockup(true);  DlgInput.set("mockup mode ON")
        case "show" => if (AppEnv.getMockup) DlgInput.set("mockup mode is ON") else DlgInput.set("mockup mode is OFF") 
        case _      =>  showHelp
      }}

      DlgInput.clear
    } else {
      showHelp
    }
  }


  /** debug command
    * 
    * @param args
    */
  def cmd_debug(args: String*): Unit = {

    // read params
    val command = getParam(List(args:_*), List("debug"))
    val level   = getOptArg(command._1, "level")
    val param   = getParam(level._1, List("show", "off", "test"))

    if ( (command._2 != None) & xor(level._2, param._2) & param._1.isEmpty ) {
      level._2.map { x => x match {
        case "error" | "warn" | "info" | "debug" =>  AppEnv.setDebugLevel(x); DlgInput.set(s"set debug mode to ${x}")
        case _                                   =>  showHelp
      }}
      param._2.map { x => x match {
        case "off"  =>  AppEnv.setDebugLevel(x);  DlgInput.set("debug mode switched off")
        case "show" =>  AppEnv.getDebugLevel match {
          case Some(level) => DlgInput.set(s"current debug level is: ${level}")
          case None        => DlgInput.set("debugging is switched off")
        }  
        case "test" => {
          debug("debug level test", "DEBUG")
          info("debug level test", "INFO")
          warn("debug level test", "WARN")
          error("debug level test", "ERROR")
        } 
        case _      =>  showHelp
      }}
      DlgInput.clear
    } else {
      showHelp
    }

    def showHelp = {
      DlgInput.set(s""" NAME:        debug - show, set or switch of debug level
                      |
                      | USAGE:       debug [OPTION]... [COMMAND]...
                      |
                      | DESCRIPTION: either set a level option or a command
                      |   
                      |   OPTION: -l, --level <level> 
                      |      <level> could be "error" | "warn" | "info" | "debug"
                      |      "error" prints only errors to the javascript console
                      |      "debug" generates the most detailed output
                      |
                      |   COMMAND: "off" | "show"
                      |      switches debug off or shows current debug level
                      |""".stripMargin)
      DlgInput.clear
    }
  }

}  