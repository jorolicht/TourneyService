package scalajs.usecase.component

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.highlight._                   // highlight.org

// for scalajs dom routines
import org.scalajs.dom._
//import org.scalajs.dom.document
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._

// tourney service imports
import shared.utils._
import shared.model._
import shared.utils.UseCaseParam

import scalajs.service._
import scalajs.{ App, AppEnv }


/** UseCase - based on name, definition of several basic artefacts
 *  
 *  - idBase: Html id attribute (idBase)  
 *  - msgBase: prefix of message label
 *  - expName: name of exported objecet accessable form javascript
 *  - dataAttrPref: Html universal data attribute (data-<dataAttrPref>-<xxx>)
 */  
abstract class UseCase(val name: String) extends BasicHtml
{

  this: BasicHtml =>
  def render(param: String = "", info: String = "", reload: Boolean = false): Unit

  def actionEvent(key: String, elem: raw.HTMLElement, event: Event): Unit = {}

  def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)): Unit = {}
  def sidebar: Boolean = true
  def check(): Boolean = { true }
  def save(param: String = ""): Unit = {} 

  val idBase = if (name == "") "APP" else s"APP__${name}"
  val msgBase = if (name == "") "app" else name.split("(?=[A-Z])").mkString(".").toLowerCase 
  val expName = if (name == "") "App" else name
  val dataAttrPref = if (name == "") "app" else name.split("(?=[A-Z])").mkString("-").toLowerCase

  implicit val ucp=UseCaseParam(idBase, msgBase, expName, dataAttrPref, AppEnv.getMessage _ ) 
  
  def getCompEnv(elem: HTMLElement):(CompPhase, Long, Int) = {
    try {
      val (coId, coPhId) = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0))
      (App.tourney.cophs((coId, coPhId)), coId, coPhId)
    } catch { case _: Throwable => error(s"getCompEnv", s"data elements for coId and coPhId not found"); (CompPhase.dummy ,0L ,0) }  
  }
}

object UseCase {
  val defaultParam = UseCaseParam("APP", "app", "App", "app", AppEnv.getMessage _ )  
}

/** Converter - converting markdown to html-snipplets
  *  
  *
  */
 @js.native
 @JSGlobal("showdown.Converter")
 class Converter extends js.Object {
   def makeHtml(text: String): String = js.native
   def setOption(optionKey: String, value: Boolean): Unit = js.native
   
   //metadata:
   //showdown.setOption('optionKey', 'value');
 }