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

case class TNP(name: String, param: String)

/** UseCase - based on name, definition of several basic artefacts
 *  
 *  - idBase: Html id attribute (idBase)  
 *  - msgBase: prefix of message label
 *  - expName: name of exported objecet accessable form javascript
 *  - dataAttrPref: Html universal data attribute (data-<dataAttrPref>-<xxx>)
 */  
abstract class TestUseCase(val name: String) extends BasicHtml
{

  this: BasicHtml =>

  def actionEvent(key: String, elem: raw.HTMLElement, event: Event): Unit = {}

  def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)): Unit = {}
  def sidebar: Boolean = true
  def check(): Boolean = { true }
  def save(param: String = ""): Unit = {} 
  def Trny = App.tourney

  val idBase = if (name == "") "APP" else s"APP__${name}"
  val msgBase = if (name == "") "app" else name.split("(?=[A-Z])").mkString(".").toLowerCase 
  val expName = if (name == "") "App" else name
  val dataAttrPref = if (name == "") "app" else name.split("(?=[A-Z])").mkString("-").toLowerCase

  def FAILED(test: String, msg: =>String)(implicit ucp: UseCaseParam) = AppEnv.logger.error(s"FAILED  ${name}.${test} -> ${msg}")
  def SUCCESS(tnp: TNP)(implicit ucp: UseCaseParam) = AppEnv.logger.info(s"SUCCESS ${name}.${tnp.name}")
  def START(tnp: TNP)(implicit ucp: UseCaseParam) = AppEnv.logger.info(s"START ${name}.${tnp.name} => param: ${tnp.param}")

  implicit val ucp=UseCaseParam(idBase, msgBase, expName, dataAttrPref, AppEnv.getMessage _) 
}

object TestUseCase {
  val defaultParam = UseCaseParam("APP", "app", "App", "app", AppEnv.getMessage _ )  
}
