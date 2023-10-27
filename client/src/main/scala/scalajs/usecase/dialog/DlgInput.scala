package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
//import scala.scalajs.js._
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom
import org.scalajs.dom.raw

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import clientviews.dialog.html
import shared.utils.UseCaseParam
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }

// --------------------------------------------------------------------------
// Dialog Box Info Handling
// -------------------------------------------------------------------------- 
@JSExportTopLevel("DlgInput") 
object DlgInput extends BasicHtml   
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgInput", "dlg.input", "DlgInput", "dlginput", scalajs.AppEnv.getMessage _ )

  def show(title: String, btnName: String, ucName: String) : Unit = {
    loadModal(html.DlgInput())
    $("#APP__DlgInput").modal("show")
    setInput("UseCase", ucName)

    val elem = dom.document.getElementById("APP__DlgInput__Input").asInstanceOf[raw.HTMLInputElement]
    elem.onkeydown = {(e: raw.KeyboardEvent) =>
      if (Seq(13).contains(e.keyCode.toInt)) {
        e.preventDefault()
        App.ucMap(ucName).actionEvent("ENTER", elem, e)
        //info("onkeydown", "ENTER")
      }
      if (Seq(38).contains(e.keyCode.toInt)) {
        e.preventDefault()
        App.ucMap(ucName).actionEvent("ArrowUp", elem, e)
      }
    }
    setHtml("Title", title)
    setHtml("BtnSubmit", btnName)
    $("#APP__DlgInput").on("shown.bs.modal", () => $("#APP__DlgInput__Input").focus() )

  }


 /** actionEvent
    * 
    * @param key
    * @param _event
    * @param elem
    */
  @JSExport 
  def actionEvent(key: String, elem: raw.HTMLElement, event: dom.Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    try {
      val ucName = getInput(gE(uc("UseCase")))
      val ikey = event.`type` match {
        case "click"  => "CLICK"
        case "change" => "CHANGE"
        case "input"  => "INPUT"
        case _        => "UNKNOWN"
      } 
      App.ucMap(ucName).actionEvent(ikey, elem, event)
    } catch { case _:Throwable => error("actionEvent", "invalid action")}
  }

  def hide = { $("#APP__DlgInput").modal("hide") }
  def get = getInput(gE(uc("Input")), "")  
  def add(content: String) = DlgInput.set(getInput(gE(uc("Output")), "") + "\n" + content)
  def set(value: String) = setHtml("Output", value)
  def clear = setInput("Input", "")
  def focus = dom.document.getElementById("APP__DlgInput__Input").asInstanceOf[raw.HTMLInputElement].focus()

}