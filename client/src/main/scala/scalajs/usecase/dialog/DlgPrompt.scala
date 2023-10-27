package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLInputElement
import org.scalajs.dom.raw.KeyboardEvent
import org.scalajs.dom.raw.Event

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._

import clientviews.dialog.html
import shared.utils.{ UseCaseParam, UCP }
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }

// --------------------------------------------------------------------------
// Dialog Box like prompt with keys
// -------------------------------------------------------------------------- 
@JSExportTopLevel("DlgPrompt") 
object DlgPrompt extends BasicHtml   
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("DlgPrompt", "dlg.prompt", "DlgPrompt", "dlgprompt", scalajs.AppEnv.getMessage _ )

  var actionfunc: (String, HTMLElement, dom.Event)=>Unit = null

  def show(title: String, initValue: String, afunc:(String, HTMLElement, dom.Event)=>Unit): Future[Either[String, String]] = {
    val p = Promise[Either[String, String]]()
    val f = p.future
  
    def dialogBoxCancel() = if (!p.isCompleted) p success Left("CANCEL")
    def dialogBoxClick(e: Event) = if (!p.isCompleted) p success Right(getInput(gE("DlgPrompt__Input")))

    loadModal(html.DlgPrompt(), "DlgPrompt__Modal")
    actionfunc = afunc

    $("#DlgPromp__Modal").on("shown.bs.modal", () => $("#DlgPrompt__Input").focus() )    
    $("[id=DlgPrompt__Close]").click((e: Event)  => dialogBoxCancel()) 
    $("[id=DlgPrompt__Start]").click((e: Event)  => dialogBoxClick(e)) 
    

    val elem = dom.document.getElementById("DlgPrompt__Input").asInstanceOf[HTMLInputElement]
    elem.onkeydown = {(e: KeyboardEvent) =>
      if (Seq(13).contains(e.keyCode.toInt)) {
        // ENTER KEY
        e.preventDefault()
        if (!p.isCompleted) p success Right(getInput(gE("DlgPrompt__Input")))
      }

      if (Seq(38).contains(e.keyCode.toInt)) {
        e.preventDefault()
        //println("KEY UPPP")
        afunc("Up", elem, e)
      }
      
      if (Seq(40).contains(e.keyCode.toInt)) {
        e.preventDefault()
        //println("KEY DOWN")
        afunc("Down", elem, e)
      }
    }

    $("#DlgPrompt__Modal").modal("show")
    setHtml("Title", title)
    setInput("Input", initValue)
    $("#DlgPrompt__Modal").on("hide.bs.modal", () => dialogBoxCancel())
    f.map {
      case Left(err)  => $("#DlgPrompt__Modal").modal("hide"); Left(err)
      case Right(res) => $("#DlgPrompt__Modal").modal("hide"); Right(res)
    }
  }


 /** actionEvent
    * 
    * @param key
    * @param _event
    * @param elem
    */
  @JSExport 
  def actionEvent(key: String, elem: HTMLElement, event: dom.Event) = {
    // debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    // try {
    //   val ikey = event.`type` match {
    //     case "click"  => "CLICK"
    //     case "change" => "CHANGE"
    //     case "input"  => "INPUT"
    //     case _        => "UNKNOWN"
    //   } 
    //   actionfunc(ikey, elem, event)
    // } catch { case _:Throwable => error("actionEvent", "invalid action")}

    key match {
      case "Submit" => { println("SUBMIT!") }
      case "Cancel" => { println("CANCEL!") }
      case _        => {} //error("actionEvent", s"unknown key ${key}")

    }

  }

  def hide = { $("#DlgPrompt").modal("hide") }
  def get = getInput(gE("DlgPrompt__Input")) 
  def set(value: String) = setInput("DlgPrompt__Input", value)(UCP())
  def clear = setInput("Input", "")
  def focus = dom.document.getElementById("DlgPrompt__Input").asInstanceOf[HTMLInputElement].focus()

}