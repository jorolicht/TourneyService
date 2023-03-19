package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._ 
import org.scalajs.dom 
import org.scalajs.dom.raw

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

import shared.utils.UseCaseParam
import shared.utils.UCP

import scalajs.usecase.component._
import clientviews.dialog._


// --------------------------------------------------------------------------
// Dialog Box with text input
// -------------------------------------------------------------------------- 
@JSExportTopLevel("DlgInputTxt")
object DlgInputTxt extends BasicHtml 
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("DlgInputTxt", "dlg.input.txt", "DlgInputTxt", "dlginputtxt", scalajs.AppEnv.getMessage _ ) 

  /**
   * show - load twirl template and show it 
   */
  def show(header: String, body: String, placeholder: String, 
           btnClose:String, btnStart:String, check:(String)=>Either[String,Boolean]): Future[Either[String, String]] = {
    val p = Promise[Either[String, String]]()
    val f = p.future
  
    def dialogBoxCancel() = if (!p.isCompleted) { p success Left("CANCEL"); ; $("#DlgInputTxt").modal("hide") }
    
    def dialogBoxClick(e: raw.Event) = {     
      if (!p.isCompleted) { 
        val inTxt = getInput("Input", "")
        check(inTxt) match {
          case Left(msg)  => { setHtml("Error", msg);  setVisible("Error", true)  }
          case Right(res) => { p success Right(inTxt); $("#DlgInputTxt").modal("hide") }
        }         
      }
    }  
    
    loadModal(html.DlgInputTxt())

    // initialize values
    setHtml("Header", header)
    setHtml("Body", body)
    setHtml("Close", btnClose)
    setHtml("Start", btnStart)
    setPlaceholder("Input", placeholder)
    setVisible("Error", false)
    setInput("Input", "")


    $("#DlgInputTxt").modal("show")
    $("#DlgInputTxt").on("hide.bs.modal", () => dialogBoxCancel())
    $("[id=DlgInputTxt__Close]").click((e: raw.Event)  => dialogBoxCancel()) 
    $("[id=DlgInputTxt__Start]").click((e: raw.Event)  => dialogBoxClick(e)) 
    f
  }

  @JSExport
  def actionEvent(key: String, elem: raw.HTMLElement, event: raw.Event) = {
    key match {     
      case "Input" => setVisible("Error", false)
      case _       => {}
    }
  }

}