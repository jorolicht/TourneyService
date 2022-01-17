package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
//import scala.scalajs.js._
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom 
import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

import shared.utils.UseCaseParam

import scalajs.usecase.component._
import scalajs.service._
import clientviews.dialog._


// --------------------------------------------------------------------------
// Dialog Box Handling
// -------------------------------------------------------------------------- 
@JSExportTopLevel("DlgBox")
object DlgBox extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgBox", "dlg.box", "DlgBox", "dlgbox", scalajs.AppEnv.getMessage _ ) 

  /**
   * show - load twirl template and show it 
   */
  def show(header: String, body: String, buttons: Seq[String], defaultButton: Int=0, sameSize: Boolean=false) : Future[Int] = {
    val p = Promise[Int]()
    val f = p.future
  
    def dialogBoxCancel() = if (!p.isCompleted) { p success 0 }
    
    def dialogBoxClick(e: Event) = {
      val DialogBoxId = "APP__DlgBox__Click_(\\d{1})".r
      
      val buNo: Int = $(e.target).attr("id").toOption.getOrElse("APP__DlgBox__Click_0") match {
        case DialogBoxId(bu) => bu.toInt
        case _               => 0
      }
      
      if (!p.isCompleted) { 
        p success buNo 
        $(getIdHa("Modal")).modal("hide")
      }         
    }
    
    setHtml("Load", html.DlgBox(header, body, buttons, defaultButton, sameSize))

    $(getIdHa("Modal")).modal("show")
    $(getIdHa("Modal")).on("hide.bs.modal", () => dialogBoxCancel())
    $("[id^=APP__DlgBox__Click_]").click((e: Event)  => dialogBoxClick(e)) 
    f
  }


 /**
   * show - load twirl template and show it with message ids
   */
  def showStd(header: String, body: String, buttons: Seq[String], defaultButton: Int=0, sameSize: Boolean=false) : Future[Int] = {
    val p = Promise[Int]()
    val f = p.future
  
    def dialogBoxCancel() = if (!p.isCompleted) { 
      p success 0 
    }
    
    def dialogBoxClick(e: Event) = {
      val DialogBoxId = "APP__DlgBox__Click_(\\d{1})".r
      
      val buNo: Int = $(e.target).attr("id").toOption.getOrElse("APP__DlgBox__Click_0") match {
        case DialogBoxId(bu) => bu.toInt
        case _               => 0
      }
      
      if (!p.isCompleted) { 
        p success buNo 
        $(getIdHa("Modal")).modal("hide")
      }         
    }
    
    setHtml("Load", html.DlgBoxStd(header, body, buttons, defaultButton, true))
    $(getIdHa("Modal")).modal("show")
    $(getIdHa("Modal")).on("hide.bs.modal", () => dialogBoxCancel())
    $("[id^=APP__DlgBox__Click_]").click((e: Event)  => dialogBoxClick(e)) 
    f
  }

}
  
