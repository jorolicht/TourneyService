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
        offEvents(gE(uc("Modal")), "hide.bs.modal")
        doModal(gE(uc("Modal")), "hide")
      }         
    }
    
    setHtml("Load", html.DlgBox(header, body, buttons, defaultButton, sameSize))

    // register routines for cancel and submit
    onEvents(gE(uc("Modal")), "hide.bs.modal", () => dialogBoxCancel())
    onClick2("[id^=APP__DlgBox__Click_]", (e: Event) => dialogBoxClick(e))
    doModal(gE(uc("Modal")), "show")
    f
  }


 /**
   * standard - load twirl template and show it with message ids
   *            return according to buttons seqeuence 1,2,3, ... 
   */
  def standard(header: String, body: String, buttons: Seq[String], defaultButton: Int=0, sameSize: Boolean=false): Future[Int] = {
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
        offEvents(gE(uc("Modal")), "hide.bs.modal")
        doModal(gE(uc("Modal")), "hide")
      }         
    }
    
    setHtml("Load", html.DlgBoxStd(header, body, buttons, defaultButton, true))
    onEvents(gE(uc("Modal")), "hide.bs.modal", () => dialogBoxCancel())
    onClick2("[id^=APP__DlgBox__Click_]", (e: Event) => dialogBoxClick(e))
    doModal(gE(uc("Modal")), "show")
    f
  }

 /**
   * confirm - shows title and msg 
   *           return true or false
   */
  def confirm(title: String, msg: String): Future[Boolean] = standard(title, msg, Seq("cancel", "ok")).map { _ match {
    case 2 => true
    case _ => false
  }}

  def saveStringAsFile(msgTitel: String, msgBody: String, fName: String, content: String): Unit= {   
    import js.JSConverters._ 
    import org.scalajs.dom.raw.HTMLAnchorElement
    
    val data = new dom.Blob(content.toJSArray.asInstanceOf[scala.scalajs.js.Array[scala.scalajs.js.Any]], dom.raw.BlobPropertyBag("text/plain"))
    var url = dom.raw.URL.createObjectURL(data)

    gE("APP__Download").asInstanceOf[HTMLAnchorElement].href = url
    setAttribute(gE("APP__Download"), "download", fName)

    standard(msgTitel, msgBody, Seq("cancel", "yes"),0,true).map { _ match {
      case 1 => debug("saveStringAsFile", "cancel file save"); false
      case 2 => gE("APP__Download").asInstanceOf[HTMLAnchorElement].click() 
      case _ => debug("saveStringAsFile", "unknown dialog box option"); false
    }}
  }

}
  
