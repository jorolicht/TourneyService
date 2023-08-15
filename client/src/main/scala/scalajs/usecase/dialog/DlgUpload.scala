package scalajs.usecase.dialog

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings
import org.scalajs.dom 

import scala.concurrent._
import scala.util.{Success, Failure}

import shared.utils.UseCaseParam

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


// --------------------------------------------------------------------------
// Dialog Upload Handling
// -------------------------------------------------------------------------- 
object DlgUpload extends BasicHtml
  with TourneySvc
{

  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgUpload", "dlg.upload", "DlgUpload", "dlgupload", scalajs.AppEnv.getMessage _ )

  private def load = if ($(getIdHa("Modal")).length <= 0) {
    $("#APP__DlgLoadOnDemand").append(clientviews.dialog.html.DlgUpload(AppEnv.msgs).toString)
  }

  @JSExport
  // request - upload of tourney information file (markdown format)
  def request: Unit = {
    val form = dom.document.forms.namedItem(getIdHa("Form")).asInstanceOf[dom.raw.HTMLFormElement]
    val formData = new dom.FormData(form)
    formData.append("fname","X12345")
    
    Ajax.post("/upload", formData ).onComplete{ 
      case Success(xhr) => $(getIdHa("Modal")).modal("hide")
      
      case Failure(e)   => $(getIdHa("Modal")).modal("hide")
        
    }
    debug(s"request", s"formData: ${formData}")
  }
  
  @JSExport
  // show upload dialog
  def show(header: String = getMsg("header"), body: String = getMsg("body")): Unit = {  
    load
    $(getIdHa("Header")).html(header)
    if (body == "") {
      $(getIdHa("Body")).hide
    } else {
      $(getIdHa("Body")).html(body)
      $(getIdHa("Body")).show
    }
    $(getIdHa("Modal")).modal("show")
  }


  @JSExport
  // validate upload file
  def validate(input: js.Dynamic): Unit = {  
    val fName = input.files.item(0).name.asInstanceOf[String]
    if (fName.length > 0) {
      $(getIdHa("ButtonSubmit")).removeClass("disabled")
    } else {
      $(getIdHa("ButtonSubmit")).addClass("disabled")
    }
    debug("validate", s"fName: ${fName}")
  }
    
}