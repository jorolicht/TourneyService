package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
//import scala.scalajs.js._
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom 
import org.scalajs.dom.raw.{ Event, UIEvent, HTMLInputElement, HTMLButtonElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

import shared.utils.UseCaseParam

import scalajs.usecase.component._
import scalajs.service._
import clientviews.dialog._


// --------------------------------------------------------------------------
// Dialog Box File Input
// -------------------------------------------------------------------------- 
// TEST: http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20dialog%20%2Dn%202%20%2D%2Dparam%20gen 
@JSExportTopLevel("DlgBoxFile")
object DlgBoxFile extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>

  /**
   * show - load twirl template and show it 
   */
  def show(header: String, body: String, sameSize: Boolean=true) : Future[Either[shared.utils.Error, String]] = {
    val p = Promise[Either[shared.utils.Error, String]]()
    val f = p.future
    var content = ""
  
    def dialogBoxCancel() = if (!p.isCompleted) { p success Left(shared.utils.Error("xxxx")) }

    setHtml(gE("DlgBoxFile_Load"), html.DlgBoxFile(header, body, sameSize)(gM _))

    gE("DlgBoxFile_BtnCancel").asInstanceOf[HTMLButtonElement].onclick = e => {
      if (!p.isCompleted) { 
        p success Left(shared.utils.Error("xxxx")) 
        doModal(gE("DlgBoxFile_Modal"), "hide")
      }
    }
    
    gE("DlgBoxFile_BtnUpload").asInstanceOf[HTMLButtonElement].onclick = e => {
      if (!p.isCompleted) { 
        if (content == "") p success Left(shared.utils.Error("xxxxcontent")) else p success Right(content)
        offEvents(gE("DlgBoxFile_Modal"), "hide.bs.modal")
        doModal(gE("DlgBoxFile_Modal"), "hide")
      }
    }    

    val fileInput = gE("DlgBoxFile_Input").asInstanceOf[dom.html.Input]
    fileInput.onchange = e => {
      val reader = new dom.FileReader()
      reader.readAsText(fileInput.files(0))
      reader.onload = (e:UIEvent) => {
        content = reader.result.asInstanceOf[String]
      }
    }

    // register routines for cancel and submit
    onEvents(gE("DlgBoxFile_Modal"), "hide.bs.modal", () => dialogBoxCancel())
    doModal(gE("DlgBoxFile_Modal"), "show")
    f
  }


}
  
