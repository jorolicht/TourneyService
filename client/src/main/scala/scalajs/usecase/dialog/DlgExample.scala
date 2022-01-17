package scalajs.usecase.dialog

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Success, Failure }
import scala.util.matching
import scala.concurrent._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.utils._
import shared.utils.Constants._
import clientviews.dialog.html


// Replace Example/Example with appropriate class name
case class Example(name: String, age: Int)

@JSExportTopLevel("DlgExample")     
object DlgExample  extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgExample", "dlg.Example", "DlgExample", "dlgexample", scalajs.AppEnv.getMessage _ )
  
  private def load() = if (getTextContent("Load").length <= 0) setHtml("Load", html.DlgExample(getMsg("hdr")))

  // set dialog input fields
  def set(tB: Example): Unit = {
    
  }

  // validate dialog for Example, return valid Example
  def validate(): Either[Error, Example] = {
    debug("verify", "dummy: OK")
    Right(Example("test", 1984))
  }

  
  // show dialog return result (Future) or Failure when canceled
  def show(header: String, tournBase: Example): Future[Either[Error, Example]] = {

    val p = Promise[Example]()
    val f = p.future

    def cancel() = if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }

    def submit(e: Event) {
      validate() match {
        case Left(err) => setHtmlVisible("Help", true, getError(err))
        case Right(tb) => {
          p success tb
          $(getId("Modal","#")).modal("hide")
        }  
      }
    }
    
    load()
    set(tournBase)

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event) => submit(e)) 
    $(getId("Modal","#")).modal("show")
    
    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}

