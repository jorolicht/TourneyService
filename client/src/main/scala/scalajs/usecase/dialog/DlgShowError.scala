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

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.utils._
import shared.utils.Constants._
import clientviews.dialog.html

object DlgShowError extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgShowError", "dlg.ShowError", "DlgShowError", "dlgshowerror", scalajs.AppEnv.getMessage _ )

  // set dialog input fields
  def set(eList: List[Error]): Unit = {
    setHtml("Content", "")
    eList.foreach {
      err => insertHtml(gE(uc("Content")), "beforeend", s"""<p class="text-danger mb-0">${getError(err)}</p>""")
    }
  }
  
  // show dialog return result (Future) or Failure when canceled
  def show(eList: List[Error]): Future[Boolean] = {
    val p     = Promise[Boolean]()
    val f     = p.future

    def cancel() = {
      offEvents(gE(uc("Modal")), "hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event):Unit = {
      if (!p.isCompleted) p success true
      offEvents(gE(uc("Modal")), "hide.bs.modal")
      doModal(gE(uc("Modal")), "hide")
    }
    
    loadModal(html.DlgShowError(), ucp)
    setHtml("Header", getMsg("hdr"))
    set(eList)

    // register routines for cancel and submit
    onEvents(gE(uc("Modal")), "hide.bs.modal", () => cancel())
    onClick(gE(uc("Submit")), (e: Event) => submit(e))
    doModal(gE(uc("Modal")), "show")

    f.map(_ => true)
     .recover { case e: Exception =>  false }
  }  
}