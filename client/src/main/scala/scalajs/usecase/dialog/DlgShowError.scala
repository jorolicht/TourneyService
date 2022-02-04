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

object DlgShowError extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgShowError", "dlg.ShowError", "DlgShowError", "dlgshowerror", scalajs.AppEnv.getMessage _ )
  private def load() = if (!checkId("Modal")) insertHtml_("APP__Load", "afterbegin", html.DlgShowError().toString)


  // set dialog input fields
  def set(eList: List[Error]): Unit = {
    setHtml("Content", "")
    eList.foreach {
      err => insertHtml("Content", "beforeend", s"""<p class="text-danger mb-0">${getError(err)}</p>""")
    }
  }
  
  // show dialog return result (Future) or Failure when canceled
  def show(eList: List[Error]): Future[Boolean] = {
    val p     = Promise[Boolean]()
    val f     = p.future

    def cancel() = {
      $(getId("Modal","#")).off("hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event):Unit = {
      if (!p.isCompleted) p success true
      $(getId("Modal","#")).off("hide.bs.modal")
      $(getId("Modal","#")).modal("hide")
    }
    
    load()
    setHtml("Header", getMsg("hdr"))
    set(eList)

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event)     => submit(e)) 
    $(getId("Modal","#")).modal("show")

    f.map(_ => true)
     .recover { case e: Exception =>  false }
  }  
}