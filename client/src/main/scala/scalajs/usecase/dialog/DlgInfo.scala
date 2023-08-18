package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
//import scala.scalajs.js._
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._

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
@JSExportTopLevel("DlgInfo") 
object DlgInfo extends BasicHtml   
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgInfo", "dlg.info", "DlgInfo", "dlginfo", scalajs.AppEnv.getMessage _ )

  def show(title: String, body: String, alertTyp: String="success") : Unit = {
    loadModal(html.DlgInfo(), ucp)

    $("#APP__DlgInfo__Modal").modal("show")
    alertTyp match {
      case "danger"  => removeClass(gE(uc("Content")), "border-success", "border-secondary"); addClass(gE(uc("Content")), "border-danger")
      case "success" => removeClass(gE(uc("Content")), "border-danger", "border-secondary"); addClass(gE(uc("Content")), "border-success")
      case _         => warn("show", s"alertTyp: ${alertTyp}")
    }
    setHtml("Title", title)
    setHtmlVisible("Body", body!="", body)
  }
}