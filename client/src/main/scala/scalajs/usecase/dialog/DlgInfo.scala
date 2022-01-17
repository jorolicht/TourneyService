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
import scalajs.usecase.component.BasicHtml._
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

  private def load = if (getTextContent("Content").length <= 0) setHtml_("APP__DlgInfo__Load", html.DlgInfo().toString)

  def show(title: String, body: String, alertTyp: String="success") : Unit = {
    load
    $("#APP__DlgInfo").modal("show")
    alertTyp match {
      case "danger"  => removeClass("Content", "border-success", "border-secondary"); addClass("Content", "border-danger")
      case "success" => removeClass("Content", "border-danger", "border-secondary"); addClass("Content", "border-success")
      case _         => warn("show", s"alertTyp: ${alertTyp}")
    }
    setHtml("Title", title)
    setHtmlVisible("Body", body!="", body)
  }
}