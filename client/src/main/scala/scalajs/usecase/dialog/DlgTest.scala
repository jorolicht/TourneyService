package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom 

import shared.utils.UseCaseParam
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


// --------------------------------------------------------------------------
// Dialog Box Info Handling
// -------------------------------------------------------------------------- 
@JSExportTopLevel("DlgTest")
object DlgTest extends BasicHtml
{
 
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgTest", "dlg.test", "DlgTest", "dlgtest", scalajs.AppEnv.getMessage _ )


  private def load = if ($(getIdHa("Modal")).length <= 0) {
    $("#APP__DlgLoadOnDemand").append(clientviews.dialog.html.DlgTest(AppEnv.msgs).toString)
  } else {
    info("load", "already loaded")
  }

  def info(msg: String, alertTyp: String) : Unit = {
    alertTyp match {
      case "danger"  => $(getIdHa("Type")).removeClass("border-success border-secondary")
                        $(getIdHa("Type")).addClass("border-danger")
      case "success" => $(getIdHa("Type")).removeClass("border-danger border-secondary")
                        $(getIdHa("Type")).addClass("border-success")
      case _         => $(getIdHa("Type")).removeClass("border-danger border-success")
                        $(getIdHa("Type")).addClass("border-secondary")
    }
    load
    $(getIdHa("Content")).html(msg)
    $(getIdHa("Modal")).modal("show")
  }

}
  
