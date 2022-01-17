package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.concurrent.duration._
import scala.util.Try

import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.highlight._                   // highlight.org
import org.yamlijs.YAML                  // facade for yaml

import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

import org.scalajs.dom.raw.{ Event, HTMLElement, HTMLLIElement, HTMLInputElement, History } // for ScalaJs bindings
import org.scalajs.dom.PopStateEvent

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

// tourney service imports
import shared.utils.UseCaseParam

import scalajs.usecase.component._
import scalajs.service._
import scalajs.App


// --------------------------------------------------------------------------
// REGISTER CLUB Dialog Handling
// -------------------------------------------------------------------------- 
object DlgSpinner extends BasicHtml
{

  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgSpinner", "dlg.spinner", "DlgSpinner", "dlgspinner", scalajs.AppEnv.getMessage _ ) 

  private def load = if ($(getIdHa("Modal")).length <= 0) {
    $("#APP__DlgLoadOnDemand").append(clientviews.dialog.html.DlgSpinner().toString)
  }

  /** Handles spinner dialog
   *
   *  @param  cmd   0 - show dialog with message
   *                1 - success
   *                2 - error
   *                3 - close dialog    
   *  @param  msg   handle to db
   *  @return Unit change spinner html element
   */  
  def show(cmd: Int, msg: String) = {
    load
    cmd match {
      case 0 => 
        $(getIdHa("Type")).removeClass("border-danger border-success border-5")
        $(getIdHa("Type")).addClass("border-secondary")
        $(getIdHa("Content")).html(msg)
        $(getIdHa("Modal")).modal("show")
      case 1 => 
        $(getIdHa("Content")).html(msg)
        $(getIdHa("Type")).addClass("border-success border-5")
        $(getIdHa("Image")).html("""<img class="img-fluid" src="assets/img/highfive150.gif" alt="Finished ...">""")
      case 2 => 
        $(getIdHa("Content")).html(msg)
        $(getIdHa("Type")).addClass("border-danger border-5")
        $(getIdHa("Image")).html("""<img class="img-fluid" src="assets/img/pullhair150.gif" alt="Error ...">""")
      case _ =>  
        $(getIdHa("Modal")).modal("hide")         
    }
  }

}