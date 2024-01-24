package scalajs.usecase.organize

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// tourney service client imports
import shared.model.{ Tourney }
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App }


// ***
// User Case Organization Certificate
// ***    
@JSExportTopLevel("OrganizeCertificate")
object OrganizeCertificate extends UseCase("OrganizeCertificate") 
  with TourneySvc with ViewServices 
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    setMainContent(clientviews.component.html.Certificate(viewPlacements(App.tourney)))
  }


  // buttonPrintPreview - print certificate preview
  @JSExport
  def buttonPrintPreview(coIdStr: String, plIdStr: String, placeStr: String): Unit = {
     printCert(App.tourney, plIdStr, coIdStr.toLongOption.getOrElse(0L) )
  }

}