package scalajs.usecase.admin

import scala.util.Try

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service client imports
import shared.utils._
import shared.model._

import scalajs.usecase.component._
import scalajs.service._
import scalajs._


@js.native
@JSGlobal
object JSON extends js.Object {
  def parse(text: String): js.Any = js.native
  def stringify(val1: js.Any, val2: js.Any, val3: js.Any): String = js.native
}


@JSExportTopLevel("AdminDatabase")
object AdminDatabase extends UseCase("AdminDatabase") 
  with TourneySvc  
{

  //***
  // DATABASE MANAGMENT
  //***
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false)  = {
    setMainContent(clientviews.admin.html.DatabaseTmpl(AppEnv.msgs).toString)
  }
    

  @JSExport
  def export() = {
    val toId     = getInput(gE(uc("FormTourneyId")),0L)
    val clubName = getInput(gE(uc("FormClub")))
    val toDate   = getInput(gE(uc("FormDate")),0)

    debug("export", s"${toId} ${clubName} ${toDate}")

    expDatabase(toId, clubName, toDate, "Admin").map { 
      case Left(err)  => setHtml("Content", "Error(${err.code}) ${err.msg})")
      case Right(res) => {
        debug("export", s"result: ${res.take(20)}")
        setHtml("Content", JSON.stringify(JSON.parse(res), null,2))
      }
    }
  }



}
 