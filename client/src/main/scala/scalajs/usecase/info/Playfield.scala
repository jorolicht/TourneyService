package scalajs.usecase.info

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

import shared.utils._

import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


// ***
// User Info View Competitions
// ***    
@JSExportTopLevel("InfoPlayfield")
object InfoPlayfield extends UseCase("InfoPlayfield")
  with TourneySvc
{
  
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    //debug("render", s"run.id: ${App.tourney.run.id} id: ${App.tourney.id}")
    setMainContent(clientviews.info.html.PlayfieldTmpl(App.tourney.playfields.values.toSeq, AppEnv.msgs).toString)
  }


  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    setMainContent(clientviews.info.html.PlayfieldTmpl(App.tourney.playfields.values.toSeq, AppEnv.msgs).toString)
  }

}

  