package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.yamlijs.YAML                  // facade for yaml
import org.highlight._                   // highlight.org
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import shared.utils._
import shared.model._


import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs._

@JSExportTopLevel("HomeDemo")
object HomeDemo extends UseCase("HomeDemo")
  with TourneySvc
{

  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = {
    App.loadRemoteTourney(1).map { 
      case Left(err)   => error("render", s"could not set demo tourney")
      case Right(res)  => { setHeadline(); App.execUseCase("InfoSchedule", "") }
    }
  }


}  