package scalajs.usecase.organize

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.{ Event, HTMLInputElement, HTMLFormElement } // for ScalaJs bindings

import scalajs.usecase.component._
import scalajs.service._
import shared.utils._

@JSExportTopLevel("OrganizeReport")
object OrganizeReport extends UseCase("OrganizeReport") 
  with TourneySvc
{
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = setMainContent("REPORTS") 

}