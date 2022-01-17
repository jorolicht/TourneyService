package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils
import scala.scalajs._

import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import shared.utils._
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


object HomeMockup extends UseCase("HomeMockup")
  with TourneySvc 
{

  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = {
    val (ucEffName,ucEffParam)  = read[(String,String)](ucParam) 
    setMainContent(clientviews.home.html.Mockup(ucEffName, ucEffParam, ucInfo).toString)
  }

}  