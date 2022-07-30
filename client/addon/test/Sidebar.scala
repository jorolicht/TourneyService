package addon.test

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLElement

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}


//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service imports
import shared.utils._
import shared.utils.Routines._

import shared.model._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object AddonSidebar extends TestUseCase("AddonSidebar") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  def testShowSubMenu(name: String, menu: String) = {
    val tnp = TNP(name, s"menu: ${menu}")
    START(tnp)
    val liElem = document.querySelector(s"[data-sbentry='OrganizeCompetition']").asInstanceOf[HTMLElement]
    val anchorElem = liElem.querySelector(s"[data-toggle='collapse']").asInstanceOf[HTMLElement]

    val ulElem = getElemById_("APP__Sidebar__OrganizeCompetition")

    ulElem.classList.add("show")
    anchorElem.classList.remove("collapsed")
  }

}