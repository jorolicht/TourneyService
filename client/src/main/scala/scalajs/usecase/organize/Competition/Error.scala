package scalajs.usecase.organize

import scala.concurrent._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement

import shared.model.CompPhase

import scalajs.usecase.component._
import scalajs.service._

import scalajs._

// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionError")
object OrganizeCompetitionError extends UseCase("OrganizeCompetitionError")  
  with TourneySvc
{

  def render(param: String = "", ucInfo: String = "", update: Boolean=false) = {
    OrganizeCompetitionTab.render("Error")
  }

  // set referee page for a competition phase(round), coId != 0 and coPhId != 0
  def setPage(coph: CompPhase): Unit = {
    debug("setPage", s" Error: coId: ${coph.coId} coPhId: ${coph.coPhId}")
    val msg = if (coph.name == "") gM("roundNotConfigured") else gM("roundXYNotConfigured", coph.name)
    setHtml(gE(s"ErrorContent_${coph.coId}_${coph.coPhId}"), msg )
  }

}