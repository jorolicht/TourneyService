package scalajs.usecase.organize

import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.HTMLElement

import shared.model._
import shared.model.CompPhase._
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._

import scalajs._

// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionReferee")
object OrganizeCompetitionReferee extends UseCase("OrganizeCompetitionReferee")  
  with TourneySvc
{

  def render(param: String = "", ucInfo: String = "", update: Boolean=false) = {
    OrganizeCompetitionTab.render("Referee")
  }

  // set referee page for a competition phase(round), coId != 0 and coPhId != 0
  def setPage(coph: CompPhase): Unit = {
    debug("setPage", s" Referee: coId: ${coph.coId} coPhId: ${coph.coPhId}")
    val base = gE(s"Referee_${coph.coId}_${coph.coPhId}")

    if (base == null) {
      // init view
      // get section, one for every coPhId
      val elem = gEqS(s"RefereeContent_${coph.coId}", s"[data-coPhId='${coph.coPhId}']")

      // generate main page with referee notes
      val noSchiris = coph.matches.length
      setHtml(elem, clientviews.organize.competition.referee.html.RefereeCard(coph, noSchiris))
      debug("setPage", s"Test msgCode: ${gMTyp(coph.matches(1).coTyp)} noSchiris: ${noSchiris}")

      coph.matches.zipWithIndex.foreach { case (m, idx) => {
        val (snoA, nameA, clubA, ttrA) = SNO(m.stNoA).getInfo(m.coTyp)(App.tourney)
        val (snoB, nameB, clubB, ttrB) = SNO(m.stNoB).getInfo(m.coTyp)(App.tourney)
        coph.coPhTyp match {
          case CompPhaseTyp.GR => {
            setHtml(gE(s"RefereeNote_${coph.coId}_${coph.coPhId}_${idx+1}"), 
                    clientviews.organize.competition.referee.html.RefereeCardSingle(coph, m.gameNo,
                    App.tourney.name, coph.name, gMTyp(m.coTyp), 
                    getMsg("group", m.asInstanceOf[MEntryGr].grId.toString),
                    getMsg("round", m.asInstanceOf[MEntryGr].round.toString),
                    nameA, nameB, clubA, clubB))
          }
          case CompPhaseTyp.KO => {
            setHtml(gE(s"RefereeNote_${coph.coId}_${coph.coPhId}_${idx+1}"), 
                    clientviews.organize.competition.referee.html.RefereeCardSingle(coph, m.gameNo,
                    App.tourney.name, coph.name, gMTyp(m.coTyp), 
                    "Gruppe A", "Runde 5",
                    nameA, nameB, clubA, clubB))
          }
          case _               =>
        }
        val qrCodeParam = new QRCodeParam { val width = 80; val height = 80 }
        val qrCode = new QRCode(gE(s"QRCode_${coph.coId}_${coph.coPhId}_${idx+1}"), qrCodeParam) 

        val nonce = RefereeNote.nonce(App.tourney.id, coph.coId, coph.coPhId, m.gameNo)
        val refereeAddr = s"${AppEnv.serverAddress}/svc/getReferee/${App.tourney.id}/${coph.coId}/${coph.coPhId}/${m.gameNo}/${nonce}"
        qrCode.makeCode(refereeAddr)
        gE(s"QRCodeLink_${coph.coId}_${coph.coPhId}_${idx+1}").asInstanceOf[dom.raw.HTMLAnchorElement].href = refereeAddr 
      }}
    } else { 
      error("setPage", s" Referee (not null)") 
    }
  }

}

 
trait QRCodeParam extends js.Object {
  val width: Int
  val height: Int
}

@js.native
@JSGlobal
class QRCode(elem: HTMLElement, param: QRCodeParam) extends js.Object {
  def makeCode(url: String): js.Any = js.native
}
