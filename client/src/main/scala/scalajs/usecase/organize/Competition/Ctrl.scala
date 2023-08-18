package scalajs.usecase.organize

// Start TestCases in Javascript Console
// test -s compphase -n 1

import scala.collection.mutable.{ ArrayBuffer }
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import shared.model._
import shared.model.CompPhase._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils.Constants._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionCtrl")
object OrganizeCompetitionCtrl extends UseCase("OrganizeCompetitionCtrl")  
  with TourneySvc
{
  import org.scalajs.dom.raw.Element
  import org.scalajs.dom.raw.HTMLElement
  import org.scalajs.dom.raw.HTMLTableElement
  import scala.collection.mutable.ListBuffer

  case class PantSelect(sno: SNO, name: String, info: String, var checked: Boolean)
  var pants = new Array[PantSelect](0)

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Ctrl")
  }

  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    import org.scalajs.dom.html.Input
    import scalajs.usecase.dialog.DlgShowError

    val (coId, coPhId) = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0))
    val action = if (!App.tourney.cophs.contains((coId, coPhId))) "BadRequest" else key 

    action match {

      case "StartRound" => {
        App.tourney.comps(coId).status = CompStatus.RUN
        App.tourney.cophs((coId, coPhId)).setStatus(CompPhaseStatus.EIN)
        App.execUseCase("OrganizeCompetitionInput", "", "")
      } // StartRound

      case "DeleteRound" => {
        val followingCoPhs = App.tourney.getCompPhaseFollowing(coId, coPhId)
        val msg = if (followingCoPhs.length > 0) {
          getMsg("delete.more", App.tourney.getCompPhaseName(coId, coPhId), App.tourney.getCompPhaseNames(coId, followingCoPhs).mkString(", ")) 
        } else {
          getMsg("delete.one", App.tourney.getCompPhaseName(coId, coPhId))
        }  

        DlgBox.standard(getMsg("delete.confirm"), msg, Seq("cancel", "yes"),0,true).map { res => res match {
          case 2 => {
            App.tourney.delCompPhase(coId, coPhId)
            App.tourney.delCompPhases(coId, followingCoPhs)
            App.execUseCase("OrganizeCompetition", "", "")
            debug("DeleteRound", s"CONFIRMED: coId: ${coId} coPhId: ${coPhId}")
          }  
          case _ => debug("DeleteRound", s"CANCEL: coId: ${coId} coPhId: ${coPhId}")
        }}
      }

      case "ResetRound" => {
        val followingCoPhs = App.tourney.getCompPhaseFollowing(coId, coPhId)
        val msg = if (followingCoPhs.length > 0) {
          getMsg("reset.more", App.tourney.getCompPhaseName(coId, coPhId), App.tourney.getCompPhaseNames(coId, followingCoPhs).mkString(", ")) 
        } else {
          getMsg("reset.one", App.tourney.getCompPhaseName(coId, coPhId))
        }  

        DlgBox.standard(getMsg("reset.confirm"), msg, Seq("cancel", "yes"),0,true).map { res => res match {
          case 2 => {
            App.tourney.getCompPhaseName(coId, coPhId)
            App.tourney.getCompPhaseNames(coId, followingCoPhs)
            App.execUseCase("OrganizeCompetitionDraw", "", "")
            debug("ResetRound", s"CONFIRMED: coId: ${coId} coPhId: ${coPhId}")
          }  
          case _ => debug("ResetRound", s"CANCEL: coId: ${coId} coPhId: ${coPhId}")
        }}
      } // ResetRound

      case "StartFollowing" => {

        debug("StartFollowing", s"coId: ${coId} coPhId: ${coPhId}")
      } // StartFollowing

      case "DemoBtn"          => App.tourney.cophs((coId,coPhId)).demo = getCheckbox(gE(uc("DemoBtn")))

      case "BadRequest" => error("Invalid Action", s"value: ${key} coId: ${coId} coPhId: ${coPhId}")

      case _            => error("Invalid Key", s"value: ${key} coId: ${coId} coPhId: ${coPhId}")

    }
  }


  // set control page for competition phase
  def setPage(coph: CompPhase): Unit = {
    val coId    = coph.coId
    val coPhId  = coph.coPhId
    val coPhCfg = coph.coPhCfg 

    def initContent(coId: Long, coPhId: Int, elem: Element, value: String) = {
      if (!exists(s"Ctrl_${coId}_${coPhId}")) { elem.innerHTML = value }
    }

    val ctrlBase = gE(s"CtrlContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']")
    initContent(coId, coPhId, ctrlBase, clientviews.organize.competition.ctrl.html.CtrlCard(coId, coPhId).toString)
        
    // set status
    setHtml(s"Status_${coph.coId}_${coph.coPhId}", getMsg(s"competition.phase.status${coph.status}"))
    setHtml(s"Config_${coph.coId}_${coph.coPhId}", coph.getDescription(getMsg_)) 
    setHtml(s"Finished_${coph.coId}_${coph.coPhId}", getMsg("finished", s"${coph.mFinished}", s"${coph.mTotal}")) 

    // set action buttons
    setActionBtn(coId, coPhId, "StartRound", coph.status == CompPhaseStatus.AUS)
    setActionBtn(coId, coPhId, "ResetRound", !(coph.status == CompPhaseStatus.AUS))
    setActionBtn(coId, coPhId, "DeleteRound", true)
    setCheckbox("DemoBtn", coph.demo)

    coPhCfg match {
      case CPC_GRPS3 | CPC_GRPS34 | CPC_GRPS4 | CPC_GRPS45| CPC_GRPS5 | CPC_GRPS56 | CPC_GRPS6 => {
        setActionBtn(coId, coPhId, "StartFollowing", !(coph.status == CompPhaseStatus.AUS))
      }

      case CPC_KO => {   }
      case CPC_SW => {   }
      case _      => {   }
    }
  }

  def setActionBtn(coId: Long, coPhId: Int, action: String, value: Boolean) = {
    setDisabled(s"Btn${action}", !value)
    setClass(s"Msg${action}", !value, "text-muted")
  }

  def checkPantEntry(sno: String, coId: Long, checked: Boolean): Unit = {
    val status = App.tourney.pl2co((sno, coId)).status
    if (status == PantStatus.REDY || status == PantStatus.REGI) {
      App.tourney.pl2co((sno, coId)).status = if (checked) PantStatus.REDY else PantStatus.REGI 
    }
  }


}