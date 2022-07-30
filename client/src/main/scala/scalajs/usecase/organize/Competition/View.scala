package scalajs.usecase.organize

// Start TestCases
// http://localhost:9000/start?ucName=TestMain&ucParam=OrganizeCompetitionView

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
@JSExportTopLevel("OrganizeCompetitionView")
object OrganizeCompetitionView extends UseCase("OrganizeCompetitionView")  
  with TourneySvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("View")
  }

  // setFrame for a competition, coId != 0 and coPhId != 0
  def setFrame(coId: Long, coPhId: Int, reload: Boolean)(implicit trny: Tourney): Unit = {
    debug("setFrame", s"coId: ${coId} coPhId: ${coPhId}")
    if (!exists(s"View_${coId}_${coPhId}") | reload) {
      val elem    = getElemById_(s"ViewContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement]
      val coPhTyp = trny.cophs(coId, coPhId).coPhTyp
      val coPhase = trny.cophs(coId, coPhId)
      coPhTyp match {
        case CPT_GR => setHtml(elem, clientviews.organize.competition.view.html.GroupCard(coId, coPhId, coPhase.groups))
        case CPT_KO => setHtml(elem, clientviews.organize.competition.view.html.KoCard(coId, coPhId, coPhase.ko))
        case CPT_SW => setHtml(elem, "input for switz-system")
        case _      => setHtml(elem, showAlert(getMsg("invalidSection")))
      }
    }
  }

  // set content for competition phase for competition
  def setContent(coId: Long, coPhId: Int)(implicit trny: Tourney) = {
    debug("setContent", s"coId: ${coId} coPhId: ${coPhId}")
    trny.cophs(coId, coPhId).coPhTyp match {
      case CPT_KO => showKoResult(coId, coPhId, trny.cophs(coId, coPhId).ko)      
      case CPT_GR => for (grp <- trny.cophs(coId, coPhId).groups ) showGrResult(coId, coPhId, grp)
      case CPT_SW => error("setContent", s"invalid type coId: ${coId} coPhId: ${coPhId}")
      case _      => error("setContent", s"invalid type coId: ${coId} coPhId: ${coPhId}")
    }
  } 

  // show results of ko round
  def showKoResult(coId: Long, coPhId: Int, koRound: KoRound) = {
    import scalajs.usecase.component.BasicHtml._
    debug("showKoResult", s"APP__GrRound_${coId}_${coPhId}")
    for (index <- 0 to koRound.size-2) {
      val result     = koRound.results(index) 
      val rnd        = result.pos._1
      val game       = result.pos._2
      val playerName = {
        if      (result.sets._1 == koRound.noWinSets) { koRound.getPlayerKoViewName(result.sno._1) } 
        else if (result.sets._2 == koRound.noWinSets) { koRound.getPlayerKoViewName(result.sno._2)} 
        else                                          { "&nbsp;" } 
      }

      // propagate result to next position of player
      if (result.valid) {
        setHtml_(s"APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game}", playerName)

        // print result only if it's not against a dummy player 
        if (result.sno._1 != SNO.BYE & result.sno._2 != SNO.BYE) {
          setHtml_(s"APP__KoRound_${coId}_${coPhId}_SetA_${rnd}_${game}", result.sets._1.toString)
          setHtml_(s"APP__KoRound_${coId}_${coPhId}_SetB_${rnd}_${game}", result.sets._2.toString)
          val balls = result.balls.mkString(",")
          setHtml_(s"APP__KoRound_${coId}_${coPhId}_Balls_${rnd}_${game}", if (balls != "") s"(${balls})" else "&nbsp;" )
        }
      } else {
        val (rnd,game) = koRound.getRndManoFromIndex(index)
        setHtml_(s"APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game}", "&nbsp;")
        setHtml_(s"APP__KoRound_${coId}_${coPhId}_SetA_${rnd}_${game}", "&nbsp;")
        setHtml_(s"APP__KoRound_${coId}_${coPhId}_SetB_${rnd}_${game}", "&nbsp;")
        setHtml_(s"APP__KoRound_${coId}_${coPhId}_Balls_${rnd}_${game}", "&nbsp;")      
      } 
    }  
  }

  // show results of ko round
  def showGrResult(coId: Long, coPhId: Int, group: Group) = {
    import scalajs.usecase.component.BasicHtml._
    debug("showGrResult", s"APP__GrRound_${coId}_${coPhId}")
		for(i <- 0 to group.size-1) {
			for(j <- 0 to group.size-1) {
				if(i!=j){
          val res = if(group.results(i)(j).valid) { group.results(i)(j).sets._1.toString + ":" + group.results(i)(j).sets._2.toString } else { "&nbsp;" }
          setHtml_(s"APP__GrRound_${coId}_${coPhId}_Set_${group.grId}_${i+1}_${j+1}", res)
          //debug("showGrResult",s"APP__GrRound_${coId}_${coPhId}_Set_${group.grId}_${i+1}_${j+1} -> ${res}")
        }
      }
      val balls  = group.balls(i)._1.toString + ":" + group.balls(i)._2.toString  
      val sets   = group.sets(i)._1.toString + ":" + group.sets(i)._2.toString  
      val points = group.points(i)._1.toString + ":" + group.points(i)._2.toString
      setHtml_(s"APP__GrRound_${coId}_${coPhId}_Balls_${group.grId}_${i}", balls)
      setHtml_(s"APP__GrRound_${coId}_${coPhId}_Sets_${group.grId}_${i}", sets)
      setHtml_(s"APP__GrRound_${coId}_${coPhId}_Points_${group.grId}_${i}", points)
      setHtml_(s"APP__GrRound_${coId}_${coPhId}_Places_${group.grId}_${i}", group.pants(i).place._1.toString)
    } 
  } 

  
}




