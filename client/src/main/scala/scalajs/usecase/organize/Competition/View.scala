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

  def render(param: String = "", ucInfo: String = "", update: Boolean=false) = {
    OrganizeCompetitionTab.render("View")
  }

  // set view page for a competition phase(round), coId != 0 and coPhId != 0
  def setPage(coph: CompPhase): Unit = {
    val contentElement = gE(s"ViewContent_${coph.coId}_${coph.coPhId}")
    if (contentElement.innerHTML == "") {
      debug("setPage", s"View init: coId: ${coph.coId} coPhId: ${coph.coPhId}")
      coph.getTyp match {
        case CompPhaseTyp.GR => setHtml(contentElement, clientviews.organize.competition.view.html.GroupCard(coph.coId, coph.coPhId, coph.groups))
        case CompPhaseTyp.RR => setHtml(contentElement, clientviews.organize.competition.view.html.RRCard(coph.coId, coph.coPhId, coph.groups(0)))
        case CompPhaseTyp.KO => setHtml(contentElement, clientviews.organize.competition.view.html.KoCard(coph.coId, coph.coPhId, coph.ko))
        case CompPhaseTyp.SW => ???
        case _               => error("setPage", s"View init invalid typ, coId: ${coph.coId} coPhId: ${coph.coPhId}")
      }
    }  

    debug("setPage", s"View: coId: ${coph.coId} coPhId: ${coph.coPhId}")
    // update view page
    coph.getTyp match {
      case CompPhaseTyp.KO => showKoResult(coph.coId, coph.coPhId, coph.ko)      
      case CompPhaseTyp.GR => for (grp <- coph.groups ) showGrResult(coph.coId, coph.coPhId, grp)
      case CompPhaseTyp.RR => showRrResult(coph.coId, coph.coPhId, coph.groups(0))
      case CompPhaseTyp.SW => ???
      case _               => error("setPage", s"View update invalid typ, coId: ${coph.coId} coPhId: ${coph.coPhId}")
    }
  }


  // show results of ko round
  def showKoResult(coId: Long, coPhId: Int, koRound: KoRound) = {
    debug("showKoResult", s"coId: ${coId} coPhId: ${coPhId}")
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
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game}"), playerName)

        // print result only if it's not against a dummy player 
        if (result.sno._1 != SNO.BYE & result.sno._2 != SNO.BYE) {
          setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetA_${rnd}_${game}"), result.sets._1.toString)
          setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetB_${rnd}_${game}"), result.sets._2.toString)
          val balls = result.balls.mkString(",")
          setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Balls_${rnd}_${game}"), if (balls != "") s"(${balls})" else "&nbsp;" )
        }
      } else {
        val (rnd,game) = koRound.getRndManoFromIndex(index)
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game}"), "&nbsp;")
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetA_${rnd}_${game}"), "&nbsp;")
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetB_${rnd}_${game}"), "&nbsp;")
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Balls_${rnd}_${game}"), "&nbsp;")      
      } 
    }  
  }

  // show results of ko round
  def showGrResult(coId: Long, coPhId: Int, group: Group) = {
    //debug("showGrResult", s"APP__GrRound_${coId}_${coPhId} for Group: ${group.grId}")
		for(i <- 0 until group.size) {
			for(j <- 0 until group.size) {
				if(i!=j){
          val res = if(group.results(i)(j).valid) { group.results(i)(j).sets._1.toString + ":" + group.results(i)(j).sets._2.toString } else { "&nbsp;" }
          setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Set_${group.grId}_${i+1}_${j+1}"), res)
          //debug("showGrResult",s"APP__GrRound_${coId}_${coPhId}_Set_${group.grId}_${i+1}_${j+1} -> ${res}")
        }
      }

      val balls  = group.balls(i)._1.toString + ":" + group.balls(i)._2.toString  
      val sets   = group.sets(i)._1.toString + ":" + group.sets(i)._2.toString  
      val points = group.points(i)._1.toString + ":" + group.points(i)._2.toString
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Balls_${group.grId}_${i}"), balls)
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Sets_${group.grId}_${i}"), sets)
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Points_${group.grId}_${i}"), points)
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Places_${group.grId}_${i}"), group.pants(i).place._1.toString)
    } 
  } 

  // showRrResult - show round robin result
  //                for size <= 12 it's like a group result
  //                for size >  12 it's a kind of (sorted) list view
  def showRrResult(coId: Long, coPhId: Int, group: Group) = {
    if (group.size <= 12) showGrResult(coId, coPhId, group) else {
      (for(i<-0 until group.size) yield {
        (group.pants(i).place._1, group.pants(i).name, group.pants(i).club, 
         group.balls(i)._1.toString + ":" + group.balls(i)._2.toString, 
         group.sets(i)._1.toString + ":" + group.sets(i)._2.toString,
         group.points(i)._1.toString + ":" + group.points(i)._2.toString)
      }).sortBy(_._1).zipWithIndex.map { e => {
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Places"), e._1._1.toString)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Name"),   e._1._2)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Club"),   e._1._3)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Balls"),  e._1._4)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Sets"),   e._1._5)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Points"), e._1._6)
      }}
    }
  }  


}
