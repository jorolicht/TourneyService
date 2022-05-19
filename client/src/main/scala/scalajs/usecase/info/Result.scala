package scalajs.usecase.info

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

import upickle.default._

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

// tourney service client imports
import shared.model._
import shared.model.CompPhase._
import shared.utils._
import shared.model.tabletennis._
import shared.utils.Constants._ 

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.App

// ***
// User Info View Competitions
// ***    
@JSExportTopLevel("InfoResult")
object InfoResult extends UseCase("InfoResult")
  with TourneySvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    showTourneyResults(App.tourney)
  }

  
  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    val coId = upd.coId
    val coPh = upd.coPh
    val grId = upd.grId

    debug("update",s"for ${coId} ${coPh} ${grId}")
    val cphase = App.tourney.cophs((coId,coPh))
    cphase.coPhTyp match {
      case CPT_KO => showKoResult(cphase.coId, cphase.coPh, cphase.ko) 
      case CPT_GR => showGrResult(cphase.coId, cphase.coPh, cphase.groups(grId-1))
      case _      => debug("update",s"error unknown competition type")
    }
  }

  // show results of ko round
  def showGrResult(coId: Long, coPh: Int, group: Group) = {
    debug("showGrResult", s"APP__GrRound_${coId}_${coPh}")
		for(i <- 0 to group.size-1) {
			for(j <- 0 to group.size-1) {
				if(i!=j){
          val res = if(group.results(i)(j).valid) { group.results(i)(j).sets._1.toString + ":" + group.results(i)(j).sets._2.toString } else { "&nbsp;" }
          setHtml_(s"APP__GrRound_${coId}_${coPh}_Set_${group.grId}_${i+1}_${j+1}", res)
          //debug("showGrResult",s"APP__GrRound_${coId}_${coPh}_Set_${group.grId}_${i+1}_${j+1} -> ${res}")
        }
      }
      val balls  = group.balls(i)._1.toString + ":" + group.balls(i)._2.toString  
      val sets   = group.sets(i)._1.toString + ":" + group.sets(i)._2.toString  
      val points = group.points(i)._1.toString + ":" + group.points(i)._2.toString
      setHtml_(s"APP__GrRound_${coId}_${coPh}_Balls_${group.grId}_${i}", balls)
      setHtml_(s"APP__GrRound_${coId}_${coPh}_Sets_${group.grId}_${i}", sets)
      setHtml_(s"APP__GrRound_${coId}_${coPh}_Points_${group.grId}_${i}", points)
      setHtml_(s"APP__GrRound_${coId}_${coPh}_Places_${group.grId}_${i}", group.players(i).place._1.toString)
    } 
  } 

  // show results of ko round
  def showKoResult(coId: Long, coPh: Int, koRound: KoRound) = {
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
        //debug("showKoResult",s"valid ${index}: APP__KoRound_${coId}_${coPh}_Winner_${rnd}_${game} -> ${playerName}")

        setHtml_(s"APP__KoRound_${coId}_${coPh}_Winner_${rnd}_${game}", playerName)

        // print result only if it's not against a dummy player 
        if (result.sno._1 != SNO.BYE & result.sno._2 != SNO.BYE) {
          setHtml_(s"APP__KoRound_${coId}_${coPh}_SetA_${rnd}_${game}", result.sets._1.toString)
          setHtml_(s"APP__KoRound_${coId}_${coPh}_SetB_${rnd}_${game}", result.sets._2.toString)
          val balls = result.balls.mkString(",")
          setHtml_(s"APP__KoRound_${coId}_${coPh}_Balls_${rnd}_${game}", if (balls != "") s"(${balls})" else "&nbsp;" )
        }
      } else {
        val (rnd,game) = koRound.getRndManoFromIndex(index)
        //debug("showKoResult",s"invalid ${index}: APP__KoRound_${coId}_${coPh}_Winner_${rnd}_${game}")

        setHtml_(s"APP__KoRound_${coId}_${coPh}_Winner_${rnd}_${game}", "&nbsp;")
        setHtml_(s"APP__KoRound_${coId}_${coPh}_SetA_${rnd}_${game}", "&nbsp;")
        setHtml_(s"APP__KoRound_${coId}_${coPh}_SetB_${rnd}_${game}", "&nbsp;")
        setHtml_(s"APP__KoRound_${coId}_${coPh}_Balls_${rnd}_${game}", "&nbsp;")      
      } 
    }  
  }

  // show results of tourney with all competitions
  def showTourneyResults(tourney: Tourney) = {
    // list of all configured competition phases
    debug("showTourneyResults", s"start")
    val coPhMapSeq = tourney.cophs.values.groupBy(x => x.coId).transform((key, value) => value.toList.sortBy(_.coPh).toSeq)
    val co2NaSt = (for { (key, comp) <- tourney.comps } yield {
      key -> (comp.name, getMsg_("competition.status." + comp.status))
    }).toMap


    val coPhMapListSorted = Map(coPhMapSeq.toSeq.sortBy(_._1):_*)
    setMainContent(clientviews.info.html.ResultTmpl(co2NaSt, coPhMapListSorted).toString)
    debug("showTourneyResults", s"phases")
    for (coPhList <- coPhMapSeq.values; coPhase  <- coPhList) {
      // show results of every phase of every competition
      if (coPhase.coPhTyp == CPT_KO) showKoResult(coPhase.coId, coPhase.coPh, coPhase.ko)
      if (coPhase.coPhTyp == CPT_GR) for (grp <- coPhase.groups ) showGrResult(coPhase.coId, coPhase.coPh, grp)
    }
  } 
}
