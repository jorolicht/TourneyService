package scalajs.usecase.info

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

// tourney service client imports
import shared.model.{ Tourney, TournBase, Player, PantStatus, CompStatus, CompTyp, SNO }
import shared.model.Competition._
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App }


// ***
// User Info View Players
// ***   
@JSExportTopLevel("InfoPlayer")
object InfoPlayer extends UseCase("InfoPlayer")
  with TourneySvc  
{
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    val x = viewCompPlayer(App.tourney)
    setMainContent(clientviews.info.html.PlayerTmpl(x))
  }

  @JSExport
  def onclickPrintPreview(elem: dom.raw.HTMLInputElement, plIdStr: String, coIdStr: String): Boolean = {
    println("Click on PrintPreview")
    printCert(App.tourney, plIdStr, coIdStr.toLongOption.getOrElse(0L))
    false
  }  

  /** list all competition and the corresponding players
   *  @return: Seq(CompetitionInfo(id,name,cnt), Seq(PlayerInfo(number,name,club,ttr)))            
   */
  def viewCompPlayer(trny: Tourney): Seq[(Long, String, Int, Seq[(String, String, String, String, String)])] = {
    
    def getPlayerInfos(tourney: Tourney, coId: Long): Seq[(String, String, String, String, String)] = {
      (for { 
        ((sno, co), info) <- tourney.pl2co if (co == coId && info.status.equalsTo(PantStatus.REDY, PantStatus.REGI, PantStatus.FINI, PantStatus.PLAY))
      } yield {
        val pant = tourney.getPant(coId, SNO(sno))
        (sno, pant.name, pant.club, tourney.getPantPlace(info.placement), pant.getRatingInfo)
      }).toSeq.filter(_._1 != "").sortWith(_._2 < _._2)
    }

    (for {  
      co      <- trny.comps.values
      plinfos  = getPlayerInfos(trny, co.id) 
    } yield {
      (co.id, trny.getCompName(co.id), plinfos.size, plinfos)
    }).toSeq.filter(_._3 > 0).sortWith(_._3 > _._3)
  }  
}