package scalajs.usecase.info

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

// tourney service client imports
import shared.model.{ Tourney, TournBase, Player }
import shared.model.Competition._

import shared.utils.Constants._ 
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
    debug(s"render", s"param: ${param} ucInfo: ${ucInfo}")
    setMainContent(clientviews.info.html.PlayerTmpl(viewCompPlayer).toString)
  }

  @JSExport
  def onclickPrintPreview(elem: dom.raw.HTMLInputElement, plIdStr: String, coIdStr: String): Boolean = {
    printCert(App.tourney, plIdStr, coIdStr.toLongOption.getOrElse(0L))
    false
  }  

  /** list all competition and the corresponding players
   *  @return: Seq(CompetitionInfo(id,name,cnt), Seq(PlayerInfo(number,name,club,ttr)))            
   */
  def viewCompPlayer(): Seq[(Long, String, Int, Seq[(String, String, String, String, String)])] = {

    def getPlayerInfos(tourney: Tourney, coId: Long): Seq[(String, String, String, String, String)] = {
      val fromStatus = if (tourney.comps(coId).status > CS_RESET ) PLS_REDY else PLS_SIGN
      (for { ((sno,co),info) <- tourney.pl2co if (co == coId) } yield {
        if (info.status >= fromStatus & tourney.comps(co).typ == 1) { 
          val pl = info.getPlayerId
          /// format "00000" !!!!
          ("%05d".format(tourney.players(pl).id), 
            tourney.players(pl).getName(),
            tourney.players(pl).clubName, 
            info.getPlaceDesc(BasicHtml.getMsg_ _), 
            tourney.players(pl).getTTR)
        } else if (info.status >= fromStatus &  tourney.comps(co).typ == 2) {
          // DOUBLE
          val pl1 = info.getPlayerId1
          val pl2 = info.getPlayerId2
          ()
          ("%05d".format(tourney.players(pl1).id) + "·" + "%05d".format(tourney.players(pl2).id),
          s"${tourney.players(pl1).lastname}/${tourney.players(pl2).lastname}",
          s"${tourney.players(pl1).clubName}/${tourney.players(pl2).clubName}",
          info.getPlaceDesc(BasicHtml.getMsg_ _), 
          s"${tourney.players(pl1).getTTR}/${tourney.players(pl2).getTTR}")
        } else {
          ("","","","","")
        }
      }).toSeq.filter(_._1 != "").sortWith(_._2 < _._2)
    }

    (for {  
      co      <- App.tourney.comps.values
      plinfos  = getPlayerInfos(App.tourney, co.id) 
    } yield {
      (co.id, co.genName(getMsg _), plinfos.size, plinfos)
    }).toSeq.filter(_._3 > 0).sortWith(_._3 > _._3)
  }  
}