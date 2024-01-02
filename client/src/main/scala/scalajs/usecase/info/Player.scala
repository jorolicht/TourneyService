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
import shared.model.{ Tourney, TournBase, Player, PantStatus, CompStatus, CompTyp }
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
    debug(s"render", s"param: ${param} ucInfo: ${ucInfo}")
    val x = viewCompPlayer
    debug(s"render", s"${x}")
    setMainContent(clientviews.info.html.PlayerTmpl(x))
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
      (for { ((sno,co),info) <- tourney.pl2co if (co == coId) } yield {
        if (info.status.equalsTo(PantStatus.REDY, PantStatus.REGI, PantStatus.FINI, PantStatus.PLAY) & tourney.comps(co).typ.equalsTo(CompTyp.SINGLE)) { 
          val pl = info.getPlayerId
          /// format "00000" !!!!
          ("%05d".format(tourney.players(pl).id), tourney.players(pl).getName(), tourney.players(pl).clubName, info.getPlaceDesc(gM _), tourney.players(pl).getTTR)
        } else if (info.status.equalsTo(PantStatus.REDY, PantStatus.REGI, PantStatus.FINI,PantStatus.PLAY) & tourney.comps(co).typ.equalsTo(CompTyp.DOUBLE)) {           
          info.getDoubleId match {
            case Left(err) => println(s"ERROR: ${err.toString()}"); ("","","","","")
            case Right(id) => {
              ("%05d".format(tourney.players(id._1).id) + "·" + "%05d".format(tourney.players(id._2).id),
               s"${tourney.players(id._1).lastname}/${tourney.players(id._2).lastname}",
               s"${tourney.players(id._1).clubName}/${tourney.players(id._2).clubName}",
               info.getPlaceDesc(gM _), 
               s"${tourney.players(id._1).getTTR}/${tourney.players(id._2).getTTR}")
            }   
          }
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