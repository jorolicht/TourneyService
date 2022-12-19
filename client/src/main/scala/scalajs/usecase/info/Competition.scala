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
import shared.model.{ Tourney, TournBase, Player, Participant }
import shared.model.Competition._
import shared.utils._


import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }

// ***
// User Info View Competitions
// ***    
@JSExportTopLevel("InfoCompetition")
object InfoCompetition extends UseCase("InfoCompetition") 
  with TourneySvc
{    
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    info("render", s"comps: ${App.tourney.comps}")
    setMainContent(clientviews.info.html.Competition(viewComp(AppEnv.getLang)).toString)
  }  

    
  /** list all competitions together with number of registered user and active user
   *  @return Info for Competition (id, name, age_group, rating_remark, type, start_date, #regUser, #activUser, status, options)
   */
  def viewComp(lang: String): Seq[(Long, String, String, String, Int, String, Int, Int, Int, String, String)] = {
    val tourney = App.tourney

    (for {  
      c       <- tourney.comps.values
      players  = if (c.status > CS_RESET) tourney.pl2co.values.filter(_.coId == c.id).filter(_.status >= Participant.PLS_REDY).toSeq else tourney.pl2co.values.filter(_.coId == c.id).toSeq
    } yield {
      info("viewComp", s"ID: ${c.id}")
      val cnt = players.filter(_.status > 0).length
      (c.id, c.name, c.getAgeGroup, c.getRatingRemark, c.typ, c.formatTime(lang, 2), players.length, cnt, c.status, c.genRange, c.formatTime(lang, 1))
    }).toSeq.sortWith(_._5 > _._5).sortWith(_._6 < _._6)
  }
}