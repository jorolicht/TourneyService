package scalajs.usecase.dialog

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JSON
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement, HTMLFormElement } // for ScalaJs bindings
//import scalatags.Text.all._              // from "com.lihaoyi" %%% "scalatags" % "0.6.7"


//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.collection.mutable.HashMap
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import shared.model.Playfield
import shared.utils.UseCaseParam

import scalajs.usecase.component._
import scalajs.service._
import scalajs.usecase._
import scalajs.{ App, AppEnv }

@JSExportTopLevel("DlgPlayfield")
object DlgPlayfield  extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgPlayfield", "dlg.playfield", "DlgPlayfield", "dlgplayfield", scalajs.AppEnv.getMessage _ )

  var playerList: Seq[(String,String)]   = Seq()
  var clubPlayer: HashMap[String,String] = HashMap()
  var plNameList: String = ""

  private def load = if ($(getIdHa("Modal")).length <= 0) {
    debug("load", "playfield dialog")
    BasicHtml.setHtml_("APP__DlgPlayfield__Load", clientviews.dialog.html.DlgPlayfield(AppEnv.msgs).toString)
  }

  @JSExport
  def changeComp(obj: js.Dynamic): Unit = {
    val coId = getInput("Competition",0L)
    
    $(getIdHa("NameA")).`val`("")
    $(getIdHa("NameB")).`val`("")
    $(getIdHa("ClubA")).`val`("")
    $(getIdHa("ClubB")).`val`("")

    if (coId > 0) {
      playerList = App.tourney.getNamesClubPlayer(coId)
      clubPlayer = collection.mutable.HashMap( playerList.map(entry => entry._2 -> entry._1) : _*) 
    } else {
      playerList = Seq()
    }
    
    plNameList = clientviews.playfield.html.PlayerDatafield(playerList).toString
    $(getIdHa("NameListA")).html(plNameList)
    $(getIdHa("NameListB")).html(plNameList)

    debug(s"changeCompetition", s"coId: ${coId}")
  }

  @JSExport
  def changePlayer(pl: Int): Unit = {
   
    pl match {
      case 0 => {
        val player =  $(getIdHa("NameA")).value.asInstanceOf[String]
        if (player != "")  $(getIdHa("ClubA")).`val`(clubPlayer(player)) else $(getIdHa("ClubA")).`val`("")
      }
      case 1 => {
        val player =  $(getIdHa("NameB")).value.asInstanceOf[String]
        if (player != "")  $(getIdHa("ClubB")).`val`(clubPlayer(player)) else $(getIdHa("ClubB")).`val`("")
      }
      case _ => debug(s"changePlayer", s"invalid number ${pl.toString}")
    }
  }


  // set playfield dilaog with playfield values
  private def set(pf: Playfield) = {
    setInput("NameA", pf.playerA)
    setInput("ClubA", pf.clubA)
    setInput("NameB", pf.playerB)
    setInput("ClubB", pf.clubB)
    setHtml("Competition", clientviews.playfield.html.CompOption(App.tourney.getNamesComp(), pf.pClass).toString )
    setInput("Info", pf.info)

    if (pf.nr > 0) {
      $(getIdHa("InfoOption")).value(1.toString)
      $(getIdHa("Nr")).attr("value", pf.nr)
    } else if (pf.nr == 0 & pf.info != "") {
      $(getIdHa("InfoOption")).value(2.toString)
      $(getIdHa("Nr")).attr("value", "")
    } else {
      $(getIdHa("InfoOption")).value(1.toString)
      $(getIdHa("Nr")).attr("value", "")      
    }
  }

  def verifyCAB(competition: String, playerA: String, playerB: String): Boolean = {
    val someEmpty = (competition.trim == "" | playerA.trim == "" | playerB.trim == "")
    if (someEmpty) $(getIdHa("Help")).text(getMsg("help1"))
    !someEmpty
  }

  def verifyInfo(info: String, pfNr: Int): Boolean = {
      (info.trim != "" & pfNr == 0)
  }

  def verifyPlayfield(pfNr: Int) : Boolean = {
      val someEmpty = pfNr < 1
      if (someEmpty) $(getIdHa("Help")).text(getMsg("help2"))
      !someEmpty
    }


  // read playfield dilaog with playfield values
  def read(): Playfield = {
    val coId = getInput("Competition",0L)
    val nr = if (getInput("InfoOption",0) != 2) getInput("Nr",0) else 0

    Playfield(nr, true, "19700101000000", "", 
      getInput("NameA"), getInput("ClubA"),
      getInput("NameB"), getInput("ClubB"),
      $(getIdHa(s"Competition_${coId}")).text,
      getInput("Info")
    )
  }
  
  // show dialog return result (Future) or Failure when canceled
  def show(pf: Playfield = Playfield(0, false, "19700101000000" , "", "", "", "", "", "", "") ): Future[Playfield] = {
    val p = Promise[Playfield]()
    val f = p.future

    def cancel() = if (!p.isCompleted) { p failure (new Exception("cancel")) }

    def submit(e: Event) {
      val pf = read()
      if ( (verifyCAB(pf.pClass, pf.playerA, pf.playerB) && verifyPlayfield(pf.nr) ) || 
           verifyInfo(pf.info, pf.nr) ) {
        if (!p.isCompleted) { 
          p success pf 
          $(getIdHa("Modal")).modal("hide")
        }
      } else {
        debug("submit", s"validation failed")
        $(getIdHa("Help")).show() 
      } 
    }
    
    load
    $(getIdHa("Form")).trigger("reset")
    $(getIdHa("Help")).hide()
    set(pf)

    // register routines for cancel and submit
    $(getIdHa("Modal")).on("hide.bs.modal", () => cancel())
    $(getIdHa("Submit")).click( (e: Event) => submit(e)) 
    //showModal("Modal")

    //$(getIdHa("Modal")).modal("show")
    debug(s"show", s"dialog ${getIdHa("Modal")}")

    f   // return the future
  }
  
}