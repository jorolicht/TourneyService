package scalajs.usecase.organize

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.{ Event, HTMLInputElement, HTMLFormElement } // for ScalaJs bindings

import shared.model.Playfield
import shared.utils._

import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// *** 
// Playfield administration
// ***
@JSExportTopLevel("OrganizePlayfield")
object OrganizePlayfield extends UseCase("OrganizePlayfield") 
   with TourneySvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    // debug("render", s"${App.tourney.startDate}") 
    val pf = App.tourney.playfields.values.filter(_.coCode != (0L,0)).toSeq
    val pfInfo = App.tourney.playfields.values.filter(_.coCode == (0L,0)).toSeq
    setMainContent(clientviews.organize.html.Playfield(pf, pfInfo))
  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    def check(input: String): Either[String, Boolean] = {
      if      (input == "")    Left(getMsg("noInput"))
      else                     Right(true)
    }

    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "Fullscreen" => {
        dom.document.open(s"/club/${App.tourney.orgDir}/${App.tourney.startDate}/playfield", 
          getMsg("tableview"), "channelmode=1,fullscreen=1,location=0,toolbar=0,menubar=0" )
      }
      case "AddInfo"    =>  DlgInputTxt.show(getMsg("hdr.addTourneyInfo"), getMsg("msg.addTourneyInfo"), getMsg("plh.addTourneyInfo"), gM("std.btn.cancel"), gM("std.btn.submit"), check) map { 
        case Left(err)    => {} // error("actionEvent", s"AddInfo: ${err}")
        case Right(value) => setPlayfield(App.tourney.genPlayfieldFromInfo(value)) map {
          case Left(err)  => error("actionEvent", s"setPlayfield: ${err}")
          case Right(res) => render() 
        }
      }

      case _                  => error("actionEvent", s"unmatched key: ${key}")
    }
  }


  @JSExport
  def onclickDelete(elem: dom.raw.HTMLInputElement) = {
    val pfNo = getData(elem, "pfNo", "")
    //debug("onclickDelete", s"elem: ${elem.id} pfNo: ${pfNo}")
    delPlayfield(pfNo).map { 
      case Left(err) => error("onclickDelete", s"delPlayfield ${getError(err)}") 
      case Right(no) => render()
    }
  }


}
