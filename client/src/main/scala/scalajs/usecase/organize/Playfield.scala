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
    debug("render", s"${App.tourney.startDate}") 
    
    if (App.tourney.playfields.isEmpty) {
      App.updatePlayfield(App.tourney.getToId()).map { _ => 
        val pfs = App.tourney.playfields.values.toSeq
        setMainContent(clientviews.organize.html.Playfield(pfs).toString)      
      }
    } else {
      val pfs = App.tourney.playfields.values.toSeq
      setMainContent(clientviews.organize.html.Playfield(pfs).toString)
    }
  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "ReqFullscreen"    => {
        dom.document.open(s"/club/${App.tourney.orgDir}/${App.tourney.startDate}/playfield", 
          getMsg("tableview"), "channelmode=1,fullscreen=1,location=0,toolbar=0,menubar=0" )
      }
      case "ReqPlayfieldEdit" => {
        DlgPlayfield.show().map { pf => setPlayfield(pf).map { 
            case Left(err)  => debug("onclickAdd", s"error: ${getError(err)}") 
            case Right(res) => {  App.tourney.playfields(pf.nr) = pf; render() }
        }}
      }
      case _                  => error("actionEvent", s"unmatched key: ${key}")
    }
  }  


  @JSExport
  def buttonAdd(): Unit = 
    DlgPlayfield.show().map { pf => setPlayfield(pf).map { 
        case Left(err)  => debug("onclickAdd", s"error: ${getError(err)}") 
        case Right(res) => { App.tourney.playfields(pf.nr) = pf; render() }  
      }}


  @JSExport
  def onclickAdd(elem: dom.raw.HTMLInputElement) = buttonAdd()
  

  @JSExport
  def onclickOpen(elem: dom.raw.HTMLInputElement) = {
    import dom.document
    debug("onclickOpen", s"${elem.id}")
    dom.document.open(s"/club/${App.tourney.orgDir}/${App.tourney.startDate.toString}/playfield", 
      getMsg("tableview"), "channelmode=1,fullscreen=1,location=0,toolbar=0,menubar=0" )
  }

  @JSExport
  def onclickDelete(elem: dom.raw.HTMLInputElement, pfCode: String, pfNrStr: String) = {
    debug("onclickDelete", s"${elem.id} ${pfCode}")
    delPlayfield(pfCode).map { 
      case Left(err) => error("onclickDelete", s"delPlayfield ${getError(err)}") 
      case Right(no) => render()
    }
  }
 

  @JSExport
  def onclickEdit(elem: dom.raw.HTMLInputElement, pfCode: String, pfNo: String) = {
    debug("onclickEdit", s"${elem.id} ${pfNo}") 
    DlgPlayfield.show(App.tourney.playfields(pfNo)).map { 
      pfdlg => setPlayfield(pfdlg).map { 
        case Left(err)  => debug("onclickEdit", s"setPlayfield ${getError(err)}") 
        case Right(res) => render()
      }
    }
  }  


}
