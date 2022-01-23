package scalajs.usecase.dialog

// Start TestCases
// DlgCardCfgSection: http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardCfgSection&ucInfo=23#
//                    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardCfgSection&ucInfo=9#
//                    
//                    

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Success, Failure }
import scala.util.matching
import scala.concurrent._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.document
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext._


import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.model.{ Tourney, Player, ParticipantEntry }
import shared.utils._
import shared.utils.Constants._
import clientviews.dialog.DlgCardCfgSection.html


@JSExportTopLevel("DlgCardCfgSection")
object DlgCardCfgSection extends BasicHtml 
  with TourneySvc with DrawSvc
{
  this: BasicHtml =>
  implicit val ucp     = UseCaseParam("APP__DlgCardCfgSection", "dlg.card.cfg.section", "DlgCardCfgSection", "dlgcardcfgsection", scalajs.AppEnv.getMessage _ )
  private def load()   = if (!checkId("Modal")) insertHtml_("APP__Load", "afterbegin", html.Main().toString)
 

  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {     
      case "Selection" => { 
        debug("actionEvent", s"Selection ${elem.asInstanceOf[Input].value}") 
        setDisabled("Submit", elem.asInstanceOf[Input].value.toIntOption == None)
      }
      case "Close"     => { $(getId("Modal","#")).off("hide.bs.modal");  $(getId("Modal","#")).modal("hide") }
      case _           => {}
    }
  }

  /** validate input configuration input return selected option or a List of Errors
   * 
   */ 
  def validate(): Either[List[Error], Int] = {
    Right(7)
  }

  // set possible configuration section 
  def set(coId: Long, secId: Int, size: Int, lang: String, pants: Array[ParticipantEntry])
         (implicit trny: Tourney): Unit = {

    val cfgOptions = grpOptions(size)
    val selOptions = new StringBuilder("<option value='None' selected>---</option>")
    for (cfg <- cfgOptions) {
      val msg= getMsg(s"option.${cfg}")
      selOptions ++= s"<option value='${cfg}'>${msg}</option>" 
    }
    setHtml("PantTbl", html.Pants(pants))
    setHtml("CfgSelection", selOptions.toString) 
    setHtml("lbl.Name", getMsg("lbl.Name", size.toString)) 
  }
  
  // show dialog return result (Future) or Failure when canceled
  def show(coId: Long, secId: Int, size:Int, lang: String, pants: Array[ParticipantEntry])
          (implicit trny: Tourney): Future[Either[Error, Int]] = {
    val p     = Promise[Int]()
    val f     = p.future

    def cancel() = {
      $(getId("Modal","#")).off("hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = {
      validate() match {
        case Left(eList) => DlgShowError.show(eList)
        case Right(result)   => {
          if (!p.isCompleted) p success result
          //disable modal first, then hide
          $(getId("Modal","#")).off("hide.bs.modal")
          $(getId("Modal","#")).modal("hide")
        }  
      }
    }
    
    load()
    set(coId, secId, size, lang, pants)

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event)     => submit(e)) 
    $(getId("Modal","#")).modal("show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}