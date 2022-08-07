package scalajs.usecase.dialog

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Success, Failure }
import scala.util.matching
import scala.concurrent._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._


import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.model.CompPhase._
import shared.model.Competition._
import shared.model.{ Tourney, Competition }
import shared.utils._
import shared.utils.Routines._
import clientviews.dialog.html
import _root_.org.w3c.dom.html.HTMLSelectElement

// Start TestCases
// DlgCardPlayer Edit: http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardComp&ucInfo=Edit
// DlgCardPlayer Show: http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardComp&ucInfo=Show
// DlgCardPlayer New:  http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardComp&ucInfo=New

@JSExportTopLevel("DlgCardComp")
object DlgCardComp extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgCardComp", "dlg.card.comp", "DlgCardComp", "dlgcardcomp", scalajs.AppEnv.getMessage _ )
  private def load() = if (!checkId("Modal")) insertHtml_("APP__Load", "afterbegin", html.DlgCardComp().toString)
 
  @JSExport
  def actionEvent(key: String, elem: raw.HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "NameCompose"  => { 
        if (getCheckbox("NameCompose")) {  
          val typ = getInput("typ", 0)
          setInput("name", getInput("AgeGroup", getMsg("plh.AgeGroup") )+ "·" 
                               + getInput("Class", getMsg("plh.Class")) + "·" 
                               + BasicHtml.getMsg_("competition.typ."+typ) )
          setAttribute("name", "readonly", "true")                     
        } else {
          removeAttribute("name", "readonly") 
          setInput("name", getInput("name").replace("·", " "))
        }
      } 
      
      case "Close"   => { $(getId("Modal","#")).off("hide.bs.modal");  $(getId("Modal","#")).modal("hide") }
      case _         => {}
    }
  }  


  // validate dialog for Competition, return valid Competition
  def validate(comp: Competition, trny: Tourney, mode: DlgOption.Value): Either[List[Error], Competition] = {
    import shared.model.Competition
    import shared.utils.Routines._
    import scala.collection.mutable.ListBuffer
    
    var eList = ListBuffer[Error]()

    if (comp.name.length <= 2 )  { eList += Error("dlg.card.comp.hlp.name");   markInput("name", Option(true))   }
    if (comp.typ == CT_UNKN )    { eList += Error("dlg.card.comp.hlp.typ");    markInput("typ", Option(true))    }
    if (comp.status == CS_UNKN ) { eList += Error("dlg.card.comp.hlp.status"); markInput("status", Option(true)) }
    if (!comp.validateDate(trny.startDate, trny.endDate)) { 
      eList += Error("dlg.card.comp.hlp.startTime"); markInput("startTime", Option(true)) 
    }

    /* read relevant input and verify it */
    if (eList.length > 0) Left(eList.toList) else Right(comp)
  }


  // getInput
  def getInput(): Competition = {

    val comp = Competition(getData("Form","id", 0L), getData("Form","hashKey",""), 
                           getInput("name", ""), getInput("typ", 0),
                           Competition.parseStartTime(getInput("startTime", "")),
                           getInput("status", CS_UNKN), getData("Form", "options", ""))
    // set optional values                       
    comp.setAgeGroup(getInput("AgeGroup", ""))
    comp.setRatingRemark(getInput("Class", ""))
    comp.setRatingLowLevel(getInput("TTRFrom", 0))
    comp.setRatingUpperLevel(getInput("TTRTo", 0))
    comp.setSex(getInput("Sex", 0))
    comp
  }

  // setInput
  def setInput(comp: Competition, trny: Tourney, lang: String, mode: DlgOption.Value): Unit = {
        
    // setting data-foo
    setData("Form", "id", comp.id)
    setData("Form", "hashKey", comp.hashKey)
    setData("Form", "name", comp.name)
    setData("Form", "typ", comp.typ) 
    setData("Form", "options", comp.options)

    // set start date and time
    if (mode == DlgOption.New) {
      setDateTimePicker("startTime", lang, int2ymd(trny.startDate), (12, 0))
      setData("Form", "status", CS_UNKN)
    } else {
      val (year, month, day, hour, minute) = ymdHM(comp.startDate)
      setDateTimePicker("startTime", lang, (year, month, day), (hour, minute))
      setData("Form", "status", comp.status)
    }

    setInput("coId", if (comp.id==0) "X" else comp.id.toString)
    setInput("name", comp.name)

    setAttribute("name", "autocomplete", "off")
    setInput("status", comp.status.toString)
    setInput("AgeGroup", comp.getAgeGroup)
    setInput("TTRFrom", comp.getFromTTR)
    setInput("TTRTo", comp.getToTTR)

    setInput("typ", comp.typ.toString)
    val (cnt, cntActiv) = trny.getCompCnt(comp)
    setDisabled("typ", cnt==0)
    
    setInput("Class", comp.getRatingRemark) 
    setInput("Sex", comp.getSex.toString)
    setCheckbox("NameCompose", comp.isNameComposed())

    setButton(mode)
    setInputFields(mode == DlgOption.View)
    setVisible("Compose", mode != DlgOption.View)
    setVisible("Optional", mode != DlgOption.View)
    if (comp.status > CPC_INIT) setDisabled("status", true)
  }

  // setButton
  def setButton(mode: DlgOption.Value): Unit = mode match {
    case DlgOption.Edit => setVisible("Cancel", true);  setVisible("Submit", true);  setVisible("Close", false)
    case DlgOption.View => setVisible("Cancel", false); setVisible("Submit", false); setVisible("Close", true)
    case DlgOption.New  => setVisible("Cancel", true);  setVisible("Submit", true);  setVisible("Close", false)
  }        

  // set all input field enabled/disabled
  def setInputFields(disabled: Boolean): Unit = {
    val container = document.querySelector(getIdHa("Form"))
    container.querySelectorAll("input, select").map { elem => 
      setDisabled(elem.asInstanceOf[HTMLInputElement], disabled)
      markInput__(elem.asInstanceOf[HTMLInputElement], None)
    }
  }

  
  // show dialog return result (Future) or Failure when canceled
  def show(comp: Competition, trny: Tourney, lang: String, mode: DlgOption.Value): Future[Either[Error, Competition]] = {
    val p     = Promise[Competition]()
    val f     = p.future

    def cancel() = {
      $(getId("Modal","#")).off("hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = {
      validate(getInput(), trny, mode) match {
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
    setInput(comp, trny, lang, mode)

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event)     => submit(e)) 
    $(getId("Modal","#")).modal("show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}