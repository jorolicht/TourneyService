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

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.model.CompPhase._
import shared.model.Competition._
import shared.model.{ Tourney, Competition, CompTyp, CompStatus }
import shared.utils._
import shared.utils.Routines._
import clientviews.dialog.html
import _root_.org.w3c.dom.html.HTMLSelectElement


@JSExportTopLevel("DlgCardComp")
object DlgCardComp extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgCardComp", "dlg.card.comp", "DlgCardComp", "dlgcardcomp", scalajs.AppEnv.getMessage _ )
 
  @JSExport
  def actionEvent(key: String, elem: raw.HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "NameCompose"  => { 
        if (getCheckbox(gUE("NameCompose"))) {  
          val typ = CompTyp(getInput(gUE("typ"), 0))
          setInput("name", getInput(gUE("AgeGroup"), gM("CompAgeGroup.plh"))  + "·" + 
                           getInput(gUE("Class"),    gM("CompClass.plh")) + "·" + 
                           gMTyp(typ) )
          setAttribute(gUE("name"), "readonly", "true")                     
        } else {
          removeAttribute(gUE("name"), "readonly") 
          setInput("name", getInput(gUE("name")).replace("·", " "))
        }
      } 
      
      case "Close"   => offEvents(gE(uc("Modal")), "hide.bs.modal"); doModal(gE(uc("Modal")), "hide")
      case _         => {}
    }
  }  


  // validate dialog for Competition, return valid Competition
  def validate(comp: Competition, trny: Tourney, mode: DlgOption.Value): Either[List[Error], Competition] = {
    import shared.model.Competition
    import shared.utils.Routines._
    import scala.collection.mutable.ListBuffer
    
    var eList = ListBuffer[Error]()

    if (comp.name.length <= 2 )         { eList += Error("dlg.card.comp.hlp.name");   markInput(gUE("name"), Option(true))   }
    if (comp.typ == CompTyp.Typ )       { eList += Error("dlg.card.comp.hlp.typ");    markInput(gUE("typ"),  Option(true))    }
    //if (comp.status == CompStatus.UNKN ) { eList += Error("dlg.card.comp.hlp.status"); markInput(gE("status"), Option(true)) }
    if (!comp.validateDate(trny.startDate, trny.endDate)) { 
      eList += Error("dlg.card.comp.hlp.startTime"); markInput(gUE("startTime"), Option(true)) 
    }

    /* read relevant input and verify it */
    if (eList.length > 0) Left(eList.toList) else Right(comp)
  }


  // getInput
  def getInput(): Competition = {

    val comp = Competition(getData(gUE("Form"),"id", 0L), getInput(gUE("name")), CompTyp(getInput(gUE("typ"), 0)),
                           parseStartTime(getInput(gUE("startTime"))),
                           CompStatus(getInput(gUE("status"), CompStatus.CFG.id)), getData(gUE("Form"), "options", ""))
    // set optional values                       
    comp.setAgeGroup(getInput(gUE("AgeGroup")))
    comp.setRatingRemark(getInput(gUE("Class")))
    comp.setRatingLowLevel(getInput(gUE("TTRFrom"), 0))
    comp.setRatingUpperLevel(getInput(gUE("TTRTo"), 0))
    comp.setSex(getInput(gUE("Sex"), 0))
    comp
  }

  // setInput
  def setInput(comp: Competition, trny: Tourney, lang: String, mode: DlgOption.Value): Unit = {
        
    // setting data-foo
    setData(gUE("Form"), "id", comp.id)
    setData(gUE("Form"), "name", comp.name)
    setData(gUE("Form"), "typ", gMTyp(comp.typ)) 
    setData(gUE("Form"), "options", comp.options)

    // set start date and time
    if (mode == DlgOption.New) {
      setDateTimePicker("startTime", lang, int2ymd(trny.startDate), (12, 0))
      setData(gUE("Form"), "status", CompStatus.CFG.id)
    } else {
      val (year, month, day, hour, minute) = ymdHM(comp.startDate)
      setDateTimePicker("startTime", lang, (year, month, day), (hour, minute))
      setData(gUE("Form"), "status", comp.status.id)
    }

    setInput("coId", if (comp.id==0) "X" else comp.id.toString)
    setInput("name", comp.name)

    setAttribute(gE(uc("name")), "autocomplete", "off")
    setInput("status", gMTyp(comp.status))
    setInput("AgeGroup", comp.getAgeGroup)
    setInput("TTRFrom", comp.getFromTTR)
    setInput("TTRTo", comp.getToTTR)

    setInput("typ", s"${comp.typ.id}")
    val (cnt, cntActiv) = trny.getCompCnt(comp)
    setDisabled("typ", cnt==0)
    
    setInput("Class", comp.getRatingRemark) 
    setInput("Sex", comp.getSex.toString)
    setCheckbox("NameCompose", comp.isNameComposed())

    setButton(mode)
    setInputFields(mode == DlgOption.View)
    setVisible("Compose", mode != DlgOption.View)
    setVisible("Optional", mode != DlgOption.View)
    //if (comp.status > CompStatus.READY) setDisabled("status", true)
    setDisabled("status", true)
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
      markInput(elem.asInstanceOf[HTMLInputElement], None)
    }
  }

  
  // show dialog return result (Future) or Failure when canceled
  def show(comp: Competition, trny: Tourney, lang: String, mode: DlgOption.Value): Future[Either[Error, Competition]] = {
    val p     = Promise[Competition]()
    val f     = p.future

    def cancel() = {
      offEvents(gE(uc("Modal")), "hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = {
      validate(getInput(), trny, mode) match {
        case Left(eList) => DlgShowError.show(eList)
        case Right(result)   => {
          if (!p.isCompleted) p success result
          //disable modal first, then hide
          offEvents(gUE("Modal"), "hide.bs.modal")
          doModal(gUE("Modal"), "hide")
        }  
      }
    }
    
    loadModal(html.DlgCardComp(), ucp)
    setInput(comp, trny, lang, mode)

    // register routines for cancel and submit
    onEvents(gUE("Modal"), "hide.bs.modal", () => cancel())
    onClick(gUE("Submit"), (e: Event) => submit(e))
    doModal(gUE("Modal"), "show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}