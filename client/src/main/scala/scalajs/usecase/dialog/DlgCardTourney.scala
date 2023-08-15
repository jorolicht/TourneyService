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
import shared.model.TournBase
import shared.utils._
import shared.utils.Constants._
import clientviews.dialog.html

@JSExportTopLevel("DlgCardTourney")
object DlgCardTourney  extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp = UseCaseParam("APP__DlgCardTourney", "dlg.card.tourney", "DlgCardTourney", "dlgcardtourney", scalajs.AppEnv.getMessage _ )
 
  @JSExport
  def actionEvent(key: String, elem: raw.HTMLElement, event: Event) = {
    //debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "VISIBILITY"   => if (elem.asInstanceOf[HTMLInputElement].value.toIntOption.getOrElse(-1) > 0) markInput(elem, None) else markInput(elem, Option(false))
      case _              => {}
    }
  }  


  // set dialog input fields
  def set(mode: String, tB: TournBase): Unit = {
    import shared.utils.Routines._
    resetInput("Modal")
    setInput("Id", tB.id.toString)
    setInput("OrgDir", tB.orgDir)
    setInput("Organizer", tB.organizer)
    setInput("Name", tB.name)
    setAttribute(gE("Name", ucp), "autocomplete", "off")
   
    setDateTimePicker("StartDate", AppEnv.getLang, int2ymd(tB.startDate) )
    setDateTimePicker("EndDate", AppEnv.getLang, int2ymd(tB.endDate))
  
    setInput("ContactName", tB.getContactName)
    setInput("ContactEMail", tB.getContactEmail)
    setInput("ContactPhone", tB.getContactPhone)
    setInput("AddrDescription", tB.getAddrDescription)
    setInput("AddrZIP", tB.getAddrZIP)
    setInput("AddrCity", tB.getAddrCity)
    setInput("AddrStreet", tB.getAddrStreet)
    setInput("AddrCountry", tB.getAddrCountry)
    mode match {
      case "edit" => {
        setVisible("BtnClose", false); setVisible("BtnSave", true); setVisible("BtnCancel", true)
        setAttribute(gE("StartDate", ucp), "readonly", "readonly")
        setBooleanOption("Visibility", Some(tB.privat))
        setIntOption("Typ", Some(tB.typ))
      }  
      case "new"    => {
        setVisible("BtnClose", false); setVisible("BtnSave", true); setVisible("BtnCancel", true)
        setBooleanOption("Visibility", None)
        setIntOption("Typ", None)
      }
      case "view"    => {
        setVisible("BtnClose", true); setVisible("BtnSave", false); setVisible("BtnCancel", false)
      }
      case _        => error("set", s"invalid mode: ${mode}")
    }
  }

        //   <button @id("BtnCancel") class="btn btn-outline-secondary btn-xs" type="button" data-dismiss="modal">@msg_("std.btn.cancel")</button>
        // <button @id("BtnSave")   class="btn btn-outline-secondary btn-xs" type="button">@msg_("std.btn.save")</button>
				// <button @id("BtnClose") 


  // validate dialog for TournBase, return valid TournBase
  def validate(): Either[List[Error], TournBase] = {
    import shared.model._
    import shared.utils.Routines._
    import scala.collection.mutable.ListBuffer
    var eList      = ListBuffer[Error]()
    val name       = getInput("Name")
    val id         = getInput("Id", 0L)
    val orgDir     = getInput("OrgDir")
    val organizer  = getInput("Organizer")
    val typ        = getIntOption("Typ")
    val privat     = getBooleanOption("Visibility")
    val startDate  = date2Int(getInput("StartDate"))
    val endDate    = date2Int(getInput("EndDate"))

    val addr = Address(getInput("AddrDescription"), getInput("AddrCountry"), getInput("AddrZIP"), getInput("AddrCity"), getInput("AddrStreet"))  
    val (lastname, firstname) = splitName(getInput("ContactName"))
    val contact = Contact(lastname, firstname, getInput("ContactPhone"), getInput("ContactEMail"))
  
    if (privat == None)        { eList += Error("dlg.TournBase.hlp.Visibility"); markInput(gE("Visibility"), Option(true)) }
    
    // check startDate only when creating a new entry
    if (getNow() > startDate & id==0) { eList += Error("dlg.TournBase.hlp.nowDate");    markInput(gE("StartDate"), Option(true)) }

    if (endDate < startDate)   { eList += Error("dlg.TournBase.hlp.endDate");    markInput(gE("EndDate"), Option(true)) }
    if (name.length<10)        { eList += Error("dlg.TournBase.hlp.Name");       markInput(gE("Name"), Option(true)) }
    if (typ == None)           { eList += Error("dlg.TournBase.hlp.Typ");        markInput(gE("Typ"), Option(true)) } 

    if (eList.length > 0) {
      Left(eList.toList)
    } else {
      Right(TournBase(name, organizer, orgDir, startDate, endDate, "", typ.getOrElse(0), privat.getOrElse(true), contact.encode, addr.encode, id))
    }
  }

  
  // show dialog return result (Future) or Failure when canceled
  def show(dlgMode: String, tournBase: TournBase): Future[Either[Error, TournBase]] = {
    val p     = Promise[TournBase]()
    val f     = p.future

    def cancel() = {
      offEvents(gE("Modal", ucp), "hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = {
      validate() match {
        case Left(eList) => DlgShowError.show(eList)
        case Right(tb)   => {
          if (!p.isCompleted) p success tb
          offEvents(gE("Modal", ucp), "hide.bs.modal")
          doModal(gE("Modal", ucp), "hide")
        }  
      }
    }
    
    loadModal(html.DlgCardTourney(), ucp)
    setHtml("Header", getMsg(s"${dlgMode}.hdr"))
    set(dlgMode, tournBase)

    // register routines for cancel and submit
    onEvents(gE("Modal", ucp), "hide.bs.modal", () => cancel())
    onClick(gE("BtnSave", ucp), (e: Event) => submit(e))
    doModal(gE("Modal", ucp), "show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}