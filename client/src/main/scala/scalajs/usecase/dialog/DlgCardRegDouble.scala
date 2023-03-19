package scalajs.usecase.dialog

// Start TestCases
// DlgCardRegDouble: http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardRegDouble

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
import shared.model.{ Tourney, Player, Pant }
import shared.utils._
import clientviews.dialog.html


@JSExportTopLevel("DlgCardRegDouble")
object DlgCardRegDouble extends BasicHtml 
  with TourneySvc with ViewServices
{
  this: BasicHtml =>
  
  implicit val ucp     = UseCaseParam("APP__DlgCardRegDouble", "dlg.card.reg.double", "DlgCardRegDouble", "dlgcardregdouble", scalajs.AppEnv.getMessage _ )
 
  var selCoId = 0L
  implicit var tourney = Tourney.init

  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    def setNameList(name: String, trny: Tourney, coId: Long, setId: Long, skipId: Long): Unit = {
      setHtml(name, genPlayerOption(name, trny, coId, setId, skipId))
      setInput(name, setId.toString)
    }


    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "Name1" => setNameList("Name2", tourney, selCoId, getInput("Name2", 0L), getInput("Name1", 0L) )        
      case "Name2" => setNameList("Name1", tourney, selCoId, getInput("Name1", 0L), getInput("Name2", 0L) ) 
      case "Close" => { $(getId("Modal","#")).off("hide.bs.modal");  $(getId("Modal","#")).modal("hide") }
      case _       => {}
    }
  }

  /** validate input of dialog for Double, return valid tupel of player id or a List of Errors
   * 
   */ 
  def validate(coId: Long)(implicit trny: Tourney): Either[List[Error], (Long, Long, Int)] = {
    import shared.utils.Routines._
    import scala.collection.mutable.ListBuffer
    
    var eList   = ListBuffer[Error]()
    val status = getRadioBtn("PlayerStatus", -1)
    debug("validate", s"status: ${status}")
 
    val id1 = getInput("Name1", 0L)
    val id2 = getInput("Name2", 0L)

    if (id1 <= 0L) eList += Error("err0164.RegDouble.Name.missing", "1")
    if (id2 <= 0L) eList += Error("err0164.RegDouble.Name.missing", "2" )

    /* read relevant input and verify it */
    if (eList.length > 0) Left(eList.toList) else Right((id1, id2, status))
  }

  def setNameList(name: String, trny: Tourney, coId: Long, setId: Long, skipId: Long): Unit = {
    import shared.utils.Routines._

    // list all players minus players which are already registered and remove also player 1 (if existing)
    val plIds = trny.players.keySet
    val pl2co = trny.pl2co.keySet.filter( ((x)) => x._2==coId ).map( ((x)) => getMDLongArr(x._1).toSeq).flatten
    val diffSet = plIds.diff(pl2co + skipId)
    debug("setNameList", s"plIds: ${plIds} pl2co: ${pl2co} diffSet: ${diffSet}     ")

    val nameOptions = (for ((id) <- diffSet) yield s"""<option value="${id}">${trny.players(id).getName(1)} (${trny.players(id).getClub(0)})</option>""").mkString(" ") 
    setHtml(name, "<option value='0'>---</option>" + nameOptions)
    setInput(name, setId.toString)
  }


  // init dialog view
  def init(trny: Tourney, coId: Long): Unit = {
    tourney = trny
    selCoId = coId
    setNameList("Name1", trny, coId, 0, 0)
    setNameList("Name2", trny, coId, 0, 0)
    setRadioBtnByValue("PlayerStatus", Pant.REDY.toString)
    setHtml("Class", trny.getCompName(coId))
  }
  
  // show dialog return result (Future) or Failure when canceled
  def show(coId: Long, trny: Tourney, lang: String): Future[Either[Error, (Long, Long, Int)]] = {
    val p     = Promise[(Long, Long, Int)]()
    val f     = p.future

    def cancel() = {
      $(getId("Modal","#")).off("hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event) {
      validate(coId) match {
        case Left(eList) => DlgShowError.show(eList)
        case Right(result)   => {
          if (!p.isCompleted) p success result
          //disable modal first, then hide
          $(getId("Modal","#")).off("hide.bs.modal")
          $(getId("Modal","#")).modal("hide")
        }  
      }
    }
    
    loadModal(html.DlgCardRegDouble(), ucp)
    init(trny, coId)

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event)     => submit(e)) 
    $(getId("Modal","#")).modal("show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}