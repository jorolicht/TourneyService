package scalajs.usecase.dialog

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

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.model.{ Tourney, Player, PantStatus }
import shared.utils._
import clientviews.dialog.html


@JSExportTopLevel("DlgCardRegSingle")
object DlgCardRegSingle extends BasicHtml 
  with TourneySvc with AuthenticateSvc with ViewServices
{
  this: BasicHtml =>
  implicit val ucp     = UseCaseParam("APP__DlgCardRegSingle", "dlg.card.reg.single", "DlgCardRegSingle", "dlgcardregsingle", scalajs.AppEnv.getMessage _ )
  implicit var tourney = Tourney.init
 
  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "Name" => { 
        debug("actionEvent", s"Input value: ${elem.asInstanceOf[Input].value}")

        Player.validateName(elem.asInstanceOf[Input].value) match {
          case Left(err)   => setPlayerView("", "", 0L)
          case Right(lfid) => setPlayerView(lfid._1, lfid._2, lfid._3)
        }
      }
    
      case "Close"   => {  
        offEvents(gUE("Modal"), "hide.bs.modal")
        doModal(gUE("Modal"), "hide")
      }    

      case _         => {}
    }
  }

  /** validate input of dialog for Player, return valid Player or a List of Errors
   * 
   */ 
  def validate(coId: Long)(implicit trny: Tourney): Either[List[Error], (Player, PantStatus.Value)] = {
    import shared.model.Player
    import shared.model.SexTyp
    import shared.model.Club
    import shared.utils.Routines._
    import scala.collection.mutable.ListBuffer
    
    var eList   = ListBuffer[Error]()
    var lfid    = ("", "", 0L) //(lastname, firstname, id)-Tuppel
    var clubId  = ("", 0L)     //(clubname, id)-Tuppel
    var email   = ""

    val bYear   = getInput(gE(uc("Year")), 0)
    val pStatus = PantStatus(getRadioBtn("PantStatus", PantStatus.UNKN.code))
    val gender  = SexTyp(getRadioBtn("Gender", SexTyp.UNKN.id)) 

    debug("validate", s"status: ${pStatus}")

    Player.validateEmail(getInput(gE(uc("Email")), "")) match {
      case Left(err)  => eList += err
      case Right(res) => email = res
    }

    Player.validateName(getInput(gE(uc("Name")), "")) match {
      case Left(err)  => eList += err
      case Right(res) => lfid = res
    }
    
    Club.validateName(getInput(gE(uc("Club")), "")) match {
      case Left(err)  => eList += err
      case Right(res) => clubId = res
    }

    val player = if (lfid._3 > 0) {
      trny.players(lfid._3)
    } else {
      val pl = Player(0L, clubId._2, clubId._1, lfid._2, lfid._1, bYear, email, gender, "") 
      pl.setTTR(getInput(gE(uc("TTR")), "")) 
        // check if player already exists ...
      if (trny.player2id.isDefinedAt(pl.hash)) eList += Error("err0162.Player.already.exists")
      pl 
    }

    /* read relevant input and verify it */
    if (eList.length > 0) Left(eList.toList) else Right((player, pStatus))
  } // end validate


  def setPlayerView(name: String, firstname: String, plId: Long)(implicit trny: Tourney): Unit = {
    if (trny.players.isDefinedAt(plId) & plId != 0L) {
      val player = trny.players(plId)
      setInput("Name", player.getName(1))
      setInput("Club", player.getClub(1))
      setInput("Email", player.email)
      setInput("TTR", player.getTTR)
      setIntOption("Year", player.getBirthyear())
      setRadioBtnByValue(uc("Gender"), player.sex.toString)
      disablePlayer(true)
    } else {
      if (name == "") {
        setInput("Club", "")
        setInput("Email", "")
        setInput("TTR", "")
        setIntOption("Year", None)
        setRadioBtnByValue(uc("Gender"), "0")
      }
      disablePlayer(false)
    }
  }

  def disablePlayer(value: Boolean): Unit = {
    setDisabled("Club", value)
    setDisabled("Email", value)
    setDisabled("TTR", value)
    setDisabled("Year", value)
    setDisabledByName("Gender", value)
  }

  // init dialog view
  def init(coId: Long)(implicit trny: Tourney): Unit = {
    
    def setNameList(name: String, trny: Tourney, coId: Long, setId: Long): Unit = {
      val nameList = (for (id <- freePlayers(trny, coId, setId)) yield s"""<option value="${trny.players(id).getName(1)}">${trny.players(id).getClub(0)}</option>""").mkString(" ") 
      setHtml(gUE(name), nameList)
    }

    def setClubList(trny: Tourney): Unit = {
      val clubList = (for ((id, c) <- trny.clubs) yield s"""<option>${c.getName(0)}</option>""").mkString(" ") 
      setHtml(gUE("ClubList"), clubList)    
    }

    setNameList("NameList", trny, coId, 0L)
    setClubList(trny)
    setPlayerView("","", 0L)(trny)

    setRadioBtnByValue(uc("PantStatus"), PantStatus.REDY.code)
    setHtml(gUE("Class"), trny.getCompName(coId))
  }
  
  // show dialog return result (Future) or Failure when canceled
  def show(coId: Long, trny: Tourney, lang: String): Future[Either[Error, (Player, PantStatus.Value)]] = {
    val p     = Promise[(Player, PantStatus.Value)]()
    val f     = p.future

    def cancel() = {
      offEvents(gE(uc("Modal")), "hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = 
      validate(coId) match {
        case Left(eList) => DlgShowError.show(eList); {}
        case Right(result)   => {
          if (!p.isCompleted) p success result
          //disable modal first, then hide
          offEvents(gE(uc("Modal")), "hide.bs.modal")
          doModal(gE(uc("Modal")), "hide")
        }  
      }

    
    tourney = trny
    loadModal(html.DlgCardRegSingle(), ucp)
    init(coId)

    // register routines for cancel and submit
    onEvents(gUE("Modal"), "hide.bs.modal", () => cancel())
    onClick(gUE("Submit"), (e: Event) => submit(e))
    doModal(gUE("Modal"), "show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}