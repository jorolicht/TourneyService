package scalajs.usecase.dialog

// Start TestCases
// DlgCardPlayer: http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardPlayer&ucInfo=View
//                http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardPlayer&ucInfo=New
//                http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardPlayer&ucInfo=Edit

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
import shared.model.{ Tourney, Player }
import shared.utils._
import shared.utils.Constants._
import clientviews.dialog.html


@JSExportTopLevel("DlgCardPlayer")
object DlgCardPlayer extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp     = UseCaseParam("APP__DlgCardPlayer", "dlg.card.player", "DlgCardPlayer", "dlgcardplayer", scalajs.AppEnv.getMessage _ )
  implicit var tourney = Tourney.init
  private def load()   = if (!checkId("Modal")) insertHtml_("APP__Load", "afterbegin", html.DlgCardPlayer().toString)
 
  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "Name" => { 
        debug("actionEvent", s"Input value: ${elem.asInstanceOf[Input].value}")

        Player.parseName(elem.asInstanceOf[Input].value) match {
          case Left(err)   => debug("parseName", s"error: ${err}")
          case Right(lfid) => setPlayer(lfid._3)
        }

        // debug("actionEvent", s"key: ${key} event: ${event.`type`}")
        // val x = getRadioBtn("Gender")
        // debug("actionEvent", s"Radio: ${x}")
      }
      case "SelectComp" => { 
        debug("actionEvent", s"key: ${key} event: ${event.`type`}")
      }      
      case "Close"   => { $(getId("Modal","#")).off("hide.bs.modal");  $(getId("Modal","#")).modal("hide") }
      case _         => {}
    }
  }

  /** validate input of dialog for Player, return valid Player or a List of Errors
   * 
   */ 
  def validate(mode: DlgOption.Value): Either[List[Error], Player] = {
    import shared.model.Player
    import shared.utils.Routines._
    import scala.collection.mutable.ListBuffer
    
    var eList = ListBuffer[Error]()

    val id        = getData("Form", "id", 0L)
    val rid       = getData("Form", "rid", 0)
    val options   = getData("Form", "options", "")
    var lfid      = ("", "", 0L) //(lastname, firstname, id)-Tuppel
    var email     = ""

    val bYear = getInput("Year", 0)
    val club  = getInput("Club", "")
    val ttr   = getInput("TTR", "")

    Player.parseEmail(getInput("Email", "")) match {
      case Left(err)  => eList += err
      case Right(res) => email = res
    }

    Player.parseName(getInput("Name", "")) match {
      case Left(err)  => eList += err
      case Right(res) => lfid = res
    }
    
    var inValue = Player(11l,12,33L,"xxx","Rob","Licht",1963,"xx@xx", 0," ")

    /* read relevant input and verify it */
    if (eList.length > 0) Left(eList.toList) else Right(inValue)
  }


  def setPlayer(plId: Long, inVisible: Boolean=true)(implicit trny: Tourney): Unit = {

    val player = if (plId != 0) trny.players(plId) else Player.init

    // set the radio button for gender
    def setGender(gender: Int) = gender match {
      case 1 => { setRadioBtn("GenderFemale", true);  setRadioBtn("GenderMale", false); setRadioBtn("GenderNone", false) }
      case 2 => { setRadioBtn("GenderFemale", false); setRadioBtn("GenderMale", true);  setRadioBtn("GenderNone", false) }
      case _ => { setRadioBtn("GenderFemale", false); setRadioBtn("GenderMale", false); setRadioBtn("GenderNone", true)  }
    }

    // setting invisible data
    if (inVisible) {
      setData("Form", "id", player.id)
      setData("Form", "rid", player.rid)
      setData("Form", "options", player.options)
    }      
    // setting visible data
    setInput("Name", player.getName(1))
    setInput("Club", player.getClub(1))
    setInput("Email", player.email)
    setInput("TTR", player.getTTR)
    setIntOption("Year", player.getBirthyear())
    setGender(player.sex)
  }

  // set player view
  def set(plId: Long, coId: Long, trny: Tourney, mode: DlgOption.Value): Unit = {
    
    def setNameList(trny: Tourney): Unit = {
      val nameList = (for ((id, p) <- trny.players) yield s"""<option value="${p.getName(1)}">${p.getClub(0)}</option>""").mkString(" ") 
      setHtml("NameList", nameList)
    }
  
    def setClubList(trny: Tourney): Unit = {
      val clubList = (for ((id, c) <- trny.clubs) yield s"""<option>${c.getName(1)}</option>""").mkString(" ") 
      setHtml("ClubList", clubList)    
    }

    tourney = trny
    setNameList(trny)
    setClubList(trny)
    setHtml("Class", trny.getCompName(coId))
    val container = document.querySelector(getIdHa("Form"))
    val cFooter   = document.querySelector(getIdHa("FormFooter"))

    mode match {
      case DlgOption.View => { 
        setVisible("Cancel", false); setVisible("Submit", false); setVisible("Close", true)
        setPlayer(plId)
        container.querySelectorAll("input, select").map(_.asInstanceOf[Input].disabled = true)
        cFooter.querySelectorAll("input, select").map(_.asInstanceOf[Input].disabled = true)
      } 
      case DlgOption.Edit => { 
        setVisible("Cancel", true);  setVisible("Submit", true);  setVisible("Close", false)
        setPlayer(plId)
        container.querySelectorAll("input, select").map(_.asInstanceOf[Input].disabled = false)
        cFooter.querySelectorAll("input, select").map(_.asInstanceOf[Input].disabled = false)
      } 
      case DlgOption.New => { 
        setVisible("Cancel", true);  setVisible("Submit", true);  setVisible("Close", false)
        setPlayer(0L)
        container.querySelectorAll("input, select").map(_.asInstanceOf[Input].disabled = false)
        cFooter.querySelectorAll("input, select").map(_.asInstanceOf[Input].disabled = false)
      } 
    }
  }
  
  // show dialog return result (Future) or Failure when canceled
  def show(plId: Long, coId: Long, trny: Tourney,  lang: String, mode: DlgOption.Value): Future[Either[Error, Player]] = {
    val p     = Promise[Player]()
    val f     = p.future

    def cancel() = {
      $(getId("Modal","#")).off("hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event) {
      validate(mode) match {
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
    set(plId, coId, trny, mode)

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event)     => submit(e)) 
    $(getId("Modal","#")).modal("show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }  
}