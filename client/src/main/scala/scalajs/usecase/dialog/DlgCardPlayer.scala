package scalajs.usecase.dialog

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Success, Failure }
import scala.util.matching
import scala.concurrent._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.scalajs.dom.document
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.Event

import scalajs.usecase.dialog.DlgBox
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.model.{ Tourney, Player, Club, SexTyp, CttPersonCsv }
import shared.utils._
import shared.utils.Routines._
import clientviews.dialog.html


@JSExportTopLevel("DlgCardPlayer")
object DlgCardPlayer extends BasicHtml 
  with TourneySvc
{
  this: BasicHtml =>
  implicit val ucp     = UseCaseParam("APP__DlgCardPlayer", "dlg.card.player", "DlgCardPlayer", "dlgcardplayer", scalajs.AppEnv.getMessage _ )
  implicit var tourney = Tourney.init

  var player = Player(0L, "", 0L, "", "", "", 0, "", SexTyp.UNKN)

  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "Year" => {
        player.birthyear = getInput(elem, 0)
        debug("actionEvent", s"year ${player.birthyear}") 
      }  

      case "Club" => Club.validateName(getInput(elem, "")) match {
        case Left(err)  => {
          markInput(elem, Some(true))
          setDisabled("Submit", true)
        }  
        case Right(res) => { 
          markInput(elem, None)
          setDisabled("Submit", false)
          player.clubName = res._1
          player.clubId   = res._2 
        }
      }

      case "TTR" => Player.validateTTR(getInput(elem, ""), tourney.getPlayerRatingRange()) match {
        case Left(err)  => markInput(elem, Some(true)); setDisabled("Submit", true)
        case Right(res) => markInput(elem, None); setDisabled("Submit", false); player.setTTR(res)      
      }

      case "Name" => Player.validateName(getInput(elem, "")) match {
        case Left(err)  => markInput(elem, Some(true)); setDisabled("Submit", true)
        case Right(res) => markInput(elem, None); setDisabled("Submit", false); player.lastname = res._1; player.firstname = res._2   
      }

      case "Email" => Player.validateEmail(getInput(elem, "")) match {
        case Left(err)  => markInput(elem, Some(true)); setDisabled("Submit", true)
        case Right(res) => markInput(elem, None); setDisabled("Submit", false); player.email = res 
      }
      
      case "Gender" => player.sex = SexTyp(getInput(elem, 0))

      case "DeleteLicense" => {
        DlgBox.standard(getMsg("delete.hdr"),getMsg("delete.msg", getInput("License", ""), player.getName(0)), Seq("cancel", "yes"), 0, true) map {
          case 2 => player.delLicense; setPlayerView(player, tourney) 
          case _ => debug("actionEvent", s"abort deletion of license")  
        }
      }
    
      case "ChangeLicense" => {
        val cttLicInfo = CttPersonCsv(tourney.licenses.getOrElse(getInput("License", ""),"")) 
        cttLicInfo.get match {
          case Left(err)  => debug("actionEvent", s"Change License ${err}")
          case Right(ctp) => player = ctp.toPlayer(player.id, 0L, player.email); setPlayerView(player, tourney)  
        }
      }

      case "Close"   => offEvents(gE("Modal", ucp), "hide.bs.modal"); doModal(gE("Modal", ucp), "hide")

      case _         => {}
    }
  }


  /** read player input of dialog for Player, return valid Player or a List of Errors
   * 
   */ 
  def validate(player: Player): Either[List[Error], Player] = {
    import scala.collection.mutable.ListBuffer
    
    var eList = ListBuffer[Error]()

    var lfid      = ("", "", 0L) //(lastname, firstname, id)-Tuppel
    var email     = ""

    val bYear = getInput("Year", 0)
    val club  = getInput("Club", "")
    

    Player.validateEmail(getInput("Email", "")) match {
      case Left(err)  => eList += err
      case Right(res) => email = res
    }

    Player.validateName(getInput("Name", "")) match {
      case Left(err)  => eList += err
      case Right(res) => lfid = res
    }

    /* read relevant input and verify it */
    if (eList.length > 0) {
      Left(eList.toList)
    } else {
      Right(player)
    }  
  }

  def setPlayerView(player: Player, trny: Tourney): Unit = {
    def getLicInfo(enc: String) =  getMsg("licInfo", getMDStr(enc,1), getMDStr(enc,0), getMDStr(enc,2), getMDStr(enc,3), getMDStr(enc,4))

    // set the radio button for gender
    def setGender(gender: SexTyp.Value) = gender match {
      case SexTyp.FEMALE => { setRadioBtn("GenderFemale", true);  setRadioBtn("GenderMale", false); setRadioBtn("GenderNone", false) }
      case SexTyp.MALE   => { setRadioBtn("GenderFemale", false); setRadioBtn("GenderMale", true);  setRadioBtn("GenderNone", false) }
      case SexTyp.UNKN   => { setRadioBtn("GenderFemale", false); setRadioBtn("GenderMale", false); setRadioBtn("GenderNone", true)  }
    }
    
    // setting visible data
    setHtml("Title", getMsg("Title", player.id.toString))
    setInput("Name", player.getName(0))
    setInput("Club", player.getClub(0))
    setHtml("ClubList", (for ((id, c) <- trny.clubs) yield s"""<option>${c.getName(1)}</option>""").mkString(" ") )  

    setInput("Email", player.email)
    setInput("TTR", player.getTTR)
    setIntOption("Year", player.getBirthyear())
    setGender(player.sex)


    val hasLicense = player.hasLicense
    setDisabled("Name", hasLicense)
    setDisabled("Club", hasLicense)
    setDisabled("TTR", hasLicense)
    setDisabled("Year", hasLicense)
    setDisabled("GenderFemale", hasLicense)
    setDisabled("GenderMale", hasLicense)
    setDisabled("GenderNone", hasLicense)

    // License control / set clickTT license list
    if (!hasLicense) {
      val liList = (for ((license, cttPlInfo) <- trny.licenses) yield (getLicInfo(cttPlInfo), license))
                   .toList.sortBy(_._1).map(x => s"""<option value='${x._2}'>${x._1}</option>""").mkString(" ")
      setHtml("LicenseList", liList)
    }

    setVisible("BtnDeleteLicense", hasLicense)
    setInput("License", if (hasLicense) player.getLicense.value else "")
    setDisabled("License", hasLicense)
  }


  // show dialog return result (Future) or Failure when canceled
  def show(plId: Long, trny: Tourney): Future[Either[Error, Player]] = {
    val p     = Promise[Player]()
    val f     = p.future

    def cancel() = {
      offEvents(gE("Modal", ucp), "hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event) {
      offEvents(gE("Modal", ucp), "hide.bs.modal")
      if (!p.isCompleted) p success player
      doModal(gE("Modal", ucp), "hide")
    }
    
    loadModal(html.DlgCardPlayer(), ucp)
    tourney = trny
    player  = trny.players(plId)
    setPlayerView(player, tourney)

    // register routines for cancel and submit
    onEvents(gE("Modal", ucp), "hide.bs.modal", () => cancel())
    onClick(gE("Submit", ucp), (e: Event) => submit(e))
    doModal(gE("Modal", ucp), "show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  } 
}