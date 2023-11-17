package scalajs.usecase.info

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure}

import shared.model.{ TournBase, Player, PantStatus, CompTyp, SexTyp }
import shared.utils.Routines._
import shared.utils.Constants._ 
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// User Info View Schedule
// ***  
@JSExportTopLevel("InfoSchedule")
object InfoSchedule extends UseCase("InfoSchedule")  
  with TourneySvc
{

  /** render
    * 
    * @param param
    * @param ucInfo
    * @param reload
    */
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    val lang = AppEnv.getLang
    debug("render", s" Start: ${App.tourney.startDate} End: ${App.tourney.endDate}" )

    // call string for date
    val toDate = if (App.tourney.startDate == App.tourney.endDate) {
      int2date(App.tourney.startDate, lang)
    } else {
      getMsg("date.range", int2date(App.tourney.startDate, lang), int2date(App.tourney.endDate, lang))
    }

    val coSeq = (for {
      (k,c)   <- App.tourney.comps
      players = App.tourney.pl2co.toSeq.filter(_._2.coId == c.id)
    } yield {
      val cnt = c.typ match {
        case CompTyp.SINGLE | CompTyp.DOUBLE => players.filter(_._2.status >= PantStatus.REGI).length
        case _ => 0
      }
      val cntVal = if (cnt > 0) cnt.toString else ""
      (c.id, c.name, c.getAgeGroup, c.getRatingRemark, c.typ, c.formatTime(lang, 2), c.status, c.genRange, cntVal, c.formatTime(lang, 1))
    }).toSeq.sortWith(_._5 > _._5).sortWith(_._10 < _._10).map(r => (r._1, r._2, r._5, r._6, r._8, r._9, r._7))


    setMainContent(clientviews.info.html.ScheduleTmpl(
      App.tourney.name, toDate, App.tourney.orgDir, App.tourney.organizer, coSeq).toString
    )
   
    setVisible("DoublePlayer", false)
    setVisible("Help", false)

    // init sign up card
    var date  = new js.Date()
    val cYear = date.getFullYear().toInt
    val plYears = clientviews.component.html.OptionListYear(cYear-4, cYear-100, getMsg("placeholder.year")).toString

    setHtml("PlayerYear", plYears)
    setHtml("PlayerYear2", plYears)

    // check if there is a competition with allows registering users
    setVisible("RegisterCard", App.tourney.comps.toSeq.filter(_._2.getWebRegister).length > 0)

    // tourneyinfo hints
    getInvitation.map {
      case Left(value)     => addClass(gE(uc("HintCard")), "hidden-xl-down")
      case Right(content)  => {
        removeClass(gE(uc("HintCard")), "hidden-xl-down")
        setHtml("HintBodyContent", s"""<article class="markdown-body">$content</article>""") 
      }  
    }
  } 


  @JSExport
  def reset(): Boolean = {
    debug("reset", "register")
    setAttribute(gE(uc("SignUp")),"disabled", "disabled")
    setCheckbox("CheckConsent", false)
    false
  }  

  
  def showHelpSchedule(msg: String="", visible: Boolean=false) = {
    setHtml("HelpText", msg)
    setVisible("Help", visible)
  }


  /**
   * verify schedule input form
   */
  @JSExport
  def verifyComp(): Int = {
    val coSel = getInput(gE(uc("Competition")),"").split("__") match {
      case Array(s1,s2) => (s1,s2) 
    }
    showHelpSchedule()
    coSel._2 match {
      case "single" | "einzel" => setVisible("DoublePlayer",false); 1
      case "double" | "doppel" => setVisible("DoublePlayer",true); 2
      case _                   => {
        showHelpSchedule(getMsg("signup.invalidComp"),true)
        setVisible("DoublePlayer",false)
        0
      }  
    }
  }    


  @JSExport
  def verifyEmail(email: String): Boolean = {
    import shared.utils.Routines._
    if ( email != "" && validEmail(email) ) {
      setHtmlVisible(s"HelpText", false)
      setVisible("Help", false)
      true
    } else {
      setHtmlVisible(s"HelpText", true, getMsg("app.emailhelp") )
      setVisible("Help", true)
      false
    }
  }


  def verifyName(lname: String): Boolean = {
    showHelpSchedule(getMsg(s"signup.invalidname"), lname == "")
    lname != ""
  }

  @JSExport
  def onclickCheckConsent(elem: dom.raw.HTMLInputElement) = {
    if (elem.checked) removeAttribute(gE(uc("SignUp")), "disabled") else setAttribute(gE(uc("SignUp")),"disabled","disabled")
  } 

  @JSExport
  def buttonSignUp(): Unit = {
    
    def doRegSPlayer(coId: Long, player: Player) = {
      import shared.model.PantStatus
      if (verifyEmail(player.email) && verifyName(player.lastname)) {
        regSingle(coId, player, PantStatus.REGI).map {
          case Left(err)  => showResult(true, getMsg("signup.ok.error"), getMsg("signup.error"), "danger")
          case Right(res) => showResult(true, getMsg("signup.ok.header"), getMsg("signup.ok"), "success")
        }
      }
    }

    def doRegDPlayer(coId: Long, pl1: Player, pl2: Player) {
        regDouble(coId, pl1, pl2).map {
          case Left(err)  => showResult(true, getMsg("signup.ok.error"), getMsg("signup.error"), "danger")
          case Right(res) => showResult(true, getMsg("signup.ok.header"), getMsg("signup.ok"), "success")
        }
    }

    // hide help
    showHelpSchedule()
    val (lastname, firstname) = getInput(gE(uc("PlayerName")),"").split(",")  match { 
      case Array(s1,s2) => (s1.trim,s2.trim)
      case _            => ("","") 
    }
    val pl = Player(0L,"",0L,getInput(gE(uc("PlayerClub"))),firstname,lastname,
                    getInput(gE(uc("PlayerYear")), 0), getInput(gE(uc("PlayerEmail"))), SexTyp.UNKN, "_")
    pl.setTTR(getInput(gE(uc("PlayerTTR"))))

    val (coId, cTyp) = getInput(gE(uc("Competition")),"").split("__") match {
      case Array(s1,s2) => (s1.toLong, s2) 
      case _            => (0L,0) 
    }
    
    verifyComp match {
      case 1 => doRegSPlayer(coId, pl)
      case 2 => {
        val (lastname2, firstname2) = getInput(gE(uc("PlayerName2"))).split(",") match { case Array(s1,s2) => (s1.trim,s2.trim); case _ => ("","") }
        val pl2 = Player(0L,"",0L,getInput(gE(uc("PlayerClub2"))), firstname2, lastname2, getInput(gE(uc("PlayerYear2")), 0), "", SexTyp.UNKN, "_")
        pl2.setTTR(getInput(gE(uc("PlayerTTR2"))))
        doRegDPlayer(coId, pl, pl2)
      }
      case _ =>  debug("buttonSignUp", "unknown competition type")
    }
  }    

 /** getInvitation          read invitation markdown document from server
   *                        format to html 
   */
  def getInvitation(): Future[Either[Error, String]] = {
    val converter = new Converter()
    // debug(s"getInvitation", "started")
    getInvitation(App.tourney.orgDir, App.tourney.startDate).map {
      case Left(err)   =>  Left(err)
      case Right(content)  => {
        // convert markdown to html, hide metadata
        converter.setOption("metadata", true)
        converter.setOption("tables", true) 
        Right(converter.makeHtml(content).toString)
      }
    }
  }
  
   
}