package scalajs

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"

import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

import scalajs.usecase.component._
import scalajs.service._
import shared.model._
import shared.model.Utility._
import cats.syntax.validated
import play.twirl.api.HtmlFormat

// http://localhost:9000/svc/getReferee/185/1/1/1/624

@JSExportTopLevel("Referee")
object Referee extends BasicHtml with TourneySvc
{
  var winSets = 0
  var refNote:RefereeNote = null

  /** referee - entry point for referee notes
   *
   * @param name       - 
   * @param gameNo     -
   * @param language   - 2chars encoded language
   * @param csrfToken  - from server
   */  
  @JSExport
  def start(refNoteJson: String, wSets: String, lang: String, csrfToken: String): Unit = { 
    AppEnv.csrf = csrfToken
    App.tourney = Tourney.init
    winSets = wSets.toIntOption.getOrElse(0)
    initMessages.map { _ => {

      AppEnv.setDebugLevel(getOrDefault(AppEnv.getLocalStorage("AppEnv.LogLevel"), gM("config.LogLevel")))
      println(s"Startup referee name:${refNote} winSets: ${winSets} lang:${lang}")

      AppEnv.home = dom.window.location.protocol + "//" + dom.window.location.host

      try refNote = read[RefereeNote](refNoteJson.replace("&quot;","\""))
      catch { case _: Throwable => errLog(s"Couldn't decode RefereeNote: ${refNoteJson}") }
    
      setHtml(gE(s"Referee_PlayerA"), s"${refNote.playerA} [${refNote.clubA}]")
      setHtml(gE(s"Referee_PlayerB"), s"${refNote.playerB} [${refNote.clubB}]")

      setHtml(gE(s"Referee_TourneyName"), refNote.trnyName)
      setHtml(gE(s"Referee_CompName"), refNote.compName)
      setHtml(gE(s"Referee_PhaseMatchNoName"), s"${refNote.phaseName} - ${gM("referee.gameNo",refNote.gameNo.toString)}")

      if (refNote.finished) {
        refNote.sets.zipWithIndex.foreach{ case (s, idx) => 
          setInput(gE(s"Referee_Set${idx+1}A"), s"${s._1}")
          setInput(gE(s"Referee_Set${idx+1}B"), s"${s._2}")
        }
        setHtml(gE(s"Referee_SetA"), refNote.getSets._1)
        setHtml(gE(s"Referee_SetB"), refNote.getSets._2)
      }
    }}
  }

 /** referee - submit
   *
   * @param elem       - this
   * 
   */  
  @JSExport
  def submit(elem: dom.raw.HTMLElement): Unit = { 
    import scalajs.usecase.dialog.DlgBox
    import scala.collection.mutable.ListBuffer

    val winSets = getData(elem, "winSets", 0)
    val balls = new ListBuffer[(Int,Int)]() 
    var sets = (0,0)

    // read referee note - result should be valid
    for(i<-1 to (winSets*2)+1) { 
      val ballA = getBall(i, "A")
      val ballB = getBall(i, "B")
      if (ballA >= 0 && ballB >= 0 & sets._1 != winSets && sets._2 != winSets) {
        balls.append((ballA, ballB))  
        sets = sets + getSets((ballA, ballB))
      }
    }

    var result = (balls.foldLeft("")( (x,y) => if (y._1 > y._2) s"${x}·${y._2}" else s"${x}·-${y._1}")).stripPrefix("·")
    val msgHdr     = gM("referee.dlg.hdr.confirm")
    val msgContent = gM("referee.dlg.result.message", refNote.playerA, refNote.playerB, s"${sets._1}", s"${sets._2}", s"${result.replace('·',' ')}")
    
    val dlgFinished = dlgCancelOk(msgHdr, msgContent) {
      //println(s"Input Match CONFIRMED with result: '${result}' ")
      inputReferee(refNote.toId, refNote.coId, refNote.coPhId, refNote.gameNo, sets, result, "ResultByPlayer", "").map {
        case Left(err)    => AppEnv.error("inputReferee", s"${err.toString}")
        case Right(gList) => {
          setHtml(gE(s"Referee_SetA"), sets._1)
          setHtml(gE(s"Referee_SetB"), sets._2)
          setVisible(gE(s"Referee_Buttons"), false)
          setVisible(gE(s"Referee_Registered"), true)
        }
      }
    }
  } 

  @JSExport
  def change(elem: dom.raw.HTMLElement): Unit = { 

    val winSets = getData(elem, "winSets", 0)
    var sets = (0,0)

    setDisabled(gE("Referee_BtnSubmit"), true)
    for(i<-1 to (winSets*2)+1) { 
      rmMark(i, "A")
      rmMark(i, "B")

      val balls = (getBall(i, "A"), getBall(i, "B"))   
      
      // check we have a complete result
      if (sets._1 != winSets && sets._2 != winSets) { 

        val result = getSets(balls)
        if (result._1 == 1 | result._2 == 1) {
          sets = sets + result
          // mark good input
          setSuccess(i,"A")
          setSuccess(i,"B")
          // result complete ?
          if ((sets._1 == winSets || sets._2 == winSets) & 
              (sets._1 + sets._2 == i)) setDisabled(gE("Referee_BtnSubmit"), false)
          
          // check for empty or wrong entry in the beginning
          if ((sets._1 == winSets || sets._2 == winSets) & (sets._1 + sets._2 < i)) {
            for (j <- 1 to i-1) {
              if (isEmpty(j, "A")) setDanger(j, "A")
              if (isEmpty(j, "B")) setDanger(j, "B")             
            }
          }
        } else {
          // result already complete, so mark wrong input
          if (balls._1 < 0 & !isEmpty(i, "A")) setDanger(i, "A")
          if (balls._2 < 0 & !isEmpty(i, "B")) setDanger(i, "B")
          if (balls._1 >= 0 & balls._2 >= 0 ) { setDanger(i,"A"); setDanger(i,"B") }
        }
      } else { 
        // everthing else should be empty now
        if (!isEmpty(i,"A") | !isEmpty(i,"B")) setDisabled(gE("Referee_BtnSubmit"), true)          
        if (!isEmpty(i,"A")) setDanger(i,"A")
        if (!isEmpty(i,"B")) setDanger(i,"B")
      }  
    }
  }  

  @JSExport
  def cancel(elem: dom.raw.HTMLElement): Unit = dom.window.location.href = s"${AppEnv.home}"

  def initMessages(): Future[Boolean] = {
    val localMessages = AppEnv.getLocalStorage("AppEnv.Messages")

    if (localMessages == "" | (AppEnv.getLocalStorage("AppEnv.LogLevel") == "debug")) {
      Ajax.get("/getMessages").map(_.responseText).map(content => {
        AppEnv.setLocalStorage("AppEnv.Messages", content)

        AppEnv.messages = read[ Map[String,Map[String,String]]] (content)
        //println(s"loadMessages remote messages")
        true
      })
    } else {
      AppEnv.messages = read[ Map[String,Map[String,String]]] (localMessages)
      //println(s"loadMessages local messages")
      Future(true)
    }
  }

  def getOrDefault(value: String, defValue: => String): String = if (value == "") defValue else value

  def isEmpty(pos: Int, ab: String)    = (getInput(gE(s"Referee_Set${pos}${ab}")).trim == "")
  def setDanger(pos: Int, ab: String)  = addClass(gE(s"Referee_Set${pos}${ab}"), "border", "border-danger")
  def setSuccess(pos: Int, ab: String) = addClass(gE(s"Referee_Set${pos}${ab}"), "border", "border-success")
  def rmMark(pos: Int, ab: String)     = removeClass(gE(s"Referee_Set${pos}${ab}"), "border", "border-danger", "border-success")
  def getBall(pos: Int, ab: String)    = getInput(gE(s"Referee_Set${pos}${ab}"), -1)

}