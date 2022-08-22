package scalajs.usecase.organize

// Start TestCases in Javascript Console
// Start.testOrgCompDraw("<toId>")

import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

import shared.model._
import shared.model.CompPhase._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Organize Competition Tab/Register
// ***
@JSExportTopLevel("OrganizeCompetitionTab")
object OrganizeCompetitionTab extends UseCase("OrganizeCompetitionTab")  
  with TourneySvc with DrawSvc
{
  import org.scalajs.dom.raw.HTMLElement
  import org.scalajs.dom.raw.HTMLTableElement
  import scala.collection.mutable.ListBuffer

  var tabMode: String = ""

  def render(param: String = "", ucInfo: String = "", update: Boolean=false) = {
    tabMode = param
    
    val coId = AppEnv.getCoId
    val coPhNameIds = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
    debug("render", s"coId: ${coId} param: ${param}")

    // generate cards for competition phase if a competition is selected
    // and competition phases are available
    if (coId > 0 & coPhNameIds.length > 0) { 
      if (!exists(s"Content")) setMainContent(clientviews.organize.competition.html.TabCard(coId, coPhNameIds))
      // at least one competition phase is configured
      var coPhId = AppEnv.coPhIdMap.getOrElse(coId, coPhNameIds.head._2)
      selectCoPh(coId, coPhId, tabMode, update)
    } else {
      setMainContent(showAlert(getMsg("noSelection"))) 
    }
  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    import shared.utils.Routines._ 
    import org.scalajs.dom.document
    
    // import org.scalajs.dom.raw.HTMLCollection
    // import org.scalajs.dom.raw.NodeList
    //debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    debug("actionEvent", s"key: ${key}")
    key match {

      case "DrawRefresh"   => {
        val coId  = elem.getAttribute("data-coId")
        val coPhId = elem.getAttribute("data-coPhId")

        val inputNodes = getElemById_(s"Draw_${coId}_${coPhId}").querySelectorAll("small[data-drawPos]")
        val result = for( i <- 0 to inputNodes.length-1) yield {
          val elem   = inputNodes.item(i).asInstanceOf[HTMLElement]
          val posOld = elem.getAttribute("data-drawPos").toIntOption.getOrElse(-1)
          val sno    = elem.getAttribute("data-sno")
          val posNew = elem.innerHTML.toIntOption.getOrElse(-2)
          debug("actionEvent", s"DrawRefresh: posOld: ${posOld} posNew: ${posNew}  sno: ${sno}  ")
          (posOld, posNew, (posOld -> posNew))
        } 
        val orgList = result.map(_._1)
        val newList = result.map(_._2)
        val reassign = result.map(_._3).toMap

        val xx = orgList.filterNot(newList.contains)
        if (xx.length > 0) error("DrawRefresh", s"keine Zuordnung: ${xx}")
        debug("DrawRefresh", s"orgList: ${orgList}  newList: ${newList} reassign: ${reassign}")
      } 

      case "Demo"   => {
        val sElem = getElemById("Links").querySelector(".active").asInstanceOf[HTMLElement]
        val (coId,coPhId) = (getData(sElem,"coId",0L), getData(sElem,"coPhId",0))
        var cnt = 0
        App.tourney.cophs((coId,coPhId)).matches.foreach { m =>
          if (m.status != MEntry.MS_FIN & m.status != MEntry.MS_FIX) {
            enterDemoResult(coId, coPhId, m.gameNo, m.winSets, cnt*600)
            cnt = cnt+1
          }
        }
      } 
      case "Reset"   => {
        debug("Reset", s"Reset Button press")
      } 
      case "Start"   => {
        val sElem = getElemById("Links").querySelector(".active").asInstanceOf[HTMLElement]
        val (coId,coPhId) = (getData(sElem,"coId",0L), getData(sElem,"coPhId",0))
        debug("START", s"coId: ${coId} coPhId: ${coPhId}")
        App.tourney.cophs((coId,coPhId)).setStatus(CPS_EIN)
        App.execUseCase("OrganizeCompetitionInput", "", "")

      }       

      case "SelectCoPh"  => selectCoPh(getData(elem, "coId", 0L), getData(elem, "coPhId", 0), tabMode)

      case _             => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }


  // select a competition phase e.g. click on tab  
  def selectCoPh(coId: Long, coPhId: Int, mode: String, update:Boolean=false) = {
    val aNodes = getElemById("Links").getElementsByTagName("a")

    // control tab buttons
    App.tourney.cophs(coId, coPhId).getStatus match {
      case CPS_AUS => setVisible("BtnDemo", false);             setVisible("BtnStart", chkMode(mode,"ID")); setVisible("BtnReset", false)
      case CPS_EIN => setVisible("BtnDemo", chkMode(mode,"I")); setVisible("BtnStart", false);              setVisible("BtnReset", chkMode(mode,"ID"))
      case _       => setVisible("BtnDemo", false);             setVisible("BtnStart", false);              setVisible("BtnReset", chkMode(mode,"ID"))
    }
    
    // set register/tab active
    for( i <- 0 to aNodes.length-1) {
      if (aNodes.item(i).asInstanceOf[HTMLElement].getAttribute("data-coPhId") == coPhId.toString) {
        aNodes.item(i).asInstanceOf[HTMLElement].classList.add("active")
      } else {
        aNodes.item(i).asInstanceOf[HTMLElement].classList.remove("active")
      }
    }  

    // remember selection
    AppEnv.coPhIdMap(coId) = coPhId
    implicit val trny = App.tourney
    mode match {
      case "Draw"  => if (update) OrganizeCompetitionDraw.update(coId, coPhId) else OrganizeCompetitionDraw.init(coId, coPhId)
      case "Input" => if (update) OrganizeCompetitionInput.update(coId, coPhId) else OrganizeCompetitionInput.init(coId, coPhId)
      case "View"  => if (update) OrganizeCompetitionView.update(coId, coPhId) else OrganizeCompetitionView.init(coId, coPhId)
    }  

    // set relevant section visible
    val contentNodes = getElemById("Content").getElementsByTagName("section")
    for( i <- 0 to contentNodes.length-1) {
      val elem = contentNodes.item(i).asInstanceOf[HTMLElement]
      elem.style.display = if (getData(elem, "coPhId", 0) == coPhId & 
                               getData(elem, "tabMode", "") == mode) "block" else "none"
    }
  }

  def chkMode(viewMode: String, chk: String): Boolean = {
    viewMode match {
      case "Draw"  => chk.contains("D")
      case "View"  => chk.contains("V")
      case "Input" => chk.contains("I") 
      case _       => false
    }
  }


  // enterDemoResult - generate demo result and put it into the input fields and
  //                   press enter button, first enter table entry which marks 
  //                   the beginning of the match after random time (2-4 seconds)
  //                   enter result
  def enterDemoResult(coId: Long, coPhId: Int, matchNo: Int, noWinSets: Int, offsetMS: Int) = {
    import scala.collection.mutable.ArrayBuffer
    var balls = ArrayBuffer[String]()
    var ballElems = ArrayBuffer[HTMLElement]()

    println(s"enterDemoResult match: ${matchNo}")
    val r = scala.util.Random
    val saveBtn = getElemById_(s"SaveBtn_${coId}_${coPhId}_${matchNo}").asInstanceOf[HTMLElement]
    val pfElem = getElemById_(s"Playfield_${coId}_${coPhId}_${matchNo}").asInstanceOf[HTMLElement]
    val result = (for (i<-0 until (noWinSets*2)+1) yield {r.nextInt(2)}).toArray
    val (setA, setB) = result.foldLeft((0,0)) {(t,v) => if (t._1 == noWinSets | t._2 == noWinSets) (t._1, t._2) else (t._1 + v, t._2 + (v^1)) }     
    
    for (i<-0 until (setA+setB)) {
      val ball = if (result(i)>0 ) s"${r.nextInt(15)}"  else s"-${r.nextInt(15)}" 
      balls += ball
      ballElems += getElemById_(s"Input_${coId}_${coPhId}").querySelector(s"[data-game_${matchNo}='ball_${i+1}']").asInstanceOf[HTMLElement]
    }
    // first set playfield to indicate running game
    dom.window.setTimeout(() => { pfElem.innerText = s"${r.nextInt(9)+1}"; clickButton(saveBtn) }, offsetMS)

    // later enter result, to finisch game
    val playtime = offsetMS + 1000*(r.nextInt(3)+1)
    for (i<-0 until (setA+setB)) {
      dom.window.setTimeout(() => { ballElems(i).innerText = balls(i); if (i == setA+setB-1) clickButton(saveBtn) }, playtime + i*100)
    }
  }

  def clickButton(btn: HTMLElement) = {
    val bgC = btn.style.backgroundColor
    btn.style.backgroundColor="#7F7F7F"
    dom.window.setTimeout(() => { btn.style.backgroundColor=bgC }, 100)
    btn.click()
  }
  

}
