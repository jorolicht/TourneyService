package scalajs.usecase.organize

// Start TestCases in Javascript Console
// Start.testOrgCompInput("182")

import scala.concurrent._
//import scala.collection.mutable.ListBuffer
import scala.util.{Success, Failure }
import scala.util.matching


import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

//import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.HTMLTableElement
import org.scalajs.dom.raw.NodeListOf
import org.scalajs.dom.raw.NodeList

import shared.model._
import shared.model.CompPhase._

import shared.utils._

import scalajs.usecase.component._
import scalajs.service._

import scalajs._

// Possible match result status
// - edit     (no result)   - activ: saveButten, inactiv: deleteButton, balls,sets unlocked
// - valid    (with result) - inactiv: saveButton, activ: deleteButton, balls,sets locked (grey)
// - fix      (with result) - not shown saveButton, deleteButton, balls,sets locked (grey)

// ***
// Organize Competition Inaput
// ***
@JSExportTopLevel("OrganizeCompetitionInput")
object OrganizeCompetitionInput extends UseCase("OrganizeCompetitionInput")  
  with TourneySvc with MatchSvc
{

  def render(param: String = "", ucInfo: String = "", update: Boolean=false) = {
    OrganizeCompetitionTab.render("Input")
  }

@JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    //import shared.utils.Routines._ 
    import org.scalajs.dom.document

    def getRow(coId: Long, coPhId: Int, game: Int):HTMLElement = {
      //val rowBase = getElemById_(s"Input_${coId}_${coPhId}").asInstanceOf[HTMLElement]
      getRowBase(coId, coPhId).querySelector(s"[data-game_${game}='row']").asInstanceOf[HTMLElement]    
    }  
    
    def getRowBase(coId: Long, coPhId: Int):HTMLElement = {
      getElemById_(s"Input_${coId}_${coPhId}").asInstanceOf[HTMLElement]   
    }
        
    val (coId, coPhId) = (getData(elem,"coId",0L), getData(elem,"coPhId",0))
    val action = if (App.tourney.cophs.contains((coId, coPhId))) {
      val status = App.tourney.cophs((coId, coPhId)).getStatus
      if (status == CompPhase.CPS_EIN || status == CompPhase.CPS_FIN) key else "BadRequest"
    } else {
      "BadRequest"
    }

    debug("actionEvent", s"key: ${key} -> ${action}")
    action match {

      case "SaveMatchResult"   => { 
        val coPhase = App.tourney.cophs((coId, coPhId))
        val game    = getData(elem, "game", 0)
        val rowBase = getRowBase(coId, coPhId)
        val row     = getRow(coId, coPhId, game)
        
        // get balls and/or sets
        // check values and if ok save input to local match (and server)
        
        val winSets = coPhase.noWinSets
        val bInput  = getInputBalls(row, winSets, game)
        val sInput  = getInputSets(row, winSets, game)
        
        val (inputOk, balls, sets, err) = bInput match {
          case Left(err)  => (false, "", (0,0), err)
          case Right(res) => if (res._2 != (0,0)) {
              (true,  res._1.mkString("·"), res._2, Error(""))
            } else sInput match {
              case Left(err)  => (false, "", (0,0), err)
              case Right(res) => (true, "", res, Error(""))
            }
        }
        if (inputOk) {
          val gameUpdateList = coPhase.setMatchPropagate(game, sets, balls, getInputInfo(row, game), getInputPlayfield(row, game))  
          for (game <- gameUpdateList) { setMatchView(rowBase, coId, coPhId, game)(coPhase) }
        } else {
          error("actionEvent", s"SaveMatchResult -> input not OK: ${err}")
        }
        debug("actionEvent", s"SaveMatchResult -> status: ${coPhase.getStatusTxt}")
      }
      
      case "DeleteMatchResult"   => {
        val coPhase  = App.tourney.cophs((coId, coPhId))
        val game     = getData(elem, "game", 0)
        val rowBase  = getRowBase(coId, coPhId) 
 
        val gameUpdateList = coPhase.resetMatchPropagate(game)
        for (g <- gameUpdateList) setMatchView(rowBase, coId, coPhId, g)(coPhase)
        debug("actionEvent", s"DeleteMatchResult -> delete ${game} refresh: ${gameUpdateList.toString} status: ${coPhase.getStatusTxt}") 
      }

      case "DeleteAll"   => { 
        val coPhase  = App.tourney.cophs((coId, coPhId))
        val rowBase         = getRowBase(coId, coPhId) 

        val gameUpdateList  = coPhase.resetAllMatches()
        for (g <- gameUpdateList) setMatchView(rowBase, coId, coPhId, g)(coPhase)
        debug("actionEvent", s"DeleteAll -> game update list: ${gameUpdateList.toString} status: ${coPhase.getStatusTxt}") 
      }


      case "Demo"   => {
        val coPhase = App.tourney.cophs((coId, coPhId))
        var cnt     = 0
        coPhase.matches.foreach { m =>
          if (m.status != MEntry.MS_FIN & m.status != MEntry.MS_FIX) {
            enterDemoResult(coId, coPhId, m.gameNo, m.winSets, cnt*600)
            cnt = cnt+1
          }
        }
      } 
      case "BadRequest"          => debug("actionEvent", s"Invalid status (!= CPS_INPUT), invalid coId ${coId} or invalid coPhId ${coPhId}") 

      case _                     => debug("actionEvent(error)", s"unknown key: ${key}") 
    } // end key match
  }        


  //
  //  READ INPUT FIELDS
  //
  // getInputPlayfield returns playfield info
  def getInputPlayfield(row: HTMLElement, gameNo: Int): String = {
    val setElem = row.querySelector(s"[data-game_${gameNo}='playfield']").asInstanceOf[HTMLElement]
    setElem.innerText.replace('\u00A0',' ').trim
  }  

  // getInputInfo returns info
  def getInputInfo(row: HTMLElement, gameNo: Int): String = {
    val setElem = row.querySelector(s"[data-game_${gameNo}='info']").asInstanceOf[HTMLElement]
    setElem.innerText.replace('\u00A0',' ').trim
  }    

  // returns valid sets or (0,0)
  def getInputSets(row: HTMLElement, nWSets: Int, gameNo: Int): Either[Error, (Int,Int)] = {
    val setElem = row.querySelector(s"[data-game_${gameNo}='sets']").asInstanceOf[HTMLElement]

    val setInput = setElem.innerText.replace('\u00A0',' ').trim
    
    // possible input patterns
    val setPattern1 = """([0-9]{2})""".r
    val setPattern2 = """([0-9]+):([0-9]+)""".r

    setInput match {
      case setPattern1(x)   => {
        try {
          val (a,b) = (x.slice(0,1).toInt, x.slice(1,2).toInt)
          if (validSets((a,b), nWSets))  Right((a,b)) else  Left(Error("err0192.input.invalid.sets")) 
        } catch { case _: Throwable =>  Left(Error("err0192.input.invalid.sets"))  }
      }
      case setPattern2(x,y) => {
        try {
          val (a,b) = (x.toInt, y.toInt)
          if (validSets((a,b), nWSets)) Right((a,b)) else  Left(Error("err0192.input.invalid.sets")) 
        } catch { case _: Throwable =>  Left(Error("err0192.input.invalid.sets"))  }
      }
      case _                => {
        if (setInput == "") { Right((0,0)) } else { Left(Error("err0192.input.invalid.sets")) } 
      }  
    }
  }

  // returns valid sets and balls or ((0,0), Array() 
  //def getInputBalls(row: HTMLElement, nWSets: Int, gameNo: Int): (Array[String],(Int,Int)) = ???

  // getBallInput returns array of ball input, strips last blank input
  // eg. read from the end, start with first value <> "", then reverse
  def getInputBalls(row: HTMLElement, nWSets: Int, gameNo: Int): Either[Error, (Array[String], (Int,Int))] = {
    val buf = scala.collection.mutable.ArrayBuffer.empty[String]
    val inv = scala.collection.mutable.ArrayBuffer.empty[String]
    var containsBlank = false
    for (i <- (nWSets*2)-1 to 1 by -1) {
      val ballElem = row.querySelector(s"[data-game_${gameNo}='ball_${i}']").asInstanceOf[HTMLElement]
      val ballInput = ballElem.innerText.replace('\u00A0',' ').trim
      if (ballInput != "" | buf.length>0) buf += ballInput
    } 
    for (i<-buf.length-1 to 0 by -1) {
      if (buf(i) == "") { containsBlank = true } else { inv += buf(i) }
    }  

    if (containsBlank) {
      Left(Error(""))
    } else if (inv.length==0) {
      Right( (inv.toArray, (0,0)) )
    } else {
      getSetsFromShortArr(inv.toArray, nWSets) match {
        case Left(err)   => Left(err)
        case Right(sets) => Right( (inv.toArray, sets) )  
      }
    } 
  }


  //
  //  COMMON-SECTION
  //
  // set input page for a competition phase, coId != 0 and coPhId != 0
  def setPage(coPhase: CompPhase): Unit = {
    val coId   = coPhase.coId
    val coPhId = coPhase.coPhId
    debug("init", s"coId: ${coId} coPhId: ${coPhId}")
    if (!exists_(s"Input_${coId}_${coPhId}")) {
      val elem    = getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement]
      val size    = coPhase.size
      val coPhTyp = coPhase.coPhTyp
      val maxRnd  = coPhase.getMaxRnds
      val winSets = coPhase.noWinSets
      coPhTyp match {
        case CPT_GR => { 
          setHtml(elem, clientviews.organize.competition.input.html.GroupCard(coId, coPhId, maxRnd, winSets))
        }  
        case CPT_KO => {
          setHtml(elem, clientviews.organize.competition.input.html.KoCard(coId, coPhId, maxRnd, winSets))
          var gameNo = 0
          for (rnd <- maxRnd to 0 by -1) {
            val cnt = scala.math.pow(2, rnd-1).toInt.max(1) 
            val tableId = s"InputRound_${coId}_${coPhId}_${rnd}"
            setHtml(tableId, "")(UCP())
            for (j<-1 to cnt) {
              gameNo = gameNo + 1
              val rowElem = getElemById(tableId)(UCP()).asInstanceOf[HTMLTableElement].insertRow(-1)
              rowElem.setAttribute(s"data-game_${gameNo}", "row") 
              setHtml(rowElem, clientviews.organize.competition.input.html.KoMatchEntry(coId, coPhId, gameNo, winSets))
            }
          }
        }  
        case CPT_SW => setHtml(elem, "input for sw-system")
        case _      => setHtml(elem, showAlert(getMsg("invalidSection")))
      }
    }

    // update page 
    coPhase.coPhTyp match {
      case CPT_GR => {
        val matchMap = coPhase.matches.groupBy(mEntry=>mEntry.round)
        val maxRnd = coPhase.getMaxRnds
        for (rnd <- 1 to maxRnd) {
          val tableId = s"InputRound_${coId}_${coPhId}_${rnd}"
          BasicHtml.setHtml_(tableId, "")
          try {
            for (m <- matchMap(rnd).sortBy(mEntry => mEntry.gameNo)) {
              val rowElem = getElemById_(tableId).asInstanceOf[HTMLTableElement].insertRow(-1)
              rowElem.setAttribute(s"data-game_${m.gameNo}", "row")
              // rowElem.setAttribute("contenteditable", "true")
              setGrMatch(coPhase, rowElem, m.asInstanceOf[MEntryGr])
            }
          } catch { case _: Throwable => error("update", s"matchMap.size: ${matchMap.size} maxRnd: ${maxRnd}") }
        } 
      }  
      case CPT_KO => for (m <- coPhase.matches) setMatchViewContent(getMatchRow(m), m.asInstanceOf[MEntryKo])

      case _ =>  {
        setHtml(getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement],
                showAlert(getMsg("invalidSection"))) 
      }  
    }
    
    // STATUS dependend settings
    val editible = (coPhase.status == CompPhase.CPS_EIN | coPhase.status == CompPhase.CPS_FIN)
    setAttribute(gE(s"Input_${coId}_${coPhId}"), "contenteditable", editible.toString)
    setVisible(gE(s"InputDemoBtn_${coId}_${coPhId}"), App.tourney.cophs((coId,coPhId)).demo & editible)
  }


  /** setMatchStatus set status for different kind of matches
   *  currently only group- and ko-matches
   */ 
  def setMatchView(rowBase: HTMLElement, coId: Long, coPhId: Int, game: Int)(implicit coPhase: CompPhase) = {
    val row = rowBase.querySelector(s"[data-game_${game}='row']").asInstanceOf[HTMLElement] 
    val m = coPhase.getMatch(game)
    setMatchViewContent(row, m) 
  }        


  //
  //  GROUP-SECTION
  //
  def setGrMatch2(coId: Long, coPhId: Int, elem: HTMLElement, m: MEntryGr)(implicit coPhase: CompPhase) = {
    val (grpName, wgw) = (Group.genName(m.grId), s"${m.wgw._1}-${m.wgw._2}")
    elem.innerHTML = clientviews.organize.competition.input.html.GrMatchEntry(
      coId, coPhId, grpName, wgw, m.gameNo, coPhase.noWinSets).toString
    setMatchViewContent(elem, m) 
  }
  
  def setGrMatch(coph: CompPhase, elem: HTMLElement, m: MEntryGr) = {
    val (grpName, wgw) = (Group.genName(m.grId), s"${m.wgw._1}-${m.wgw._2}")
    elem.innerHTML = clientviews.organize.competition.input.html.GrMatchEntry(
      coph.coId, coph.coPhId, grpName, wgw, m.gameNo, coph.noWinSets).toString
    setMatchViewContent(elem, m) 
  }


  //
  //  setMatchViewContent
  //
  def setMatchViewContent(row: HTMLElement, m: MEntry)  = {
    import shared.model.MEntry._
    implicit val trny = App.tourney

    // get the view elements
    val nameA      = row.querySelector(s"[data-game_${m.gameNo}='nameA']").asInstanceOf[HTMLElement]
    val nameB      = row.querySelector(s"[data-game_${m.gameNo}='nameB']").asInstanceOf[HTMLElement]
    val info       = row.querySelector(s"[data-game_${m.gameNo}='info']").asInstanceOf[HTMLElement]
    val sets       = row.querySelector(s"[data-game_${m.gameNo}='sets']").asInstanceOf[HTMLElement]
    val playfield  = row.querySelector(s"[data-game_${m.gameNo}='playfield']").asInstanceOf[HTMLElement]

    val gameNo     = row.querySelector(s"[data-game_${m.gameNo}='gameNo']").asInstanceOf[HTMLElement]    
    val saveBtn    = row.querySelector(s"[data-game_${m.gameNo}='save']").asInstanceOf[HTMLElement]
    val deleteBtn  = row.querySelector(s"[data-game_${m.gameNo}='delete']").asInstanceOf[HTMLElement]
    val resultElts = row.querySelectorAll(s"[data-result_${m.gameNo}='result']")

    setHtml(nameA, SNO(m.stNoA).getName(m.coTyp, getMsg("bye")))
    setHtml(nameB, SNO(m.stNoB).getName(m.coTyp, getMsg("bye")))
    setHtml(info, m.info)
    setHtml(sets, if (m.sets!=(0,0)) s"${m.sets._1}:${m.sets._2}" else "")
    setHtml(playfield, m.getPlayfield)

    // set ball view
    val balls = m.result.split('·')
    for (i <- 0 to (m.winSets-1)*2) {
      val elem = row.querySelector(s"[data-game_${m.gameNo}='ball_${i+1}']").asInstanceOf[HTMLElement]
      if (i<balls.length) setHtml(elem, balls(i)) else setHtml(elem, "")
    }    

    println(s"Set match view: ${m.gameNo} ${MEntry.statusInfo(m.status)}")
    // set editible, color and buttons    
    gameNo.classList.remove("text-success")
    gameNo.classList.remove("text-danger")
    gameNo.classList.remove("text-dark")
    gameNo.classList.remove("text-info")

    row.classList.remove("bg-light")
    row.classList.remove("bg-secondary")
    row.classList.remove("text-white")

    m.status match {
      case MS_MISS  => {
        gameNo.classList.add("text-danger")
        resultElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
        setDisabled(saveBtn, true); setDisabled(deleteBtn, true)
      }
      case MS_BLOCK => {
        gameNo.classList.add("text-danger")
        resultElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
        setDisabled(saveBtn, false); setDisabled(deleteBtn, true)
      } 
      case MS_READY => {
        gameNo.classList.add("text-success")
        resultElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"true"))
        setDisabled(saveBtn, false); setDisabled(deleteBtn, true)         
      } 
      case MS_RUN   => {
        gameNo.classList.add("text-primary")
        resultElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"true"))
        setDisabled(saveBtn, false); setDisabled(deleteBtn, true)     
      } 
      case MS_FIN   => {
        gameNo.classList.add("text-dark")
        resultElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
        row.classList.add("bg-light")
        setDisabled(saveBtn, true); setDisabled(deleteBtn, false)        
      } 
      case MS_FIX   => {
        resultElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
        row.classList.add("bg-secondary")
        row.classList.add("text-white")
        setDisabled(saveBtn, true); setDisabled(deleteBtn, true)      
      }
      case _        => {}
    }
  }


  // return corresponding row element of ko match
  def getMatchRow(m: MEntry): HTMLElement = { 
    try {
      val tableElem = m.coPhTyp match {
        case CPT_KO  => getElemById_(s"InputRound_${m.coId}_${m.coPhId}_${m.round}").asInstanceOf[HTMLElement]
        case CPT_GR  => getElemById_(s"InputRound_${m.coId}_${m.coPhId}_${m.round}").asInstanceOf[HTMLElement]
        case _       => dom.document.createElement("div").asInstanceOf[HTMLElement] 
      }
      tableElem.querySelector(s"[data-game_${m.gameNo}='row']").asInstanceOf[HTMLElement]
    } catch { case _: Throwable => {
      error("getMatchRow", s"coId: ${m.coId} coPhId: ${m.coPhId} round: ${m.round} gameNo: ${m.gameNo}")
      dom.document.createElement("div").asInstanceOf[HTMLElement] 
    }}
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