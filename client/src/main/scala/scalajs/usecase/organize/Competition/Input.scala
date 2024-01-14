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
    
    def getRowBase(coId: Long, coPhId: Int):HTMLElement = gE(s"Input_${coId}_${coPhId}").asInstanceOf[HTMLElement]   
        
    val (coId, coPhId) = (getData(elem,"coId",0L), getData(elem,"coPhId",0))
    val action = if (App.tourney.cophs.contains((coId, coPhId))) {
      val status = App.tourney.cophs((coId, coPhId)).getStatus
      if (status == CompPhaseStatus.EIN || status == CompPhaseStatus.FIN ||
          (status == CompPhaseStatus.AUS && key=="InputStart"))
         key else "BadRequest"
    } else {
      "BadRequest"
    }

    // debug("actionEvent", s"key: ${key} -> ${action}")
    action match {

      case "SaveMatchResult"   => { 
        val game    = getData(elem, "game", -1)
        val rowBase = getRowBase(coId, coPhId)
        val row     = getRow(coId, coPhId, game)
        
        // get balls and/or sets
        // check values and if ok save input to local match (and server)
        val noWinSets = App.tourney.getCoPhNoWinSets(coId, coPhId)

        val bInput  = getInputBalls(row, noWinSets, game)
        val sInput  = getInputSets(row, noWinSets, game)
        
        val (inputOk, balls, sets, err) = bInput match {
          case Left(err)  => (false, "", (0,0), err)
          case Right(res) => if (res._2 != (0,0)) {
              (true,  res._1.mkString("·"), res._2, Error.dummy)
            } else sInput match {
              case Left(err)  => (false, "", (0,0), err)
              case Right(res) => (true, "", res, Error.dummy)
            }
        }
        if (inputOk) inputMatch(coId, coPhId, game, sets, balls, getInputInfo(row, game), getInputPlayfield(row, game), getUTCTimestamp).map {
          case Left(err)           => error("actionEvent", s"SaveMatchResult -> inputMatch: ${err}") 
          case Right(gUpdateList)  => for (game <- gUpdateList) setMatchView(App.tourney.cophs((coId, coPhId)), rowBase, game) 
        } else error("actionEvent", s"SaveMatchResult -> input not OK: ${err}")
      }
      
      case "DeleteMatchResult"   => {
        val rowBase  = getRowBase(coId, coPhId)
        val game     = getData(elem, "game", -1)
        resetMatch(coId, coPhId, game).map {
          case Left(err)          => error("actionEvent", s"DeleteMatchResult -> resetMatch: ${err}")
          case Right(gUpdateList) => {
            App.tourney.delPlayfield(coId, coPhId, game)
            for (g <- gUpdateList) { 
              App.tourney.delPlayfield(coId, coPhId, g)
              setMatchView(App.tourney.cophs((coId, coPhId)), rowBase, g)
            }
          }  
        }        
      }

      case "DeleteAll"   => { 
        val rowBase  = getRowBase(coId, coPhId) 

        dlgCancelOk(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", App.tourney.getCompPhaseName(coId,coPhId))) {
          resetMatches(coId, coPhId).map {
            case Left(err)          => error("actionEvent", s"DeleteAll -> resetMatches: ${err}")
            case Right(gUpdateList) => for (g <- gUpdateList) {
              App.tourney.delPlayfield(coId, coPhId, g)
              setMatchView(App.tourney.cophs((coId, coPhId)), rowBase, g)
            }  
          } 
        }
      }


      case "Demo"   => {
        val coPhase = App.tourney.cophs((coId, coPhId))

        for (i<-0 until coPhase.getMaxRnds+2) {
          if (coPhase.status != CompPhaseStatus.FIN) {         
            dom.window.setTimeout(() => { 
              debug("DemoResult", s"CompPhase status: ${coPhase.status} round: ${i}")
              if ((coPhase.matches.count { _.status == MEntry.MS_READY }) > 0) {
                var cnt = 0
                coPhase.matches.foreach { m => if (m.status == MEntry.MS_READY) { 
                  enterDemoResult(coId, coPhId, m.gameNo, m.winSets, cnt*200) 
                  cnt = cnt + 1
                }}
              }
            } , i*2000)
          }
        }  
      } 

      case "InputStart"          => updateCompPhaseStatus(coId, coPhId, CompPhaseStatus.EIN) map {
        case Left(err)   => error("StartInputCoPh", s"${err}") 
        case Right(res)  => App.execUseCase("OrganizeCompetitionInput", "", "")  
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


  // getBallInput returns array of ball input, strips last blank input
  // eg. read from the end, start with first value <> "", then reverse
  // returns valid sets and balls or ((0,0), Array()
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
      Left(Error("err0254.ballInput.invalid"))
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
  def setPage(coph: CompPhase): Unit = {
    val coId   = coph.coId
    val coPhId = coph.coPhId
    
    val contentElement = gE(s"InputContent_${coph.coId}_${coph.coPhId}")
    if (contentElement.innerHTML == "") {
      //debug("setPage", s"Input init: coId: ${coph.coId} coPhId: ${coph.coPhId}")
      val size    = coph.size
      val maxRnd  = coph.getMaxRnds
      val winSets = coph.noWinSets
      coph.getTyp match {
        case CompPhaseTyp.GR | CompPhaseTyp.RR => setHtml(contentElement, clientviews.organize.competition.input.html.GroupCard(coph))
        case CompPhaseTyp.KO => {
          setHtml(contentElement, clientviews.organize.competition.input.html.KoCard(coph))
          var gameNo = 0
          for (rnd <- maxRnd to 0 by -1) {
            val cnt = scala.math.pow(2, rnd-1).toInt.max(1) 
            val tableId = s"InputRound_${coId}_${coPhId}_${rnd}"
            setHtml(gE(tableId), "")
            for (j<-1 to cnt) {
              gameNo = gameNo + 1
              val rowElem = gE(tableId).asInstanceOf[HTMLTableElement].insertRow(-1)
              rowElem.setAttribute(s"data-game_${gameNo}", "row") 
              setHtml(rowElem, clientviews.organize.competition.input.html.KoMatchEntry(coId, coPhId, gameNo, winSets))
            }
          }
        } 
        case CompPhaseTyp.SW => setHtml(contentElement, "input for sw-system")
        case _               => setHtml(contentElement, showAlert(getMsg("invalidSection")))
      }
    }  

    // update input page 
    //debug("setPage", s"Input update: coId: ${coph.coId} coPhId: ${coph.coPhId}")
    coph.getTyp match {
      case CompPhaseTyp.GR | CompPhaseTyp.RR  => {
        val matchMap = coph.matches.groupBy(mEntry=>mEntry.round)
        val maxRnd = coph.getMaxRnds
        for (rnd <- 1 to maxRnd) {
          val tableId = s"InputRound_${coId}_${coPhId}_${rnd}"
          setHtml(gE(tableId), "")
          try {
            for (m <- matchMap(rnd).sortBy(mEntry => mEntry.gameNo)) {
              val rowElem = gE(tableId).asInstanceOf[HTMLTableElement].insertRow(-1)
              rowElem.setAttribute(s"data-game_${m.gameNo}", "row")
              setGrMatch(coph, rowElem, m.asInstanceOf[MEntryGr])
            }
          } catch { case _: Throwable => error("update", s"matchMap.size: ${matchMap.size} maxRnd: ${maxRnd}") }
        } 
      }  
      case CompPhaseTyp.KO => for (m <- coph.matches) setMatchViewContent(getMatchRow(m), m.asInstanceOf[MEntryKo], coph.status)

      case _ => {} 

    }
    

    // STATUS dependend settings
    val editible = (coph.status == CompPhaseStatus.EIN | coph.status == CompPhaseStatus.FIN)
    setAttribute(gE(s"Input_${coId}_${coPhId}"), "contenteditable", editible.toString)
    setVisible(gE(s"InputStartBtn_${coId}_${coPhId}"), coph.status==CompPhaseStatus.AUS)
    setVisible(gE(s"InputDemoBtn_${coId}_${coPhId}"), App.tourney.cophs((coId,coPhId)).demo)
    setDisabled(gE(s"InputDemoBtn_${coId}_${coPhId}"), !editible)
  }


  /** setMatchStatus set status for different kind of matches
   *  currently only group- and ko-matches
   */ 
  def setMatchView(coPhase: CompPhase, rowBase: HTMLElement, game: Int) = {
    val row = rowBase.querySelector(s"[data-game_${game}='row']").asInstanceOf[HTMLElement] 
    val m = coPhase.getMatch(game)
    setMatchViewContent(row, m, coPhase.status) 
  }        


  //
  //  GROUP-SECTION
  //  
  def setGrMatch(coph: CompPhase, elem: HTMLElement, m: MEntryGr) = {
    val (grpName, wgw) = coph.getTyp match {
      case CompPhaseTyp.GR => (Group.genName(m.grId), s"${m.wgw._1}-${m.wgw._2}")
      case CompPhaseTyp.RR => ("", s"${m.wgw._1}-${m.wgw._2}")
    }
    elem.innerHTML = clientviews.organize.competition.input.html.GrMatchEntry(
      coph.coId, coph.coPhId, grpName, wgw, m.gameNo, coph.noWinSets).toString
    setMatchViewContent(elem, m, coph.status) 
  }


  //
  //  setMatchViewContent
  //
  def setMatchViewContent(row: HTMLElement, m: MEntry, status: CompPhaseStatus.Value)  = {
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
    val inputElts  = row.querySelectorAll(s"[data-input_${m.gameNo}]")

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

    // disable buttons if status is not CPS_EIN or CPS_FIN
    val disBtn = if (status == CompPhaseStatus.UNKN || status == CompPhaseStatus.AUS) true else false
    //println(s"Set match view: ${m.gameNo} ${MEntry.statusInfo(m.status)}")

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
        inputElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
        setDisabled(saveBtn, true || disBtn ); setDisabled(deleteBtn, true)
      }
      case MS_BLOCK => {
        gameNo.classList.add("text-danger")
        inputElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
        setDisabled(saveBtn, false || disBtn ); setDisabled(deleteBtn, true)
      } 
      case MS_READY => {
        gameNo.classList.add("text-success")
        inputElts.map(_.asInstanceOf[HTMLElement].removeAttribute("contenteditable"))
        setDisabled(saveBtn, false || disBtn); setDisabled(deleteBtn, true)         
      } 
      case MS_RUN   => {
        gameNo.classList.add("text-primary")
        inputElts.map(_.asInstanceOf[HTMLElement].removeAttribute("contenteditable"))
        setDisabled(saveBtn, false || disBtn); setDisabled(deleteBtn, true)     
      } 
      case MS_FIN   => {
        gameNo.classList.add("text-dark")
        inputElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
        row.classList.add("bg-light")
        setDisabled(saveBtn, true); setDisabled(deleteBtn, false || disBtn)        
      } 
      case MS_FIX   => {
        inputElts.map(_.asInstanceOf[HTMLElement].setAttribute("contenteditable", s"false"))
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
        case CompPhaseTyp.KO  => gE(s"InputRound_${m.coId}_${m.coPhId}_${m.round}")
        case CompPhaseTyp.GR  => gE(s"InputRound_${m.coId}_${m.coPhId}_${m.round}")
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

    val r = scala.util.Random
    val saveBtn = gE(s"SaveBtn_${coId}_${coPhId}_${matchNo}").asInstanceOf[HTMLElement]
    val pfElem = gE(s"Playfield_${coId}_${coPhId}_${matchNo}").asInstanceOf[HTMLElement]

    val result = (for (i<-0 until (noWinSets*2)+1) yield {r.nextInt(2)}).toArray
    val (setA, setB) = result.foldLeft((0,0)) {(t,v) => if (t._1 == noWinSets | t._2 == noWinSets) (t._1, t._2) else (t._1 + v, t._2 + (v^1)) }     
    
    for (i<-0 until (setA+setB)) {
      val ball = if (result(i)>0 ) s"${r.nextInt(15)}"  else s"-${r.nextInt(15)}" 
      balls += ball
      ballElems += gE(s"Input_${coId}_${coPhId}").querySelector(s"[data-game_${matchNo}='ball_${i+1}']").asInstanceOf[HTMLElement]
    }
    //println(s"enterDemoResult match: ${matchNo} balls: ${balls.mkString(":")}") 

    // first set playfield to indicate running game
    if (offsetMS == 0) {
      pfElem.innerText = s"${r.nextInt(9)+1}"
      clickButton(saveBtn)
    } else {
      dom.window.setTimeout(() => { pfElem.innerText = s"${r.nextInt(9)+1}"; clickButton(saveBtn) }, offsetMS)
    }
    
    // later enter result, to finisch game
    val playtime = offsetMS + 100*(r.nextInt(3)+1)
    for (i<-0 until (setA+setB)) {
      dom.window.setTimeout(() => { ballElems(i).innerText = balls(i); if (i == setA+setB-1) clickButton(saveBtn) }, playtime + i*50)
    }
  }

  def clickButton(btn: HTMLElement) = {
    val bgC = btn.style.backgroundColor
    btn.style.backgroundColor="#7F7F7F"
    dom.window.setTimeout(() => { btn.style.backgroundColor=bgC }, 100)
    btn.click()
  }

  // getUTCTimestamp
  // format: yyyyMMddhhmmss
    def getUTCTimestamp: String = {
      val date  = new js.Date()
      f"${date.getUTCFullYear().toInt}%04d${date.getUTCMonth().toInt+1}%02d${date.getUTCDate().toInt}%02d${date.getUTCHours().toInt}%02d${date.getUTCMinutes().toInt}%02d${date.getUTCSeconds().toInt}%02d"
    }


}