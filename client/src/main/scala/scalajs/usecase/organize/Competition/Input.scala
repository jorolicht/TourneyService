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
  with TourneySvc with DrawSvc with MatchSvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Input")
  }

@JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    //import shared.utils.Routines._ 
    import org.scalajs.dom.document

    def getRow(coId: Long, coPhId: Int, game: Int):HTMLElement = {
      val rowBase = getElemById(s"Input_${coId}_${coPhId}").asInstanceOf[HTMLElement]
      rowBase.querySelector(s"[data-game_${game}='row']").asInstanceOf[HTMLElement]    
    }  
    
    def getRowBase(coId: Long, coPhId: Int):HTMLElement = {
      getElemById(s"Input_${coId}_${coPhId}").asInstanceOf[HTMLElement]   
    } 

    val trny = App.tourney
    
    //debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    debug("actionEvent", s"key: ${key}")
    key match {

      case "SaveMatchResult"   => { 
        val (coId, coPhId, game)  = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0), getData(elem, "game", 0))
        val rowBase               = getRowBase(coId, coPhId)
        val row                   = getRow(coId, coPhId, game)
        
        // get balls and/or sets
        // check values and if ok save input to local match (and server)
        val winSets = trny.cophs((coId, coPhId)).noWinSets
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
          val gameUpdateList = trny.cophs((coId, coPhId)).setMatchPropagate(game, sets, balls, getInputInfo(row, game), getInputPlayfield(row, game))  
          for (game <- gameUpdateList) { setMatchView(rowBase, coId, coPhId, game)(trny) }
          debug("actionEvent", s"input OK: status: ${trny.cophs((coId,coPhId)).getMatch(game).toString} refresh: ${gameUpdateList.toString}")
        } else {
          error("actionEvent", s"input not OK: ${err}")
        }
      }
      
      case "DeleteMatchResult"   => {
        val (coId, coPhId, game) = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0), getData(elem, "game", 0))
        val rowBase              = getRowBase(coId, coPhId)    

        val gameUpdateList = trny.cophs((coId, coPhId)).resetMatchPropagate(game)
        for (g <- gameUpdateList) setMatchView(rowBase, coId, coPhId, g)(trny)
        debug("actionEvent", s"delete ${game} refresh: ${gameUpdateList.toString}") 
      }

      case "DeleteAllResults"   => { 
        val (coId, coPhId)  = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0))
        val rowBase         = getRowBase(coId, coPhId) 

        val gameUpdateList  = trny.cophs((coId, coPhId)).resetAllMatches()
        for (g <- gameUpdateList) setMatchView(rowBase, coId, coPhId, g)(trny)
        debug("actionEvent", s"delete all: ${gameUpdateList.toString}") 
      }

      case _                     => debug("actionEvent(error)", s"unknown key: ${key}") 

    }
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
  // setInputFrame for a competition, coId != 0
  def setFrame(coId: Long, coPhId: Int, reload: Boolean)(implicit trny: Tourney): Unit = {
    //debug("setFrame", s"coId: ${coId} coPhId: ${coPhId}")
    if (!exists(s"Input_${coId}_${coPhId}") | reload) {
      val elem    = getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement]
      val size    = trny.cophs(coId, coPhId).size
      val coPhTyp = trny.cophs(coId, coPhId).coPhTyp
      val maxRnd  = trny.cophs(coId, coPhId).getMaxRnds
      val winSets = trny.cophs(coId,coPhId).noWinSets
      coPhTyp match {
        case CPT_GR => setHtml(elem, clientviews.organize.competition.input.html.GroupCard(coId, coPhId, maxRnd, winSets))
        case CPT_KO => {
          setHtml(elem, clientviews.organize.competition.input.html.KoCard(coId, coPhId, maxRnd, winSets))
          var gameNo = 0
          for (rnd <- maxRnd to 0 by -1) {
            val cnt = scala.math.pow(2, rnd-1).toInt.max(1) 
            val tableElem = s"InputRound_${coId}_${coPhId}_${rnd}"
            setHtml(tableElem, "")
            for (j<-1 to cnt) {
              gameNo = gameNo + 1
              val rowElem = getElemById(tableElem).asInstanceOf[HTMLTableElement].insertRow(-1)
              rowElem.setAttribute(s"data-game_${gameNo}", "row") 
              setHtml(rowElem, clientviews.organize.competition.input.html.KoMatchEntry(coId, coPhId, gameNo, winSets))
            }
          }
        }  
        case CPT_SW => setHtml(elem, "input for sw-system")
        case _      => setHtml(elem, showAlert(getMsg("invalidSection")))
      }
    }
  }


  def setContent(coId: Long, coPhId: Int) (implicit trny: Tourney) = {
    trny.cophs(coId, coPhId).coPhTyp match {
      case CPT_GR => {
        val matchMap = trny.cophs(coId,coPhId).matches.groupBy(mEntry=>mEntry.round)
        val maxRnd = trny.cophs(coId, coPhId).getMaxRnds
        for (rnd <- 1 to maxRnd) {
          val tableElem = s"InputRound_${coId}_${coPhId}_${rnd}"
          setHtml(tableElem, "")
          for (m <- matchMap(rnd).sortBy(mEntry => mEntry.gameNo)) {
            val rowElem = getElemById(tableElem).asInstanceOf[HTMLTableElement].insertRow(-1)
            rowElem.setAttribute(s"data-game_${m.gameNo}", "row") 
            setGrMatch(coId, coPhId, rowElem, m.asInstanceOf[MEntryGr])(trny)
          }
        } 
      }  
      case CPT_KO => for (m <- trny.cophs(coId, coPhId).matches) setMatchViewContent(getMatchRow(m), m.asInstanceOf[MEntryKo])(trny)

      case _ =>  {
        setHtml(getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement],
                showAlert(getMsg("invalidSection"))) 
      }  
    }
  }


  /** setMatchStatus set status for different kind of matches
   *  currently only group- and ko-matches
   */ 
  def setMatchView(rowBase: HTMLElement, coId: Long, coPhId: Int, game: Int)(implicit trny: Tourney) = {
    val row = rowBase.querySelector(s"[data-game_${game}='row']").asInstanceOf[HTMLElement] 
    val m = trny.cophs((coId, coPhId)).getMatch(game)
    setMatchViewContent(row, m) 
  }        


  //
  //  GROUP-SECTION
  //
  def setGrMatch(coId: Long, coPhId: Int, elem: HTMLElement, m: MEntryGr)(implicit trny: Tourney) = {
    val (grpName, wgw) = (Group.genName(m.grId), s"${m.wgw._1}-${m.wgw._2}")
    elem.innerHTML = clientviews.organize.competition.input.html.GrMatchEntry(
      coId, coPhId, grpName, wgw, m.gameNo, trny.cophs(coId,coPhId).noWinSets).toString
    setMatchViewContent(elem, m) 
  }


  //
  //  KO-SECTION
  //
  def setMatchViewContent(row: HTMLElement, m: MEntry)(implicit trny: Tourney)  = {
    import shared.model.MEntry._

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
        case CPT_KO  => getElemById(s"InputRound_${m.coId}_${m.coPhId}_${m.round}").asInstanceOf[HTMLElement]
        case CPT_GR  => getElemById(s"InputRound_${m.coId}_${m.coPhId}_${m.round}").asInstanceOf[HTMLElement]
        case _       => dom.document.createElement("div").asInstanceOf[HTMLElement] 
      }
      tableElem.querySelector(s"[data-game_${m.gameNo}='row']").asInstanceOf[HTMLElement]
    } catch { case _: Throwable => {
      error("getMatchRow", s"coId: ${m.coId} coPhId: ${m.coPhId} round: ${m.round} gameNo: ${m.gameNo}")
      dom.document.createElement("div").asInstanceOf[HTMLElement] 
    }}
  }

}