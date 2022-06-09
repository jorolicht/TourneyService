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
import shared.model.tabletennis._

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
    import shared.utils.Routines._ 
    import org.scalajs.dom.document
    
    // import org.scalajs.dom.raw.HTMLCollection
    // import org.scalajs.dom.raw.NodeList
    //debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    debug("actionEvent", s"key: ${key}")
    key match {

      case "SaveMatchResult"   => { 
        val coId   = getData(elem, "coId", 0L)
        val coPhId = getData(elem, "coPhId", 0)
        val game   = getData(elem, "game", 0)

        val tableElem = getElemById(s"Input_${coId}_${coPhId}").asInstanceOf[HTMLElement]
        val row       = tableElem.querySelector(s"[data-game_${game}='row']").asInstanceOf[HTMLElement]
        
        // get balls and/or sets
        // check values and if ok 
        // save input to local match (and server)
        val bInput = getInputBalls(row, 3, game)
        val sInput = getInputSets(row, 3, game)
        
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
          val m = App.tourney.cophs((coId,coPhId)).getMatch(game)
          val running = false
          m.setSets(sets)
          m.setResult(balls)
          m.setInfo(getInputInfo(row, game))
          m.setPlayfield(getInputPlayfield(row, game))
          m.setStatus(calcStatus( SNO(m.stNoA), SNO(m.stNoB), m.playfield!="", m.sets, validSets(m.sets, m.winSets), running))

          App.tourney.cophs((coId,coPhId)).setMatch(m)
          setKoMatchStatus(row, m.asInstanceOf[MEntryKo])

          debug("actionEvent", s"input OK: status: ${m.toString}")
        } else {
          error("actionEvent", s"input not OK: ${err}")
        }
      }
      
      case "DeleteMatchResult"   => {
        val coId      = getData(elem, "coId", 0L)
        val coPhId    = getData(elem, "coPhId", 0)
        val game      = getData(elem, "game", 0)
        val tableElem = getElemById(s"Input_${coId}_${coPhId}").asInstanceOf[HTMLElement]
        val row       = tableElem.querySelector(s"[data-game_${game}='row']").asInstanceOf[HTMLElement]        

        val m = App.tourney.cophs((coId,coPhId)).getMatch(game)
        val running = false
        m.setSets((0,0))
        m.setResult("")
        m.setPlayfield("")
        m.setStatus(calcStatus( SNO(m.stNoA), SNO(m.stNoB), m.playfield!="", m.sets, validSets(m.sets, m.winSets), running))
        App.tourney.cophs((coId,coPhId)).setMatch(m)
        setKoMatchStatus(row, m.asInstanceOf[MEntryKo])
        debug("actionEvent", s"delete: ${game}")      
      }

      case _                     => { 
        debug("actionEvent(error)", s"unknown key: ${key}") 
      }

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
      coPhTyp match {
        case CPT_GR => setHtml(elem, clientviews.organize.competition.input.html.GroupCard(coId, coPhId, maxRnd))
        case CPT_KO => {
          setHtml(elem, clientviews.organize.competition.input.html.KoCard(coId, coPhId, maxRnd))
          var gameNo = 0
          for (rnd <- maxRnd to 0 by -1) {
            val cnt = scala.math.pow(2, rnd-1).toInt.max(1) 
            val tableElem = s"InputRound_${coId}_${coPhId}_${rnd}"
            setHtml(tableElem, "")
            for (j<-1 to cnt) {
              gameNo = gameNo + 1
              val rowElem = getElemById(tableElem).asInstanceOf[HTMLTableElement].insertRow(-1)
              rowElem.setAttribute(s"data-game_${gameNo}", "row") 
              setHtml(rowElem, clientviews.organize.competition.input.html.KoMatchEntry(coId, coPhId, gameNo, trny.cophs(coId,coPhId).noWinSets))
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
      case CPT_KO => for (m <- trny.cophs(coId, coPhId).matches) setKoMatch(m.asInstanceOf[MEntryKo])(trny)

      case _ =>  {
        setHtml(getElemById_(s"InputContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']").asInstanceOf[HTMLElement],
                showAlert(getMsg("invalidSection"))) 
      }  
    }
  }



  //
  //  GROUP-SECTION
  //

  def setGrMatch(coId: Long, coPhId: Int, elem: HTMLElement, m: MEntryGr)(implicit trny: Tourney) = {
    val (grpName, wgw) = (getGroupName(m.grId), s"${m.wgw._1}-${m.wgw._2}")
    elem.innerHTML = clientviews.organize.competition.input.html.MatchEntry(
      grpName, wgw,
      SNO(m.stNoA).getName(m.coTyp, getMsg("bye")), 
      SNO(m.stNoB).getName(m.coTyp, getMsg("bye")), 
      m.gameNo, m.info, m.getPlayfield, m.result.split('·'), 
      m.sets, trny.cophs(coId,coPhId).noWinSets).toString
  }


  //
  //  KO-SECTION
  //

  def setKoMatch(m: MEntryKo)(implicit trny: Tourney)  = {
    val row = getKoMatchRow(m)
    setKoMatchElem(row, m.gameNo, "nameA", SNO(m.stNoA).getName(m.coTyp, getMsg("bye")))
    setKoMatchElem(row, m.gameNo, "nameB", SNO(m.stNoB).getName(m.coTyp, getMsg("bye")))
    setKoMatchElem(row, m.gameNo, "info", m.info)
    // setKoMatchElem(row, m.gameNo, "playfield", m.getPlayfield)
    // setKoMatchElem(row, m.gameNo, "sets", if (m.sets!=(0,0)) s"${m.sets._1}:${m.sets._2}" else "")
    // setKoMatchBalls(row, m.gameNo, m.winSets, m.result.split('·'))
    setKoMatchStatus(row, m)
  }

  // set balls in result fields
  def setKoMatchBalls(row: HTMLElement, gameNo: Int, noWinSets: Int, balls:Array[String]) = {
    for (i <- 0 to (noWinSets-1)*2) {
      val elem = row.querySelector(s"[data-game_${gameNo}='ball_${i+1}']").asInstanceOf[HTMLElement]
      if (i<balls.length) setHtml(elem, balls(i)) else setHtml(elem, "")
    }
  }

  // mark row according to status
  def setKoMatchStatus(row: HTMLElement, m: MEntryKo) = {
    import shared.model.MEntry._
    
    val gameNo = row.querySelector(s"[data-game_${m.gameNo}='gameNo']").asInstanceOf[HTMLElement]
    gameNo.classList.remove("text-success")
    gameNo.classList.remove("text-danger")
    gameNo.classList.remove("text-dark")
    gameNo.classList.remove("text-info")

    row.classList.remove("bg-light")
    row.classList.remove("bg-secondary")
    row.classList.remove("text-white")

    setKoMatchElem(row, m.gameNo, "sets", if (m.sets!=(0,0)) s"${m.sets._1}:${m.sets._2}" else "")
    setKoMatchBalls(row, m.gameNo, m.winSets, m.result.split('·'))
    setKoMatchElem(row, m.gameNo, "playfield", m.getPlayfield)

    m.status match {
      case MS_MISS  => {
        gameNo.classList.add("text-danger")
        setKoMatchEditable(row, m.gameNo, false)
        setKoMatchBtn(row, m.gameNo, "save", true)
        setKoMatchBtn(row, m.gameNo, "delete", true)
      }
      case MS_BLOCK => {
        gameNo.classList.add("text-danger")
        setKoMatchEditable(row, m.gameNo, false)
        setKoMatchBtn(row, m.gameNo, "save", true)
        setKoMatchBtn(row, m.gameNo, "delete", true)      
      } 
      case MS_READY => {
        gameNo.classList.add("text-success")
        setKoMatchEditable(row, m.gameNo, true)
        setKoMatchBtn(row, m.gameNo, "save", false)
        setKoMatchBtn(row, m.gameNo, "delete", true)            
      } 
      case MS_RUN   => {
        gameNo.classList.add("text-primary")
        setKoMatchEditable(row, m.gameNo, true)
        setKoMatchBtn(row, m.gameNo, "save", false)
        setKoMatchBtn(row, m.gameNo, "delete", true)       
      } 
      case MS_FIN   => {
        gameNo.classList.add("text-dark")
        setKoMatchEditable(row, m.gameNo, false)
        row.classList.add("bg-light")
        setKoMatchBtn(row, m.gameNo, "save", true)
        setKoMatchBtn(row, m.gameNo, "delete", false)         
      } 
      case MS_FIX   => {
        setKoMatchEditable(row, m.gameNo, false)
        row.classList.add("bg-secondary")
        row.classList.add("text-white")
        setKoMatchBtn(row, m.gameNo, "save", true)
        setKoMatchBtn(row, m.gameNo, "delete", true)        
      }
      case _        => {}
    }
  }

  // set input fields e.g. balls and sets (not) editable
  def setKoMatchEditable(row: HTMLElement, gameNo: Int, editable: Boolean) = {
    val resultElts =  row.querySelectorAll(s"[data-result_${gameNo}='result']")
    for( i <- 0 to resultElts.length-1) {
      val elem = resultElts.item(i).asInstanceOf[HTMLElement]
      elem.setAttribute("contenteditable", s"${editable}")
    }
  }

  // set buttons for save and delete match
  def setKoMatchBtn(row: HTMLElement, gameNo: Int, name: String, disabled: Boolean) = {
    setDisabled(row.querySelector(s"[data-game_${gameNo}='${name}']").asInstanceOf[HTMLElement], disabled)
  }  

  // set a Ko match element like name, table, info, ...
  def setKoMatchElem(row: HTMLElement, gameNo: Int, name: String, value: String) = {
    setHtml(row.querySelector(s"[data-game_${gameNo}='${name}']").asInstanceOf[HTMLElement], value)
  }

  // return corresponding row element of ko match
  def getKoMatchRow(m: MEntryKo): HTMLElement = {
    try {
      val tableElem = getElemById(s"InputRound_${m.coId}_${m.coPhId}_${m.round}").asInstanceOf[HTMLElement]
      tableElem.querySelector(s"[data-game_${m.gameNo}='row']").asInstanceOf[HTMLElement]
    } catch { case _: Throwable => {
      error("getKoRow", s"coId: ${m.coId} coPhId: ${m.coPhId} round: ${m.round} gameNo: ${m.gameNo}")
      dom.document.createElement("div").asInstanceOf[HTMLElement] 
    }}
  }


}