package scalajs.usecase.organize

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

//import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
//import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.{ Event, HTMLElement, HTMLInputElement, HTMLFormElement, HTMLTableRowElement } // for ScalaJs bindings

import upickle.default._

import shared.utils._
import shared.model._
import shared.model.PantStatus
import shared.model.CompStatus
import shared.utils.Routines._

import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App }


// ***
// Home of organizer register participants
// ***
@JSExportTopLevel("OrganizePlayer")
object OrganizePlayer extends UseCase("OrganizePlayer")  
  with TourneySvc with ViewServices
{
  var player: Seq[(String, String, String, PantStatus.Value, String, Long, CompTyp.Value, CompStatus.Value, String, Long, Long, String)] = Seq()
  var snoSortDirection = true

   /** view4PlayerRegister
   *  returns Sequence of Player (sno, name, clubname, status, coName, coId, coTyp, coStatus, email) 
   */
  def view4PlayerRegister(): Seq[(String, String, String, PantStatus.Value, String, Long, CompTyp.Value, CompStatus.Value, String, Long, Long, String)] = {
    val tourney = App.tourney
    
    // list for single, double or mixed players
    (for {
      p2ce  <- tourney.pl2co.values.toSeq
      comp   = tourney.comps(p2ce.coId) 
    } yield {
      comp.typ match {
        case CompTyp.SINGLE => {
          val pl1 = tourney.players(p2ce.getPlayerId)
          
          (Pant.genSNO(pl1.id, pl1.id), s"${pl1.lastname}, ${pl1.firstname}", pl1.clubName, p2ce.status, comp.name, comp.id, comp.typ, comp.status, pl1.email, pl1.id, 0L, pl1.getLicense.value)
        }
        case CompTyp.DOUBLE | CompTyp.MIXED => {
          val (plId1, plId2) = p2ce.getDoubleId
          val (pl1, pl2) = (tourney.players(plId1), tourney.players(plId2))
          (Pant.genSNO(pl1.id, pl2.id), s"${pl1.lastname}/${pl2.lastname}", s"${pl1.clubName}/${pl2.clubName}", p2ce.status, comp.name, comp.id, comp.typ, comp.status, pl1.email, pl1.id, pl2.id, "")
        }
        case _ => ("", "", "", PantStatus.UNKN, "", 0L, CompTyp.Typ, CompStatus.UNKN, "", 0L, 0L, "")
      }
    }).toSeq
  }


  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) =  { 
    println("Start Player setMain Content")
    setMainContent(clientviews.organize.html.Player())
    update()
  }  


  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    player = view4PlayerRegister()

    setHtml(gUE("MainCard"), clientviews.organize.player.html.MainCard(player.filter(_._4 >= PantStatus.REGI))) 
    setHtml(gUE("WaitCard"), clientviews.organize.player.html.WaitCard(player.filter(_._4 == PantStatus.WAIT))) 
    setHtml(gUE("RejectCard"), clientviews.organize.player.html.RejectCard(player.filter(_._4 == PantStatus.RJEC)))
  
    // check if there are players with need signup commitment
    setBadge(player.filter(_._4 == PantStatus.PEND.id).length)
  }       

  def setBadge(cnt: Int = (-1)) = {
    val sicoCnt = if (cnt == -1 ) view4PlayerRegister.filter(_._4 == PantStatus.PEND).length else cnt    
    if (sicoCnt > 0) {
       // add badge if not already added, do it now
      setHtml(gUE("SidebarBadge"), sicoCnt.toString)
      setHtml(gUE("CardBadge"), if (sicoCnt == 1) getMsg("badge.waiting.one") else getMsg("badge.waiting.more", sicoCnt.toString))    
    }
    //setVisible("SidebarBadge", sicoCnt > 0)
    setVisible("CardBadge", sicoCnt > 0)
  } 

  private def toggleSortDirection(elem: HTMLElement) = {
    if (elem.classList.contains("fa-sort")) {
      elem.classList.remove("fa-sort")
      elem.classList.add("fa-sort-asc")
    } else if (elem.classList.contains("fa-sort-asc")) {
      elem.classList.remove("fa-sort-asc")
      elem.classList.add("fa-sort-desc")
    } else if (elem.classList.contains("fa-sort-desc")) {
      elem.classList.remove("fa-sort-desc")
      elem.classList.add("fa-sort")      
    }
  }  

  private def getSortDirection(elem: HTMLElement): Int = {
    if (elem.classList.contains("fa-sort-asc"))        1
    else if (elem.classList.contains("fa-sort-desc")) -1
    else                                               0
  }

  def sortByName(): Unit = {
    toggleSortDirection(gE(uc("SortName")))
    sortStatus(); sortComp(); sortName()
    setContent(clientviews.organize.player.html.ContentCard(player).toString)
  } 

  def sortByComp(): Unit = {
    toggleSortDirection(gE(uc("SortComp")))
    sortName(); sortStatus(); sortComp()
    setContent(clientviews.organize.player.html.ContentCard(player).toString)
  }  

  def sortBySNO(): Unit = {
    val pList = if (snoSortDirection) {
      player.filter(_._4 >= PantStatus.REGI).sortBy( x => if (x._11 == 0) x._10 else x._10 + 1000000 )
    } else {
      player.filter(_._4 >= PantStatus.REGI).sortBy( x => if (x._11 == 0) x._10 else x._10 + 1000000 ).reverse    
    }
    snoSortDirection = !snoSortDirection
    setContent(clientviews.organize.player.html.ContentCard(pList).toString)
  } 

  def sortByStatus(): Unit = {
    toggleSortDirection(gE(uc("SortStatus")))
    sortName(); sortComp(); sortStatus()
    setContent(clientviews.organize.player.html.ContentCard(player).toString)
  } 

  def sortName() = getSortDirection(gE(uc("SortName"))) match {
    case -1 => player = player.sortBy(_._2.toLowerCase).reverse 
    case  1 => player = player.sortBy(_._2.toLowerCase)
    case  _ => {} 
  }

  def sortStatus() = getSortDirection(gE(uc("SortStatus"))) match {
    case -1 => player = player.sortBy(_._4).reverse 
    case  1 => player = player.sortBy(_._4)
    case  _ => {} 
  }  

  def sortComp() = getSortDirection(gE(uc("SortComp"))) match {
    case -1 => player = player.sortBy(_._5).reverse 
    case  1 => player = player.sortBy(_._5)
    case  _ => {} 
  }    

  def setContent(content: String) = {
    val rows = gE(uc("MainCard")).querySelectorAll("tr[data-update]")
    rows.map( row => row.parentNode.removeChild(row))
    gE(uc("PlayerDummy")).asInstanceOf[HTMLTableRowElement].insertAdjacentHTML("afterend", content)
  }

  @JSExport
  def buttonSetStatus(coId: Long, sno: String, status: PantStatus.Value): Unit = {

    def setPlayerStatusUpdate(coId: Long, sno: String, status: PantStatus.Value): Unit = {
      App.tourney.pl2co(sno, coId).status = status
      setPantStatus(coId, SNO(sno), status).map { _ => update() }
    }

    status match {
      case PantStatus.REGI => confirm(coId, sno, PantStatus.REGI).map { if (_) setPlayerStatusUpdate(coId, sno, PantStatus.REGI) }
      case PantStatus.WAIT => confirm(coId, sno, PantStatus.WAIT).map { if (_) setPlayerStatusUpdate(coId, sno, PantStatus.WAIT) }
      case PantStatus.RJEC => confirm(coId, sno, PantStatus.RJEC).map { if (_) setPlayerStatusUpdate(coId, sno, PantStatus.RJEC) }
      case PantStatus.REDY => setPlayerStatusUpdate(coId, sno, PantStatus.REDY)
      case              _  => error("buttonSetStatus", s"error unknown status"); setPlayerStatusUpdate(coId, sno, PantStatus.UNKN)
    }
  }  

  @JSExport
  def onchangeCheckActiv(elem: HTMLInputElement): Unit = {
    val coId = getCoId(elem)
    val sno = SNO(getSNO(elem))
    val status = if (elem.checked) PantStatus.REDY else PantStatus.REGI
    setPantStatus(coId, sno, status).map { 
      case Left(err)  => error("onclickCheckActiv", s"coId: ${coId} status: ${status} sno: ${sno.value}") 
      case Right(res) => info("onclickCheckActiv", s"coId: ${coId} status: ${status} sno: ${sno.value}") 
    }
  }

  @JSExport
  def onclickSort(elem: dom.raw.HTMLElement, sortByTyp: String): Unit = {
    import shared.utils.PlayerSortTyp
    println(s"onclickSort ${sortByTyp}")
    PlayerSortTyp(sortByTyp.toIntOption.getOrElse(0)) match {
      case PlayerSortTyp.SNO         => sortBySNO()
      case PlayerSortTyp.Name        => sortByName()
      case PlayerSortTyp.Competition => sortByComp()    
      case PlayerSortTyp.Status      => sortByStatus() 
      case PlayerSortTyp.UNKNOWN     => println(s"SortBy: unknown")
    }
  } 

  @JSExport
  def onclickEdit(elem: dom.raw.HTMLElement): Unit = {
    val pl1 = getData(elem, "plId1", 0L)
    val pl2 = getData(elem, "plId2", 0L)
    println(s"PlayerIds: ${pl1} ${pl2}")
    DlgCardPlayer.show(pl1, App.tourney)
  } 


  /** confirm - give optional feedback to user about new status
   */ 
  def confirm(coId: Long, sno: String, status: PantStatus.Value): Future[Boolean] = {
    import scalajs.usecase.dialog.DlgBox
    val email     = App.tourney.players(getMDLongArr(sno)(0)).email
    val firstname = App.tourney.contact.firstname

    val dlgMsg = status match {
      case PantStatus.RJEC => (getMsg("hdr.reject"),getMsg("body.reject"),getMsg("email.reject",email,firstname))
      case PantStatus.WAIT => (getMsg("hdr.wait"),getMsg("body.wait"),getMsg("email.wait",email,firstname))
      case PantStatus.REGI => (getMsg("hdr.confirm"),getMsg("body.confirm"),getMsg("email.confirm",email,firstname))
      case               _ => ("UNKNOWN","message","")
    }

    DlgBox.standard(dlgMsg._1, dlgMsg._2, Seq("cancel", "no", "yes"),0,true)
      .map { res => res match {
        case 1 => debug("confirm", "cancel"); false
        case 2 => debug("confirm", "no"); true
        case 3 => {
          dom.window.location.href = dlgMsg._3
          debug("confirm", "yes"); true
        }  
        case _ => debug("confirm", "unknown"); false
      }}
  }
}