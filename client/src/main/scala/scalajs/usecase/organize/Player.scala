package scalajs.usecase.organize

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.{ Event, HTMLInputElement, HTMLFormElement } // for ScalaJs bindings

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
  var player: Seq[(String, String, String, Int, String, Long, CompTyp.Value, CompStatus.Value, String)] = Seq()

   /** view4PlayerRegister
   *  returns Sequence of Player (id, name, clubname, status, coName, coId, coTyp, coStatus, email) 
   */
  def view4PlayerRegister(): Seq[(String, String, String, Int, String, Long, CompTyp.Value, CompStatus.Value, String)] = {
    val tourney = App.tourney
    
    // list for single players
    val pList = (for {
      p2ce  <- tourney.pl2co.values.toSeq
      comp   = tourney.comps(p2ce.coId) 
    } yield {
      if (comp.typ == 1) {
        val pl1 = tourney.players(p2ce.getPlayerId)
        (PantEntry.genSNO(pl1.id, pl1.id), s"${pl1.lastname}, ${pl1.firstname}", pl1.clubName, p2ce.status.id, comp.name, comp.id, comp.typ, comp.status, pl1.email)
      } else {
        val (plId1, plId2) = p2ce.getDoubleId
        val (pl1,pl2) = (tourney.players(plId1), tourney.players(plId2))
        //val pl2 = tourney.players(plId2)
        (PantEntry.genSNO(pl1.id, pl2.id), s"${pl1.lastname}/${pl2.lastname}", s"${pl1.clubName}/${pl2.clubName}", p2ce.status.id, comp.name, comp.id, comp.typ, comp.status, pl1.email)
      }
    }).toSeq
    pList
  }


  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) =  { 
    setMainContent(clientviews.organize.html.Player())
    update()
  }  


  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    player = view4PlayerRegister()
    setHtml("MainCard", clientviews.organize.player.html.MainCard(player.filter(_._4 >= PantStatus.REGI.id))) 
    setHtml("WaitCard", clientviews.organize.player.html.WaitCard(player.filter(_._4 == PantStatus.WAIT.id))) 
    setHtml("RejectCard", clientviews.organize.player.html.RejectCard(player.filter(_._4 == PantStatus.RJEC.id)))
  
    // check if there are players with need signup commitment
    setBadge(player.filter(_._4 == PantStatus.REGI.id).length)
  }       

  def setBadge(cnt: Int = (-1)) = {
    val sicoCnt = if (cnt == -1 ) view4PlayerRegister.filter(_._5 == PantStatus.REGI).length else cnt    
    if (sicoCnt > 0) {
       // add badge if not already added, do it now
      setHtml("SidebarBadge", sicoCnt.toString)
      if (sicoCnt == 1) {
        setHtml("CardBadge", getMsg("badge.waiting.one"))
      } else {
        setHtml("CardBadge", getMsg("badge.waiting.more", sicoCnt.toString))
      }   
    }
    setVisible("SidebarBadge", sicoCnt > 0)
    setVisible("CardBadge", sicoCnt > 0)
  } 

  private def setClass(name: String, value: Int) = {
    value match {
      case  1 => setAttribute(gE(name,ucp), "class", "fa fa-sort-asc")
      case -1 => setAttribute(gE(name,ucp), "class", "fa fa-sort-desc")
      case  _ => setAttribute(gE(name,ucp), "class", "fa fa-sort")
    }  
  }


  def sortByName(): Unit = {
    val pList = getTextContent("SortByName") match {
      case  "1" =>  { setHtml("SortByName","-1"); player.sortBy(_._2).reverse }  
      case "-1" =>  { setHtml("SortByName","0");  player.sortBy(_._1) } 
      case   _  =>  { setHtml("SortByName","1");  player.sortBy( _._2) } 
    }
    val pList2 = getTextContent("SortByComp") match {
      case "-1"  => pList.sortBy(_._6).reverse
      case  "1"  => pList.sortBy(_._6)
      case   _   => pList
    }
    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList2))
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0) )
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0) )
  } 

  def sortByComp(): Unit = {
    val pList = getTextContent("SortByComp") match {
      case  "1" => { setHtml("SortByComp","-1"); player.sortBy(_._6).reverse }  
      case "-1" => { setHtml("SortByComp","0"); player.sortBy(_._1) } 
      case  _   => { setHtml("SortByComp","1"); player.sortBy(_._6) }        
    }
    val pList2 = getTextContent("SortByName") match {
      case "-1" => pList.sortBy(_._2).reverse
      case  "1" => pList.sortBy(_._2)
      case    _ => pList
    }
    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList2).toString)
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0))
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0))
  }  


  def sortBySNO(): Unit = {
    setHtml("SortByComp","0")
    setHtml("SortByName","0")
    val pList = getTextContent("SortByNo") match {
      case "0"    =>  setHtml("SortByNo","1"); player.filter(_._4 >= PantStatus.REGI.id).sortBy(_._1)
      case  _     =>  setHtml("SortByNo","0"); player.filter(_._4 >= PantStatus.REGI.id).sortBy(_._1).reverse
    }

    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList).toString)
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0))
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0))
  } 


  def sortByStatus(): Unit = {
    setHtml("SortByComp","0")
    setHtml("SortByName","0")
    val pList = getTextContent("SortByStatus") match {
      case "0" =>  setHtml("SortByStatus","1"); player.sortBy(_._4)
      case _   =>  setHtml("SortByStatus","0"); player.sortBy(_._4).reverse
    }

    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList).toString)
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0))
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0))
  } 


  @JSExport
  def buttonSetStatus(coIdStr: String, sno: String, status: String): Unit = {

    def setPlayerStatusUpdate(coId: Long, sno: String, status: PantStatus.Value): Unit = {
      App.tourney.pl2co(sno, coId).status = status
      setPantStatus(coId, sno, status).map { _ => update() }
    }

    val coId = coIdStr.toLongOption.getOrElse(0L)
    PantStatus(status) match {
      case PantStatus.REGI => confirm(coId, sno, PantStatus.REGI).map { if (_) setPlayerStatusUpdate(coId, sno, PantStatus.REGI) }
      case PantStatus.WAIT => confirm(coId, sno, PantStatus.WAIT).map { if (_) setPlayerStatusUpdate(coId, sno, PantStatus.WAIT) }
      case PantStatus.RJEC => confirm(coId, sno, PantStatus.RJEC).map { if (_) setPlayerStatusUpdate(coId, sno, PantStatus.RJEC) }
      case PantStatus.REDY => setPlayerStatusUpdate(coId, sno, PantStatus.REDY)
      case              _  => error("buttonSetStatus", s"error unknown status"); setPlayerStatusUpdate(coId, sno, PantStatus.UNKN)
    }
  }  

  @JSExport
  def onclickCheckActiv(elem: HTMLInputElement, coIdStr: String, plIdStr: String): Unit = {
    buttonSetStatus(coIdStr, plIdStr, if (elem.checked) "PLS_REDY" else "PLS_UNDO")
  }

  @JSExport
  def onclickSort(elem: dom.raw.HTMLElement, sortByTyp: String): Unit = {
    import shared.utils.PlayerSortTyp
    PlayerSortTyp(sortByTyp.toIntOption.getOrElse(0)) match {
      case PlayerSortTyp.SNO         => sortBySNO()
      case PlayerSortTyp.Name        => sortByName()
      case PlayerSortTyp.Competition => sortByComp()
      case PlayerSortTyp.Status      => sortByStatus()
      case PlayerSortTyp.UNKNOWN     => println(s"SortBy: unknown")
    }
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
      case        _ => ("UNKNOWN","message","")
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