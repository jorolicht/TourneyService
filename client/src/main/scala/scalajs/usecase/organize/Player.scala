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
import shared.model.Pant
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
  var player: Seq[(Long, String, String, String, Int, String, Long, Int, Int, String)] = Seq()

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) =  { 
    setMainContent(clientviews.organize.html.Player())
    update()
  }  


  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    player = view4PlayerRegister()
    setHtml("MainCard", clientviews.organize.player.html.MainCard(player.filter(_._5 >= Pant.SICO))) 
    setHtml("WaitCard", clientviews.organize.player.html.WaitCard(player.filter(_._5 == Pant.WAIT))) 
    setHtml("RejectCard", clientviews.organize.player.html.RejectCard(player.filter(_._5 == Pant.RJEC)))
  
    // check if there are players with need signup commitment
    setBadge(player.filter(_._5 == Pant.SICO).length)
  }       

  def setBadge(cnt: Int = (-1)) = {
    val sicoCnt = if (cnt == -1 ) view4PlayerRegister.filter(_._5 == Pant.SICO).length else cnt    
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
      case  1 => setAttribute(name, "class", "fa fa-sort-asc")
      case -1 => setAttribute(name, "class", "fa fa-sort-desc")
      case  _ => setAttribute(name, "class", "fa fa-sort")
    }  
  }


  @JSExport
  def sortByName(): Unit = {
    val pList = getTextContent("SortByName") match {
      case  "1" =>  { setHtml("SortByName","-1"); player.sortBy(_._3).reverse }  
      case "-1" =>  { setHtml("SortByName","0");  player.sortBy(_._1) } 
      case   _  =>  { setHtml("SortByName","1");  player.sortBy( _._3) } 
    }
    val pList2 = getTextContent("SortByComp") match {
      case "-1"  => pList.sortBy(_._7).reverse
      case  "1"  => pList.sortBy(_._7)
      case   _   => pList
    }
    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList2))
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0) )
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0) )
  } 

  @JSExport
  def sortByComp(): Unit = {
    val pList = getTextContent("SortByComp") match {
      case  "1" => { setHtml("SortByComp","-1"); player.sortBy(_._7).reverse }  
      case "-1" => { setHtml("SortByComp","0"); player.sortBy(_._1) } 
      case  _   => { setHtml("SortByComp","1"); player.sortBy(_._7) }        
    }
    val pList2 = getTextContent("SortByName") match {
      case "-1" => pList.sortBy(_._3).reverse
      case  "1" => pList.sortBy(_._3)
      case    _ => pList
    }
    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList2).toString)
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0))
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0))
  }  

  @JSExport
  def sortByNo(): Unit = {
    setHtml("SortByComp","0")
    setHtml("SortByName","0")
    val pList = getTextContent("SortByNo") match {
      case "0"    =>  setHtml("SortByNo","1"); player.filter(_._5 > -2).sortBy(_._1)
      case  _     =>  setHtml("SortByNo","0"); player.filter(_._5 > -2).sortBy(_._1).reverse
    }

    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList).toString)
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0))
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0))
  } 

  @JSExport
  def sortByStatus(): Unit = {
    setHtml("SortByComp","0")
    setHtml("SortByName","0")
    val pList = getTextContent("SortByStatus") match {
      case "0" =>  setHtml("SortByStatus","1"); player.sortBy(_._5)
      case _   =>  setHtml("SortByStatus","0"); player.sortBy(_._5).reverse
    }

    setHtml("MainCard", clientviews.organize.player.html.MainCard(pList).toString)
    setClass("SortNameIcon", getTextContent("SortByName").toIntOption.getOrElse(0))
    setClass("SortCompIcon", getTextContent("SortByComp").toIntOption.getOrElse(0))
  } 


  @JSExport
  def buttonSetStatus(coIdStr: String, sno: String, status: String): Unit = {
    // get player ident from string
    def getIds(s: String): (Long, Long) = {
      if (s.contains("/")) {
        val lArr = s.split("/")
        ( lArr(0).toLongOption.getOrElse(0L), lArr(1).toLongOption.getOrElse(0L)  )
      } else {
        (s.toLongOption.getOrElse(0L), 0L)
      }
    }

    def setPlayerStatusUpdate(coId: Long, sno: String, status: Int): Unit = {
      App.tourney.pl2co(sno, coId).status = status
      setPantStatus(coId, sno, status).map { _ => update() }
    }

    val coId = coIdStr.toLongOption.getOrElse(0L)
    status match {
      case "PLS_SIGN" => confirm(coId, sno, Pant.SIGN).map { if (_) setPlayerStatusUpdate(coId, sno, Pant.SIGN) }
      case "PLS_WAIT" => confirm(coId, sno, Pant.WAIT).map { if (_) setPlayerStatusUpdate(coId, sno, Pant.WAIT) }
      case "PLS_RJEC" => confirm(coId, sno, Pant.RJEC).map { if (_) setPlayerStatusUpdate(coId, sno, Pant.RJEC) }
      case "PLS_REDY" => setPlayerStatusUpdate(coId, sno, Pant.REDY)
      case "PLS_UNDO" => setPlayerStatusUpdate(coId, sno, Pant.SIGN)
      case         _  => error("buttonSetStatus", s"error unknown status")
    }
  }  

  @JSExport
  def onclickCheckActiv(elem: HTMLInputElement, coIdStr: String, plIdStr: String): Unit = {
    buttonSetStatus(coIdStr, plIdStr, if (elem.checked) "PLS_REDY" else "PLS_UNDO")
  }


  /** confirm - give optional feedback to user about new status
   *          status: PLS_RJEC = -3  rejected
   *                  PLS_WAIT = -2  waiting list
   *                  PLS_SIGN =  0  signup confirmed
   */ 

  def confirm(coId: Long, sno: String, status: Int): Future[Boolean] = {
    import scalajs.usecase.dialog.DlgBox
    val email     = App.tourney.players(getMDLongArr(sno)(0)).email
    val firstname = App.tourney.contact.firstname

    val dlgMsg = status match {
      case Pant.RJEC => (getMsg("hdr.reject"),getMsg("body.reject"),getMsg("email.reject",email,firstname))
      case Pant.WAIT => (getMsg("hdr.wait"),getMsg("body.wait"),getMsg("email.wait",email,firstname))
      case Pant.SIGN => (getMsg("hdr.confirm"),getMsg("body.confirm"),getMsg("email.confirm",email,firstname))
      case        _ => ("UNKNOWN","message","")
    }

    DlgBox.showStd(dlgMsg._1, dlgMsg._2, Seq("cancel", "no", "yes"),0,true)
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