package scalajs.usecase.organize

// Start TestCases
// http://localhost:9000/start?ucName=TestMain&ucParam=OrganizeTourney


import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
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
import shared.model.Tourney._
import shared.model.Competition._
import shared.utils.Constants._ 
import shared.utils.Routines._

import scalajs.usecase.dialog.{ DlgBox, DlgSpinner, DlgPlayfield }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Home of organizer
// *** 
@JSExportTopLevel("OrganizeTourney")
object OrganizeTourney extends UseCase("OrganizeTourney")  
  with TourneySvc with AppHelperSvc
{
  import scalajs.usecase.component.BasicHtml._
  import clientviews.organize.tourney.html
  import scalajs.usecase.dialog._
  var tournBases: Seq[TournBase] = Seq()

  // render view
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    setMainContent(clientviews.organize.html.Tourney(App.tourney.getToId).toString)
    update()
  } 

  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    getTournBases(AppEnv.getOrgDir).map { 
      case Left(err)    => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
      case Right(trnys) => {
        val toId = App.tourney.getToId
        tournBases = trnys
        //debug("update", s"TournBases: ${tournBases}")
        setHtml("TourneyCard", html.TourneyCard(toId, trnys).toString)

        selTableRow(uc(s"TableRow_${toId}"))
        setViewTournBase(App.tourney)
      }  
    }
    OrganizePlayer.setBadge()
  }


  @JSExport
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    showResult(false)
    key match {
      case "UploadChange" => setHtmlVisible("UploadError", false)
      case "Download"     => {

      }  
      // Upload ClickTT File
      case ULD_CLICKTT    => {
        val fName   = getInput("Input" + key, "")
        // debug("actionEvent", s"Upload fName: ${fName} uldType: ${key}")
        if (fName == "") setHtmlVisible("UploadError", true, getMsg("upload.noFile")) else { 
          setHtmlVisible("UploadError", false)
          if (App.tourney.getToId != 0 ) {
            // want update tournament/event ?           
            DlgBox.showStd(getMsg("upload.msgbox.header"), getMsg("upload.msgbox.body1", App.getTourneyName()),
              Seq("cancel", "update", "new"),0,true).map { result => result match {
                case 2 => doCttUpload(UploadModeUpdate)
                case 3 => doCttUpload(UploadModeNew) 
                case _ => debug("buttonUpload", "cancel")
              }}         
          } else {
            // want create new tournament ?
            DlgBox.showStd(getMsg("upload.msgbox.header"),getMsg("upload.msgbox.body2"), Seq("no", "yes"),0,true)
              .map { result => result match {
                case 2 => doCttUpload(UploadModeNew) 
                case _ => debug("buttonUpload", "cancel")
              }}                        
          }
        }  
      }

      // Upload invitation file
      case ULD_INVIT  => {
        val fName   = getInput("Input" + key, "")
        // debug("actionEvent", s"Upload fName: ${fName} uldType: ${key}")
        if (fName == "") {
          setHtmlVisible("UploadError", true, getMsg("upload.noFile")) 
        } else if (App.tourney.getToId == 0) {
          setHtmlVisible("UploadError", true, getMsg("upload.notTournSel")) 
        } else {
          doUpload(key)
        }
      }

      // Upload logo, cert or banner
      case ULD_LOGO | ULD_CERT | ULD_BANNER => {
        val fName   = getInput("Input" + key, "")
        if (fName == "") setHtmlVisible("UploadError", true, getMsg("upload.noFile")) else { 
          setHtmlVisible("UploadError", false)
          doUpload(key)
        }  
      }

      // Click on new tourney button
      case "New"   => {
        DlgCardTourney.show("new", TournBase("", AppEnv.getOrganizer, AppEnv.getOrgDir, getNow(), getNow(), "", TTY_UNKN, true, "", "" , 0L)).map {
          case Left(err) => {} // only cancel as error DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
          case Right(tb) => addTournBase(tb).map {
            case Left(err)   => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
            case Right(trny) => App.setLocalTourney(trny); update()
          }
        }  
      }

      // Click edit button on list
      case "Edit"   => {
        event.stopPropagation()
        val toId = getData(elem, "toId", 0L)
        if (toId == App.tourney.getToId & toId != 0) DlgCardTourney.show("edit", App.tourney.getBase()).map {
          case Left(err)  => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
          case Right(tB)  => setTournBase(tB).map {
            case Left(err)   => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
            case Right(trny) => App.setLocalTourney(trny); update()
          }  
        }
      } 

      // Click edit button on list
      case "View"   => {
        event.stopPropagation()
        val toId = getData(elem, "toId", 0L)
        if (toId == App.tourney.getToId & toId != 0) DlgCardTourney.show("view", App.tourney.getBase()).map {
          case Left(err)  => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
          case Right(tB)  => setTournBase(tB).map {
            case Left(err)   => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
            case Right(trny) => App.setLocalTourney(trny); update()
          }  
        }
      }        

      // Select new tourney on list
      case "Select"   => { 
        val toId = getData(elem, "toId", 0L) 
        if (getData(elem, "toId", 0L) != App.tourney.getToId) {
          App.loadRemoteTourney(getData(elem, "toId", 0L)).map {
            case Left(err)  => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
            case Right(res) => highlight(elem)
          }
        }
      }  

      // Click dustbin on list
      case "Delete"   => { 
        val toId = getData(elem, "toId", 0L)
        event.stopPropagation()
        if (toId == App.tourney.getToId & toId != 0) {
          confirmDlg(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", App.tourney.name)).map { _ =>
            delTourney(toId).map { 
              case Left(err)  => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
              case Right(res) => { App.resetLocalTourney(); render() }
            }
          }
        }
      }

      case _          => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }
  


  // doUpload create new tournament/event if toId equals 0  
  def doUpload(upType: String)  = {
    
    // perform upload of tourney information file (xml, markdown, or image)
    val fileForm = dom.document.getElementById(getId("UploadForm" + "_" + upType)).asInstanceOf[dom.raw.HTMLFormElement]
    val formData = new dom.FormData(fileForm)
      
    // formData.append("toId",   App.tourney.getToId) 
    // formData.append("uptype", upType)
    // formData.append("sdate",  sdate.toString)
    // formData.append("edate",  edate.toString)
      
    DlgSpinner.show(0, getMsg("upload.spinner")) 
    
    uploadFile(App.tourney.getToId, App.tourney.startDate, upType, formData).map {
      case Left(err)  => println(s"${err}"); DlgSpinner.show(2, getMsg("upload.error.0")) 
      case Right(res) => DlgSpinner.show(1, s"TourneyId: ${res}")
    } 
  }


  // doUpload create new tournament/event if toId equals 0  
  def doCttUpload(upMode: Int)  = {
    import shared.utils.Constants._
    
    // perform upload of tourney information file (xml, markdown, or image)
    val fileForm = dom.document.getElementById(getId("UploadForm" + "_" + ULD_CLICKTT)).asInstanceOf[dom.raw.HTMLFormElement]
    val formData = new dom.FormData(fileForm)
      
    DlgSpinner.show(0, getMsg("upload.spinner")) 

    upMode match {
      case UploadModeUpdate => {
        updCttFile(App.tourney.getToId, App.tourney.startDate, formData).map {
          case Left(err)  => println(s"${err}"); DlgSpinner.show(2, getMsg("upload.error.0")) 
          case Right(res) => DlgSpinner.show(1, s"TourneyId: ${res}")
        } 
      }
      case UploadModeNew    => {
        newCttFile(App.tourney.getToId, App.tourney.startDate, formData).map {
          case Left(err)  => println(s"${err}"); DlgSpinner.show(2, getMsg("upload.error.0")) 
          case Right(res) => DlgSpinner.show(1, s"TourneyId: ${res}")
        } 
      }

    }
    

  }


  // ***
  // Usecase: Organize Tourney DOWNLOAD
  // ***

  // buttonDownloadNotifyList - click on download button
  @JSExport
  def buttonDownloadNotifyList() = {
    debug("buttonDownloadNotifyList", "click")

    val bPB  = Map("type" -> "text/comma-separated-values").asInstanceOf[js.Dictionary[js.Any]]
    val blob = new dom.Blob(js.Array(genNotificationList), bPB.asInstanceOf[dom.raw.BlobPropertyBag])

    val anchor = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
    val href   = dom.raw.URL.createObjectURL(blob)
    anchor.href = href
    $(anchor).attr("download",getMsg("download.notify.filename"))
    dom.document.body.appendChild(anchor)
    anchor.click()
    dom.document.body.removeChild(anchor)
    dom.raw.URL.revokeObjectURL(href)
  }

     
  /** genNotificationList
   *  returns Sequence of Players
   */
  def genNotificationList() : String = {
    val buf = new StringBuffer(getMsg("download.notify.filehdr"))
    buf.append("\n")

    val tourney = App.tourney
    val player  = App.tourney.players
    for {
      co    <- tourney.comps.values
      plco  <- tourney.pl2co.values.filter(_.coId==co.id).filter(_.status >= Pant.SICO)
    } yield {
      if (co.typ == CT_SINGLE) {
        buf.append(s"${plco.sno},${player(plco.getPlayerId).lastname},${player(plco.getPlayerId).firstname},${player(plco.getPlayerId).getTTR},${player(plco.getPlayerId).clubName},${co.name}\n")
      }  
    }
    buf.toString
  }  

  /** highlight selected element
   * 
   */ 
  def highlight(elem: dom.raw.HTMLElement) = {
    
    $(elem).addClass("bg-secondary").siblings().removeClass("bg-secondary")
    $(elem).addClass("text-white").siblings().removeClass("text-white")
  }

  /** setViewTournBase set the view of the base part of the tourney
   * 
   */ 
  def setViewTournBase(trny: Tourney): Unit = {
    import shared.utils.Routines._

    if (trny.id == 0) {
      collapse("TourneyCard", true)
    } else {
      collapse("TourneyCard", false)
    }
  }


}