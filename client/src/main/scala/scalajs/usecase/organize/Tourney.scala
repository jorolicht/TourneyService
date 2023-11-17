package scalajs.usecase.organize

import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._


import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.{ Event, HTMLInputElement, HTMLFormElement } // for ScalaJs bindings

import shared.utils._
import shared.model._
import shared.model.PantStatus
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
object OrganizeTourney extends UseCase("OrganizeTourney") with TourneySvc 
{
  import clientviews.organize.tourney.html
  import scalajs.usecase.dialog._
  var tournBases: Seq[TournBase] = Seq()

  // render view
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    setMainContent(clientviews.organize.html.Tourney(App.tourney.id))
    update()
  } 

  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    getTournBases(AppEnv.getOrgDir).map { 
      case Left(err)    => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
      case Right(trnys) => {
        val toId = App.tourney.id
        tournBases = trnys
        //debug("update", s"TournBases: ${tournBases}")
        setHtml("TourneyCard", html.TourneyCard(toId, trnys))

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
      
      case s"DownloadType_$id"    => {
        val dloType = DownloadType(id.toIntOption.getOrElse(0))
        dloType match {
          case DownloadType.ClickTT => println("Download ClickTT"); actionDownloadClickTT()
          case DownloadType.Player  => println("Download Player");  actionDownloadPlayer() 
          case DownloadType.UNKNOWN => println("Download UNKNOWN") 
        }
      } 

      // do file upload depending on upload type
      case s"UploadType_$id"    => {
        val uplType = UploadType(id.toIntOption.getOrElse(0))
        val fName   = getInput(gE(uc(s"Input_${uplType.id}")))
        uplType match {
          case UploadType.ClickTT => println("Upload ClickTT"); actionUploadClickTT(fName) 
          case UploadType.Invite  => println("Upload Invite");  actionUploadInvite(fName) 
          case UploadType.Logo    => println("Upload Logo");    actionUploadGeneric(fName, uplType)
          case UploadType.Cert    => println("Upload Cert");    actionUploadGeneric(fName, uplType)
          case UploadType.Banner  => println("Upload Banner");  actionUploadGeneric(fName, uplType)
          case UploadType.UNKNOWN => println("Upload UNKNOWN")
        }   
      }  

          // do file upload depending on upload type
      case s"TourneyAction_$id"    => {
        val trnyActionType = TourneyAction(id.toIntOption.getOrElse(0))
        val toId = getData(elem, "toId", 0L)
        event.stopPropagation()
        trnyActionType match {
          case TourneyAction.New     => println("Tourney Action New");    actionTourneyNew() 
          case TourneyAction.Edit    => println("Tourney Action Edit");   actionTourneyEdit(toId) 
          case TourneyAction.View    => println("Tourney Action View");   actionTourneyView(toId)
          case TourneyAction.Delete  => println("Tourney Action Delete"); actionTourneyDelete(toId)
          case TourneyAction.Select  => println("Tourney Action Select"); actionTourneySelect(toId, elem)
          case TourneyAction.UNKNOWN => println("Tourney Action Unknown")
        }   
      }

      case _          => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }


  /**
   ** Action Routines
   */

  // Click edit button on list
  def actionTourneyEdit(toId: Long) = {
    if (toId == App.tourney.getToId & toId != 0) DlgCardTourney.show("edit", App.tourney.getBase()).map {
      case Left(err)  => if (err != Error("dlg.canceled")) DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
      case Right(tB)  => setTournBase(tB).map {
        case Left(err)   => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
        case Right(trny) => App.setLocalTourney(trny); update()
      }  
    }
  } 


  // Click viewbutton on list
  def actionTourneyView(toId: Long) = {
    if (toId == App.tourney.getToId & toId != 0) DlgCardTourney.show("view", App.tourney.getBase()).map {
      case Left(err)  => if (err != Error("dlg.canceled")) DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
      case Right(tB)  => setTournBase(tB).map {
        case Left(err)   => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
        case Right(trny) => App.setLocalTourney(trny); update()
      }  
    }
  }


  // Select new tourney on list
  def actionTourneySelect(toId: Long, elem: dom.raw.HTMLElement) = 
    if (toId != App.tourney.getToId) {
      App.loadRemoteTourney(toId).map {
        case Left(err)  => if (err != Error("dlg.canceled")) DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
        case Right(res) => highlight(elem)
      }
    }
    

  // Click dustbin on list
  def actionTourneyDelete(toId: Long) = 
    if (toId == App.tourney.getToId & toId != 0) 
      dlgCancelOk(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", App.tourney.name)) { 
        delTourney(toId).map { 
          case Left(err)  => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
          case Right(res) => { App.resetLocalTourney(); render() }
        }        
      }



  // Click on new tourney button
  def actionTourneyNew() = 
    DlgCardTourney.show("new", TournBase("", AppEnv.getOrganizer, AppEnv.getOrgDir, getNow(), getNow(), "", TourneyTyp.UNKN.id, true, "", "" , 0L)).map {
      case Left(err) => if (err != Error("dlg.canceled")) DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
      case Right(tb) => addTournBase(tb).map {
        case Left(err)   => DlgInfo.show(getMsg("dlg.error"), getError(err), "danger")
        case Right(trny) => App.setLocalTourney(trny); update()
      }
    }  


  // perform upload of clickTT file, either update or new
  def actionUploadClickTT(fName: String) = 
    if (fName == "") setHtmlVisible("UploadError", true, getMsg("upload.noFile")) else { 
      setHtmlVisible("UploadError", false)
      if (App.tourney.getToId != 0 ) {
        // want update tournament/event ?           
        DlgBox.standard(getMsg("upload.msgbox.header"), getMsg("upload.msgbox.body1", App.tourney.name),
          Seq("cancel", "update", "new"),0,true).map { result => result match {
            case 2 => doUpload(UploadType.ClickTT, UploadMode.Update)
            case 3 => doUpload(UploadType.ClickTT, UploadMode.New) 
            case _ => debug("buttonUpload", "cancel")
          }}         
      } else {
        // want create new tournament ?
        DlgBox.standard(getMsg("upload.msgbox.header"),getMsg("upload.msgbox.body2"), Seq("no", "yes"),0,true)
          .map { result => result match {
            case 2 => doUpload(UploadType.ClickTT, UploadMode.New) 
            case _ => debug("buttonUpload", "cancel")
          }}                        
      }
    } 

  def actionUploadGeneric(fName: String, uplType: UploadType.Value) = 
    if (fName.isEmpty()) setHtmlVisible("UploadError", true, getMsg("upload.noFile")) else { 
      setHtmlVisible("UploadError", false)
      doUpload(uplType, UploadMode.UNKNOWN)
    }  
    
  def actionUploadInvite(fName: String) =   
    if (fName.isEmpty())                   { setHtmlVisible("UploadError", true, getMsg("upload.noFile"))      } 
    else if (App.tourney.getToId == 0) { setHtmlVisible("UploadError", true, getMsg("upload.notTournSel")) } 
    else { setHtmlVisible("UploadError", false);  doUpload(UploadType.Invite, UploadMode.UNKNOWN)          }


  // buttonDownloadPlayer returns list of players/participants
  def actionDownloadPlayer() = {
    val fName = getMsg("download.notify.filename")
    DlgBox.saveStringAsFile(
      gM("dlg.box.save.verify.hdr"), 
      gM("dlg.box.save.verify.msg", fName), 
      getMsg("download.notify.filename"),
      genPlayerList
    )
  }  


  // actionDownloadClickTT returns XMLresult file for clickTT
  def actionDownloadClickTT() = {
    startSpinner()
    downloadFile(DownloadType.ClickTT).map {
      case Left(err)  => stopSpinner(); DlgInfo.show(gM("dlg.info.download.error.hdr"), getError(err), "danger") 
      case Right(res) => stopSpinner(); DlgBox.saveStringAsFile(gM("dlg.box.save.verify.hdr"), gM("dlg.box.save.verify.msg", res._1), res._1,  res._2)
    }
  }


  /**
   ** Helper Routines
   */

  // doUpload upload a file 
  def doUpload(uplType: UploadType.Value, uplMode: UploadMode.Value)  = {
    import shared.utils.Constants._
    
    // perform upload of tourney information file (xml, markdown, or image)
    val formData = new dom.FormData(gE(uc(s"UploadForm_${uplType.id}")).asInstanceOf[HTMLFormElement])
      
    startSpinner()
    uplType match {
      case UploadType.ClickTT => uplMode match {
        case UploadMode.Update => updCttFile(App.tourney.getToId, App.tourney.startDate, formData).map {
          case Left(err)  => stopSpinner(); DlgInfo.show("Fehlermeldung", getError(err), "danger") 
          case Right(res) => stopSpinner(); DlgInfo.show("Fertigmeldung", s"TourneyId: ${res}", "success")
        } 
        case UploadMode.New    => newCttFile(App.tourney.getToId, App.tourney.startDate, formData).map {
          case Left(err)  => stopSpinner(); DlgInfo.show("Fehlermeldung", getError(err), "danger") 
          case Right(res) => stopSpinner(); DlgInfo.show("Fertigmeldung", s"TourneyId: ${res}", "success")
        } 
        case UploadMode.UNKNOWN => stopSpinner(); println("Unknown upload mode")
      }
      case _ => uploadFile(App.tourney.getToId, App.tourney.startDate, uplType, formData).map {
        case Left(err)  => stopSpinner(); DlgInfo.show("Fehlermeldung", getError(err), "danger") 
        case Right(res) => stopSpinner(); DlgInfo.show("Fertigmeldung", s"TourneyId: ${res}", "success")
      }
    }
  }

  /** genPlayerList
   *  returns Sequence of Players
   */
  def genPlayerList() : String = {
    val buf = new StringBuffer(getMsg("download.notify.filehdr") + "\n")

    val tourney = App.tourney
    val player  = App.tourney.players
    for {
      co    <- tourney.comps.values
      plco  <- tourney.pl2co.values.filter(_.coId==co.id).filter(_.status >= PantStatus.REGI)
    } yield {
      if (co.typ == CompTyp.SINGLE) {
        buf.append(s"${plco.sno},${player(plco.getPlayerId).lastname},${player(plco.getPlayerId).firstname},${player(plco.getPlayerId).getTTR},${player(plco.getPlayerId).clubName},${co.name}\n")
      }  
    }
    buf.toString
  }  


  // highlight selected element
  def highlight(elem: dom.raw.HTMLElement) = {
    import org.querki.jquery._ 
    $(elem).addClass("bg-secondary").siblings().removeClass("bg-secondary")
    $(elem).addClass("text-white").siblings().removeClass("text-white")
  }

  // setViewTournBase set the view of the base part of the tourney
  def setViewTournBase(trny: Tourney): Unit = {
    import shared.utils.Routines._
    collapse("TourneyCard", trny.id == 0)
  }

}