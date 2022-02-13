package scalajs.usecase.organize

// Start TestCases
// http://localhost:9000/start?ucName=TestMain&ucParam=OrgComp

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
import shared.utils.Constants._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox, DlgPlayfield, DlgInfo }
import scala.collection.mutable.{ ArrayBuffer }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._

import clientviews.organize.competition.html._
import scalajs.usecase.dialog._

// ***
// COMPETITION Administration
// ***
@JSExportTopLevel("OrganizeCompetition")
object OrganizeCompetition extends UseCase("OrganizeCompetition")  
  with TourneySvc with ViewServices with UIComponentServices
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    setMainContent(clientviews.organize.html.Competition().toString)
    update()
  }

  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    val toId = AppEnv.getToId
    val coId = AppEnv.getCoId
    
    // genCompetitionTblData - returns Sequence of tupels with competition attributes
    def genCompetitionTblData(trny: Tourney, lang: String): Seq[(Long, String, String, String, Int, String, Int, Int, Int, String, String)] = {
      (for { co <- trny.comps.values.toSeq } yield co.typ match {
        case CT_SINGLE | CT_DOUBLE => {
          val (cnt, cntActiv) = trny.getCompCnt(co)
          (co.id, co.name, co.getAgeGroup, co.getRatingRemark, co.typ, co.formatTime(lang), cnt, cntActiv, co.status, co.genRange(), co.options)
        }
        case _ => (co.id, co.name, co.getAgeGroup, co.getRatingRemark, co.typ, co.startDate, 0, 0, co.status, co.genRange(), co.options)
      }).toSeq.sortBy(_._6)
    }

    // set main card
    setHtml("CompCard", CompCard(genCompetitionTblData(Trny, AppEnv.getLang), coId))
 
    // set play card and select competition
    if (coId > 0) {
      val comp = App.tourney.comps(coId)
      setDisabled("BtnStartCompetition", comp.status > CP_INIT)
      selTableRow(uc(coId.toString))
      comp.typ match {
        case CT_SINGLE => setHtml("ParticipantCard", SingleCard(genSingleTblData(coId), comp.status <= CP_INIT)) 
        case CT_DOUBLE => setHtml("ParticipantCard", DoubleCard(genDoubleTblData(coId), comp.status <= CP_INIT)) 
        case _         => error("update", s"competition typ not yet supported") 
      }   
    } else {
      setDisabled("BtnStartCompetition", true)
      setHtml("ParticipantCard", s"""
        <div class="alert alert-info text-center mb-0" role="alert">
          <span class="tuse-font-1">${getMsg("nocompselect")}</span>
        </div>    
      """)
    } 
    setHeadline()
  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    //debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    debug("actionEvent", s"key: ${key}")
    key match {
      //
      // Action with competition
      // 
      case "StartCompetition"   => {
        // set coId if a new is available
        var coId = getData(elem, "coId", 0L)
        if (coId != 0) AppEnv.setCoId(coId) else coId = AppEnv.getCoId

        debug("actionEvent", s"node id: ${elem.id} coId: ${coId}")

        val pants = (Trny.pl2co.filterKeys(_._2 == coId).map { x =>
          val sno = SNO(x._2.sno) 
          val (snoValue, name, club, ttr) = sno.getInfo(Trny.comps(coId).typ)(Trny)
          val status = (x._2.status == Constants.PLS_REDY)
          PantSelect(sno, s"${name} [${club}]", s"TTR: ${ttr}", status, true) 
        }).to(ArrayBuffer).sortBy(x => (!x.checked, x.name))

        DlgCardCfgSection.show(coId, 0, pants)(Trny) map {
          case Left(err)           => debug("DlgCardCfgSection", s"show => cancel: ${err}")
          case Right((cfg, pants)) => {
            // update ALL entries, only entries with PLS_REDY or PLS_SIGN should be affected
            val pantStatus = pants.map { p => (p.sno.value, if (p.checked) Constants.PLS_REDY else Constants.PLS_SIGN) }
            //for (pant <- pants) Trny.pl2co((pant.sno.value, coId)).status = if (pEntry.checked) PLS_REDY else PLS_SIGN
            setPantBulkStatus(coId, pantStatus.toList).map {
              case Left(err)   => error("setPantBulkStatus", s"${err}")
              case Right(res)  => {
                for (pant <- pantStatus) Trny.pl2co((pant._1, coId)).status = pant._2
                
              }  
            }
          }
        }        

        //debug("StartCompetition", s"todo start competition")
      }  

  val PLS_SIGN =  0  // signup confirmed
  val PLS_REDY =  1  // participation confirmed      

    // val sno:       String,         // mapping of player identifier to start numbers = sno1 <MD> sno2 <MD> ...
    //                                // player/double or team gets a participant of the competition
    //                                // participants are identified by start numbers (sno) 
    // val coId:      Long, 
    // var ident:     String,
    // var placement: String,         // format <position> or <fromPosition>.<toPosition>
    // var status:    Int,            // -1 = pending signup acknowledge
    //                                //  0 = register/signup
    //                                //  1 = participate
    //                                //  2 = currently playing
    //                                //  3 = finished
    //  var options:   String = "_"            



      case "AddCompetition"     => {    
        AppEnv.setCoId(0L)
        DlgCardComp.show(Competition.init, App.tourney, AppEnv.getLang, DlgOption.New).map {
          case Left(err)    => debug("AddCompetition", s"dialog DlgCardComp.show failed/canceled: ${err}")
          case Right(comp)  => setComp(comp).map { 
            case Left(err)  => DlgShowError.show(List(err))
            case Right(co)  => {
              debug("setComp", s"RESULT ${co.id}")
              App.tourney.comps(co.id) = co 
              App.saveLocalTourneyCfg(App.tourney)
              AppEnv.setCoId(co.id)
              update() 
            }
          }
        }
      }

      case "DeleteCompetition"      => {
        val coId = getData(elem, "coId", 0L)
        val coName = App.tourney.comps(coId).name

        event.stopPropagation()
        DlgBox.showStd(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", coName), Seq("cancel", "ok")).map { _ match {
          case 2 => delComp(coId).map { 
            case Left(err)  => DlgInfo.show("FEHLER", getError(err), "danger")
            case Right(res) => {  
              App.tourney.delComp(coId)
              App.saveLocalTourneyCfg(App.tourney)
              AppEnv.setCoId(0) 
              update()               
            }
          }
          case _ => {}
        }}
      }

      case "ShowCompetition"      => {
        val coId = getData(elem, "coId", 0L)  
        AppEnv.setCoId(coId)
        event.stopPropagation()
        DlgCardComp.show(App.tourney.comps(coId), App.tourney, AppEnv.getLang, DlgOption.View )
        update()
      }

      case "EditCompetition"      => {
        val coId = getData(elem, "coId", 0L)  
        AppEnv.setCoId(coId)
        selTableRow(uc(coId.toString))  
        event.stopPropagation()
        DlgCardComp.show(App.tourney.comps(coId), App.tourney, AppEnv.getLang, DlgOption.Edit ).map {
          case Left(err)   =>
          case Right(comp) => setComp(comp).map { 
            case Left(err)  => setHtmlVisible("CompError", true, getError(err.asInstanceOf[Error]))
            case Right(co) => { 
                debug("setComp", s"RESULT ${co.id}")
                App.tourney.comps(co.id) = co 
                App.saveLocalTourneyCfg(App.tourney)
                AppEnv.setCoId(co.id); 
                update() 
            }
          }
        } 
      }    

      case "CheckCompetition"      => { 
        val coId = getData(elem, "coId", 0L)
        val status = if (elem.asInstanceOf[dom.raw.HTMLInputElement].checked) CS_REGIS else CS_RESET

        event.stopPropagation()
        setCompStatus(coId, status).map {
          case Left(err)  => DlgShowError.show(List(err))
          case Right(res) => {
            if (!res) error("setCompStatus", s"failed for coId: ${coId}")
            App.tourney.comps(coId).status = status 
            App.saveLocalTourneyCfg(App.tourney)
            AppEnv.setCoId(coId)
            update()
          }
        }
      }       

      case "SelectCompetition"    => {   
        AppEnv.setCoId(getData(elem, "coId", 0L))
        update()
      }

      //
      // Action with participants
      //

      // activate/deactivate participant
      case "CheckParticipant"      => { 
        val coId = AppEnv.getCoId
        val sno  = getData(elem, "sno", "")
        val status = if (elem.asInstanceOf[dom.raw.HTMLInputElement].checked) 1 else 0 
        setParticipantStatus(coId, sno, status).map { 
          case Left(err)  => DlgShowError.show(List(err)) 
          case Right(res) => App.tourney.setParticipantStatus(coId, sno, status) match {
            case Left(err)  => error("setParticipantStatus", "action CheckParticipant, couldn't set participant status")
            case Right(res) => {
              // set statistic/numbers
              val p2c = App.tourney.pl2co.values.filter(_.coId == coId).toSeq
              val (total, activ) = (p2c.length, p2c.filter(_.status > PLS_SIGN).length )
              App.saveLocalTourneyCfg(App.tourney)
              setHtml(s"Count_${coId}", s"${total}/${activ}") 
            }
          }
        }  
      }

      case "DeleteParticipant"      => { 
        val coId = AppEnv.getCoId
        val sno  = getData(elem, "sno", "")

        if (coId > 0) {
          val comp = App.tourney.comps(coId)
          val coName = App.tourney.comps(coId).name
          val paName = SNO(sno).getName(comp.typ)(App.tourney)

          val confirm = comp.typ match {
            case CT_SINGLE => confirmDlg(getMsg("confirm.single.delete.hdr"), getMsg("confirm.single.delete.msg", paName, coName))
            case CT_DOUBLE => confirmDlg(getMsg("confirm.double.delete.hdr"), getMsg("confirm.double.delete.msg", paName, coName))
            case _         => error("update", s"competition typ not yet supported"); Future(false)
          }
          confirm.map { _ match {
            case true  =>  delParticipant2Comp(coId, sno).map { 
              case Left(err)  => DlgShowError.show(List(err)) 
              case Right(res) => {
                if ( App.tourney.pl2co.isDefinedAt((sno, coId)) ) App.tourney.pl2co -= ((sno,coId))
                App.saveLocalTourneyCfg(App.tourney)
                update()
              }
            }
            case false => info("DeleteParticipant", s"not confirmed for: ${sno}")
          }}

        }
      }

      //def show(coId: Long, trny: Tourney, lang: String): Future[Either[Error, (Player, Int)]] = {
      case "RegParticipant"    => {    
        val coId = AppEnv.getCoId
        App.tourney.comps(coId).typ match {
          case CT_SINGLE => regSingle(App.tourney, coId, AppEnv.getLang)
          case CT_DOUBLE => regDouble(App.tourney, coId, AppEnv.getLang)
          case _         => error("update", s"competition typ not yet supported") 
        }  
      } 

      case "UnselectPlayer"    => {    
        debug("UnselectPlayer", s"key: ${key}")
      }
      
      case "ParticipantCard"    => {
        togCollapse("ParticipantCard")
      }

      case _          => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }

/** regSingle
 * 
 */
def regSingle(tourney: Tourney, coId: Long, lang: String): Unit = {
    import cats.data.EitherT
    import cats.implicits._
    
    (for {
      pl1St2 <- EitherT(DlgCardRegSingle.show(coId, tourney, lang))
      plId   <- EitherT(regSingle(coId, pl1St2._1, pl1St2._2))
      trny   <- EitherT(getTourney(tourney.id))
    } yield { (pl1St2, plId, trny) }).value.map {
      case Left(err)  => if (!err.equal2Code("dlg.canceled")) DlgShowError.show(List(err)) else info("DlgCardRegSingle", s"dialog canceled: ${err}")
      case Right(res) => { 
        App.setLocalTourney(res._3)
        update()      
      }
    }
  }


  /** regDouble
   * 
   */ 
  def regDouble(tourney: Tourney, coId: Long, lang: String):Unit = {
    DlgCardRegDouble.show(coId, tourney, lang).map {
      case Left(err)  => if (!err.equal2Code("dlg.canceled")) DlgShowError.show(List(err)) else info("DlgCardRegSingle", s"dialog canceled: ${err}")
      case Right((plId1, plId2, status)) => {
        setParticipant2Comp(Participant2Comp.double(plId1, plId2, coId, status)) map {
          case Left(err)  => DlgShowError.show(List(err))
          case Right(p2c) => {
            info("RegDouble", s"success: ${p2c}")
            tourney.pl2co((p2c.sno, p2c.coId)) = p2c
            App.saveLocalTourneyCfg(App.tourney)
            update()
          }
        }
      }
    }
  }
  
}