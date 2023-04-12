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
import shared.model.Pant
import shared.model.Competition._
import shared.model.CompPhase._
import shared.utils.Routines._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox, DlgPlayfield, DlgInfo }
import scala.collection.mutable.{ ArrayBuffer }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._

import clientviews.organize.competition.html._
import scalajs.usecase.dialog._
import scalajs.usecase.dialog.DlgCardCfgCompPhase.PantSelect
import scalajs.usecase.dialog.DlgCardCfgCompPhase.QualifyTyp

// ***
// COMPETITION Administration
// ***
@JSExportTopLevel("OrganizeCompetition")
object OrganizeCompetition extends UseCase("OrganizeCompetition")  
  with TourneySvc with ViewServices with AppHelperSvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    setMainContent(clientviews.organize.html.Competition())
    update()
  }

  override def update(param: String = "", upd: UpdateTrigger = UpdateTrigger("", 0L)) = {
    val toId = AppEnv.getToId
    val coId = App.getCurCoId
    
    val compList = (for { co <- App.tourney.comps.values.toSeq } yield {
      val (cnt, cntActiv) = App.tourney.getCompCnt(co)
      (co, cnt, cntActiv)
    }).toSeq.sortBy(_._1.startDate)  

    // set main card
    //for (comp <- compList) println(s"CoPhases: ${comp._1.name} Status: ${comp._1.status}") // DEBUG
  
    setHtml("CompCard", CompCard(compList, AppEnv.getLang) )
    println(s"Competition.update toId: ${toId} coId: ${coId}")
 
    // set play card and select competition
    if (coId > 0) {
      val comp = App.tourney.comps(coId)
      setDisabled("BtnStartCompetition", comp.status > CPC_INIT)
      selTableRow(uc(coId.toString))
      comp.typ match {
        case CT_SINGLE => setHtml("ParticipantCard", SingleCard(genSingleTblData(coId), comp.status <= CPC_INIT)) 
        case CT_DOUBLE => setHtml("ParticipantCard", DoubleCard(genDoubleTblData(coId), comp.status <= CPC_INIT)) 
        case _         => error("update", s"competition typ not yet supported") 
      }
      collapse("CompCard", false)
      //showSBMenu("OrganizeCompetition")
    } else {
      setDisabled("BtnStartCompetition", true)
      setHtml("ParticipantCard", s"""
        <div class="alert alert-info text-center mb-0" role="alert">
          <span class="tuse-font-1">${getMsg("nocompselect")}</span>
        </div>    
      """)
    } 
    setHeadline()
    //addClass("OrganizeCompetition", "show")(UCP("APP__Sidebar"))
    // val classAttr = getAttribute2(uc("OrganizeCompetition"), "class")
    // println(s"OrganizeCompetition: ${classAttr}")

  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    import cats.data.EitherT
    
    //debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    debug("actionEvent", s"key: ${key}")
    key match {
      //
      // Action with competition
      // 
      case "StartCompetition"   => {
        //import scalajs.usecase.dialog.DlgCardCfgCompPhase.PantSelect
        // set coId if a new is available
        val coPhIdPrev = 0 
        var coId = getData(elem, "coId", 0L)
        if (coId != 0) App.setCurCoId(coId) else coId = App.getCurCoId

        debug("actionEvent", s"node id: ${elem.id} coId: ${coId}")

        // initialize participants to be shown 
        // only participants with status signed or ready
        val pants = (App.tourney.pl2co.filterKeys(_._2 == coId).filter { case (x1,x2) => x2.status == Pant.SIGN || x2.status == Pant.REDY } map { x =>
          val sno = SNO(x._2.sno) 
          val (snoValue, name, club, ttr) = sno.getInfo(App.tourney.comps(coId).typ)(App.tourney)
          val enabled = (x._2.status == Pant.REDY)
          // show name, club name and ttr value
          PantSelect(sno, s"${name} [${club}]", s"TTR: ${ttr}", enabled) 
        }).to(ArrayBuffer).sortBy(x => (!x.checked, x.name))
        
        startCompPhaseDlg(coId, coPhIdPrev, QualifyTyp.None, pants)(App.tourney).map {
          case Left(err)   => error("startCompetitionDlg", s"error message: ${err}")
          case Right((coph, pantResult)) => {
            //set pant status in participant 2 competition mapping
            for ((key, pEntry) <- App.tourney.pl2co) { if (key._2 == coId && pEntry.status == Pant.REDY) pEntry.status == Pant.SIGN }
            pantResult.foreach { x => App.tourney.pl2co((x.sno, coId)).status = Pant.REDY }
            
            App.setCurCoPhId(coId, coph.coPhId)
            App.tourney.comps(coId).status = Competition.CS_RUN
            coph.drawOnRanking(pantResult, App.tourney.comps(coId).typ)
            //coph.status = CompPhase.CPS_AUS  
            App.execUseCase("OrganizeCompetitionDraw", "", "")
            println(s"CompPhase: \n ${coph.toString()}")
          }
        }
      }

      case "AddCompetition"     => {    
        App.resetCurCoId
        DlgCardComp.show(Competition.init, App.tourney, AppEnv.getLang, DlgOption.New).map {
          case Left(err)    => debug("AddCompetition", s"dialog DlgCardComp.show failed/canceled: ${err}")
          case Right(comp)  => addComp(comp).map { 
            case Left(err)  => DlgShowError.show(List(err))
            case Right(co)  => {
              debug("addComp", s"RESULT ${co.id}")
              App.tourney.comps(co.id) = co 
              App.saveLocalTourney(App.tourney)
              App.setCurCoId(co.id)
              update() 
            }
          }
        }
      }

      case "DeleteCompetition"      => {
        val coId = getData(elem, "coId", 0L)
        val coName = App.tourney.comps(coId).name

        event.stopPropagation()
        DlgBox.standard(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", coName), Seq("cancel", "ok")).map { _ match {
          case 2 => delComp(coId).map { 
            case Left(err)  => DlgInfo.show("FEHLER", getError(err), "danger")
            case Right(res) => {  
              App.tourney.delComp(coId)
              App.saveLocalTourney(App.tourney)
              App.resetCurCoId 
              update()               
            }
          }
          case _ => {}
        }}
      }

      case "ShowCompetition"      => {
        val coId = getData(elem, "coId", 0L)  
        App.setCurCoId(coId)

        event.stopPropagation()
        DlgCardComp.show(App.tourney.comps(coId), App.tourney, AppEnv.getLang, DlgOption.View )
        update()
      }

      case "EditCompetition"      => {
        val coId = getData(elem, "coId", 0L)  
        App.setCurCoId(coId)
        selTableRow(uc(coId.toString))  
        event.stopPropagation()
        DlgCardComp.show(App.tourney.comps(coId), App.tourney, AppEnv.getLang, DlgOption.Edit ).map {
          case Left(err)   =>
          case Right(comp) => setComp(comp).map { 
            case Left(err)  => setHtmlVisible("CompError", true, getError(err.asInstanceOf[Error]))
            case Right(co) => { 
                debug("setComp", s"RESULT ${co.id}")
                App.tourney.comps(co.id) = co 
                App.saveLocalTourney(App.tourney)
                App.setCurCoId(co.id)
                update() 
            }
          }
        } 
      }    

      case "CheckCompetition"      => { 
        val coId = getData(elem, "coId", 0L)
        App.tourney.comps(coId).setWebRegister(getCheckbox(elem)) 
      }       

      case "SelectCompetition"    => {   
        App.setCurCoId(getData(elem, "coId", 0L))
        update()
      }

      //
      // Action with participants
      //

      // activate/deactivate participant
      case "CheckParticipant"      => { 
        val coId = App.getCurCoId
        val sno  = getData(elem, "sno", "")
        val status = if (elem.asInstanceOf[dom.raw.HTMLInputElement].checked) 1 else 0 
        setPantStatus(coId, sno, status).map { 
          case Left(err)  => DlgShowError.show(List(err)) 
          case Right(res) => {
            // set statistic/numbers
            val p2c = App.tourney.pl2co.values.filter(_.coId == coId).toSeq
            val (total, activ) = (p2c.length, p2c.filter(_.status > Pant.SIGN).length )
            setHtml(s"Counter_${coId}", s"${total}/${activ}")           
          }
        }
      }

      case "DeleteParticipant"      => { 
        val coId = App.getCurCoId
        val sno  = getData(elem, "sno", "")

        if (coId > 0) {
          val comp = App.tourney.comps(coId)
          val coName = App.tourney.comps(coId).name
          val paName = SNO(sno).getName(comp.typ)(App.tourney)

          val confirm = comp.typ match {
            case CT_SINGLE => DlgBox.confirm(getMsg("confirm.single.delete.hdr"), getMsg("confirm.single.delete.msg", paName, coName))
            case CT_DOUBLE => DlgBox.confirm(getMsg("confirm.double.delete.hdr"), getMsg("confirm.double.delete.msg", paName, coName))
            case _         => error("update", s"competition typ not yet supported"); Future(false)
          }
          confirm.map { _ match {
            case true  =>  delPant2Comp(coId, sno).map { 
              case Left(err)  => DlgShowError.show(List(err)) 
              case Right(res) => {
                if ( App.tourney.pl2co.isDefinedAt((sno, coId)) ) App.tourney.pl2co -= ((sno,coId))
                App.saveLocalTourney(App.tourney)
                update()
              }
            }
            case false => info("DeleteParticipant", s"not confirmed for: ${sno}")
          }}

        }
      }

      //def show(coId: Long, trny: Tourney, lang: String): Future[Either[Error, (Player, Int)]] = {
      case "RegParticipant"    => {    
        App.tourney.comps(App.getCurCoId).typ match {
          case CT_SINGLE => regSingle(App.tourney, App.getCurCoId, AppEnv.getLang)
          case CT_DOUBLE => regDouble(App.tourney, App.getCurCoId, AppEnv.getLang)
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
        setPant2Comp(Pant2Comp.double(plId1, plId2, coId, status)) map {
          case Left(err)  => DlgShowError.show(List(err))
          case Right(p2c) => {
            info("RegDouble", s"success: ${p2c}")
            tourney.pl2co((p2c.sno, p2c.coId)) = p2c
            App.saveLocalTourney(App.tourney)
            update()
          }
        }
      }
    }
  }


  /** START COMPETITION PHASE DIALOG
   * 
   */
  def startCompPhaseDlg(coId: Long, baseCoPhId: Int, qualify: QualifyTyp.Value, pants: ArrayBuffer[PantSelect])
                         (implicit trny: Tourney): Future[Either[Error, (CompPhase, ArrayBuffer[ParticipantEntry]) ]] = {


    import cats.data.EitherT
    import scalajs.usecase.dialog.DlgCardCfgCompPhase
    import scalajs.usecase.dialog.DlgCardCfgCompPhase.Result
    import scalajs.usecase.dialog.DlgCardCfgCompPhase.QualifyTyp

    (for {
      dlgResult <-  EitherT(DlgCardCfgCompPhase.show(coId, qualify, pants))
      coph      <-  EitherT(addCompPhase(coId, baseCoPhId, dlgResult.qualify, dlgResult.config, dlgResult.name, dlgResult.winSets))
    } yield { (dlgResult, coph) }).value.map {
      case Left(err)     => Left(err)
      case Right(result) => {
        val pants = result._1.pants.filter { case x => x.checked } map { x => x.sno.getPantEntry(coId)(trny) }
        //result._2.draw(pants, trny.comps(coId).typ)
        Right((result._2, pants))
      }  
    }
  }


  
}