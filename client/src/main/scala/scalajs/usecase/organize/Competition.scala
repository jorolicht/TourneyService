package scalajs.usecase.organize

import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import shared.model._
import shared.model.PantStatus
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


/* DEBUG LINKS
** http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20compphase%20%2Dn%204 
**
** 
*/


// ***
// COMPETITION Administration
// ***
@JSExportTopLevel("OrganizeCompetition")
object OrganizeCompetition extends UseCase("OrganizeCompetition")  
  with TourneySvc with ViewServices
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    setMainContent(clientviews.organize.html.Competition())
    updComp()
  }

  def updComp() = {
    val coId   = App.tourney.getCurCoId
    val coPhId = App.tourney.getCurCoPhId

    setData(gUE("BtnAddCompPhase"), "coId", coId)

    val compList = (for { co <- App.tourney.comps.values.toSeq } yield {
      val (cnt, cntActiv) = App.tourney.getCompCnt(co)
      (co, cnt, cntActiv)
    }).toSeq.sortBy(_._1.startDate)  
    setHtml("CompCard", CompCard(compList, coId, AppEnv.getLang) )
    
    println(s"OrganizeCompetition.update -> coId: ${coId}  coPhId: ${coPhId}")

    // set play card and select competition
    if (coId > 0) {
      val comp = App.tourney.comps(coId)
      setVisible(gUE("CoPhCardMain"), true)
      setVisible(gUE("ParticipantCardMain"), true)
      setHtml("CoPhCardHdr", getMsg("coph.card", comp.name))
      setHtml("ParticipantCardHdr", getMsg("participant.card", comp.name))

      // prepare CompCard
      collapse("CompCard", false)
      
      // prepare CoPhCard
      val coPhNameId = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
      println(s"Competition Phase List: ${coPhNameId.mkString(":")}")
      setHtml("CoPhCard", CoPhCard(coId, coPhNameId))
      setCompPhaseTab(coId, coPhId) 


      val editable = (comp.status == CompStatus.READY) || (comp.status == CompStatus.CFG)

      // prepare ParticipantCard
      comp.typ match {
        case CompTyp.SINGLE => setHtml("ParticipantCard", SingleCard(genSingleTblData(App.tourney, coId), editable )) 
        case CompTyp.DOUBLE => setHtml("ParticipantCard", DoubleCard(genDoubleTblData(App.tourney, coId), editable )) 
        case _              => error("update", s"competition typ not yet supported") 
      }
      
      //showSBMenu("OrganizeCompetition")
    } else {
      setVisible(gUE("CoPhCardMain"), false)
      setVisible(gUE("ParticipantCardMain"), false)
    }
    setHeader()
    //addClass("OrganizeCompetition", "show")(UCP("APP__Sidebar"))
    // val classAttr = getAttribute2(uc("OrganizeCompetition"), "class")
    // println(s"OrganizeCompetition: ${classAttr}")
  }


  def updCoPh(coph: CompPhase) = {
    import org.scalajs.dom.raw.HTMLInputElement 
    import org.scalajs.dom.document
    if (coph.coPhId == 1) {
        val pants = (App.tourney.pl2co.filterKeys(_._2 == coph.coId).filter { case (x1,x2) => x2.status == PantStatus.REDY } map { x => SNO(x._2.sno) }).toList
        coph.noPlayers = pants.length
        println(s"updCoPh: pants ${pants.mkString(":")}")
    } else {
      // select pants from previous phase / round
      // read winner looser all selection
      // read configuration
      // decide on pants selection depending on KO or Group round
    }

  
    setHtml(gE(s"CoPhCardContent_${coph.coId}_${coph.coPhId}"), clientviews.organize.competition.CoPhCard.html.Content(coph) )
    
    setCophSystemOptions(coph.coId, coph.coPhId, coph.noPlayers)
    selectOption(s"Winset_${coph.coId}_${coph.coPhId}", coph.noWinSets.toString)
    selectOption(s"GameSystem_${coph.coId}_${coph.coPhId}", coph.coPhCfg.id.toString)
    setHtml(gUE(s"Status_${coph.coId}_${coph.coPhId}"), gMTyp(coph.status))
    setHtml(gUE(s"NoGames_${coph.coId}_${coph.coPhId}"), gUM("coph.noGames", coph.mFinished.toString, coph.mTotal.toString))
    setHtml(gUE(s"NoPlayer_${coph.coId}_${coph.coPhId}"), gUM("coph.noPlayers", coph.noPlayers.toString))
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
        if (coId != 0) App.tourney.setCurCoId(coId) else coId = App.tourney.getCurCoId

        debug("actionEvent", s"node id: ${elem.id} coId: ${coId}")

        // initialize participants to be shown 
        // only participants with status signed or ready
        val pants = (App.tourney.pl2co.filterKeys(_._2 == coId).filter { case (x1,x2) => x2.status == PantStatus.REGI || x2.status == PantStatus.REDY } map { x =>
          val sno = SNO(x._2.sno) 
          val (snoValue, name, club, ttr) = sno.getInfo(App.tourney.comps(coId).typ)(App.tourney)
          val enabled = (x._2.status == PantStatus.REDY)
          // show name, club name and ttr value
          PantSelect(sno, s"${name} [${club}]", s"TTR: ${ttr}", enabled) 
        }).to(ArrayBuffer).sortBy(x => (!x.checked, x.name))
        
        startCompPhaseDlg(coId, coPhIdPrev, QualifyTyp.None, pants)(App.tourney).map {
          case Left(err)   => error("startCompetitionDlg", s"error message: ${err}")
          case Right((coph, pantResult)) => {
            //set pant status in participant 2 competition mapping
            for ((key, pEntry) <- App.tourney.pl2co) { if (key._2 == coId && pEntry.status == PantStatus.REDY) pEntry.status == PantStatus.REGI }
            pantResult.foreach { x => App.tourney.pl2co((x.sno, coId)).status = PantStatus.REDY }

            App.tourney.comps(coId).setCurCoPhId(coph.coPhId)
            App.tourney.comps(coId).status = CompStatus.RUN
            coph.drawOnRanking(pantResult, App.tourney.comps(coId).typ)
            //coph.status = CompPhase.CPS_AUS  
            App.execUseCase("OrganizeCompetitionDraw", "", "")
            println(s"CompPhase: \n ${coph.toString()}")
          }
        }
      }

      case "AddCompetition"     => {    
        App.tourney.setCurCoId(0)
        DlgCardComp.show(Competition.init, App.tourney, AppEnv.getLang, DlgOption.New).map {
          case Left(err)    => debug("AddCompetition", s"dialog DlgCardComp.show failed/canceled: ${err}")
          case Right(comp)  => addComp(comp).map { 
            case Left(err)  => DlgShowError.show(List(err))
            case Right(co)  => {
              debug("addComp", s"RESULT ${co.id}")
              App.tourney.comps(co.id) = co 
              App.saveLocalTourney(App.tourney)
              App.tourney.setCurCoId(co.id)
              updComp() 
            }
          }
        }
      }

      case "DeleteCompetition"      => {
        val coId = getData(elem, "coId", 0L)
        val coName = App.tourney.comps(coId).name

        event.stopPropagation()
        dlgCancelOk(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", coName)) { 
          delComp(coId).map { 
            case Left(err)  => DlgInfo.show("FEHLER", getError(err), "danger")
            case Right(res) => {  
              App.tourney.delComp(coId)
              App.saveLocalTourney(App.tourney)
              App.tourney.setCurCoId(0)     // resetCurCoId
              updComp()               
            }
          }
        }
      }

      case "ShowCompetition"      => {
        val coId = getData(elem, "coId", 0L)  
        App.tourney.setCurCoId(coId)

        event.stopPropagation()
        DlgCardComp.show(App.tourney.comps(coId), App.tourney, AppEnv.getLang, DlgOption.View )
        updComp()
      }

      case "EditCompetition"      => {
        val coId = getData(elem, "coId", 0L)  
        App.tourney.setCurCoId(coId)
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
                App.tourney.setCurCoId(co.id)
                updComp() 
            }
          }
        } 
      }    

      case "CheckCompetition"      => { 
        val coId = getData(elem, "coId", 0L)
        App.tourney.comps(coId).setWebRegister(getCheckbox(elem)) 
      }       

      case "SelectCompetition"    => {   
        App.tourney.setCurCoId(getData(elem, "coId", 0L))
        updComp()
      }

      //
      // Action with participants
      //

      // activate/deactivate participant
      case "CheckParticipant"      => { 
        val coId = App.tourney.getCurCoId
        val sno  = getData(elem, "sno", "")
        val status = if (elem.asInstanceOf[dom.raw.HTMLInputElement].checked) PantStatus.REDY else PantStatus.REGI 
        setPantStatus(coId, sno, status).map { 
          case Left(err)  => DlgShowError.show(List(err)) 
          case Right(res) => {
            // set statistic/numbers
            val p2c = App.tourney.pl2co.values.filter(_.coId == coId).toSeq
            val (total, activ) = (p2c.length, p2c.filter(_.status > PantStatus.REGI).length )
            setHtml(s"Counter_${coId}", s"${total}/${activ}")

            //update competition phase
            if (App.tourney.cophs.contains((coId, App.tourney.getCurCoPhId))) updCoPh(App.tourney.cophs((coId, App.tourney.getCurCoPhId)))
          }
        }
      }

      case "DeleteParticipant"      => { 
        val coId = App.tourney.getCurCoId
        val sno  = getData(elem, "sno", "")

        if (coId > 0) {
          val comp = App.tourney.comps(coId)
          val coName = App.tourney.comps(coId).name
          val paName = SNO(sno).getName(comp.typ)(App.tourney)

          val confirm = comp.typ match {
            case CompTyp.SINGLE => DlgBox.confirm(getMsg("confirm.single.delete.hdr"), getMsg("confirm.single.delete.msg", paName, coName))
            case CompTyp.DOUBLE => DlgBox.confirm(getMsg("confirm.double.delete.hdr"), getMsg("confirm.double.delete.msg", paName, coName))
            case _              => error("update", s"competition typ not yet supported"); Future(false)
          }
          confirm.map { _ match {
            case true  =>  delPant2Comp(coId, sno).map { 
              case Left(err)  => DlgShowError.show(List(err)) 
              case Right(res) => {
                if ( App.tourney.pl2co.isDefinedAt((sno, coId)) ) App.tourney.pl2co -= ((sno,coId))
                App.saveLocalTourney(App.tourney)
                updComp()
              }
            }
            case false => info("DeleteParticipant", s"not confirmed for: ${sno}")
          }}

        }
      }

      //def show(coId: Long, trny: Tourney, lang: String): Future[Either[Error, (Player, Int)]] = {
      case "RegParticipant"    => {    
        App.tourney.comps(App.tourney.getCurCoId).typ match {
          case CompTyp.SINGLE => regSingle(App.tourney, App.tourney.getCurCoId, AppEnv.getLang)
          case CompTyp.DOUBLE => regDouble(App.tourney, App.tourney.getCurCoId, AppEnv.getLang)
          case _              => error("update", s"competition typ not yet supported") 
        }  
      } 

      case "UnselectPlayer"    => {    
        debug("UnselectPlayer", s"key: ${key}")
      }
      
      case "ParticipantCard"    => {
        togCollapse("ParticipantCard")
      }


      //
      // Action with competition phase
      //      

      case "SelectCoPh"   => {
        val (coId, coPhId) = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0))
        App.tourney.comps(coId).setCurCoPhId(coPhId)
        setCompPhaseTab(coId, coPhId)
      }  


      case "AddCompPhase" => DlgInputTxt.show(getMsg("dlg.coph.new.hdr"), 
                                              getMsg("dlg.coph.new.lbl"), 
                                              getMsg("dlg.coph.new.plh"), 
                                              gM("std.btn.close"), 
                                              gM("std.btn.add"), chkCoPhName) map { 
        case Left(err)    => error("AddCompPhase.action", s"DlgInputTxt ${err}")
        case Right(value) => {
          val coId = getData(elem, "coId", 0L)
          addCompPhase(coId, value).map {
            case Left(err)    => error("AddCompPhase.action", s"addCompPhase ${err}")
            case Right(coph)  => {
              App.tourney.comps(coId).setCurCoPhId(coph.coPhId)
              updComp()             
              collapse("CoPhCard", false)
            }
          }      
        }
      }

      case "CollapseCoPh" => {
        togCollapse("CoPhCard")
        setCompPhaseTab(App.tourney.getCurCoId, App.tourney.getCurCoPhId)        
      }

      case "GameSystem" => {
        val coId   = getData(elem, "coId", 0L)
        val coPhId = getData(elem, "coPhId", 0)
        App.tourney.getCoPh(coId, coPhId) match { case Right(coph) => coph.coPhCfg = CompPhaseCfg(getInput(gUE(s"GameSystem_${coId}_${coPhId}"), 0)); case _ => {} }
      }

      case "Winset" => {
        val coId   = getData(elem, "coId", 0L)
        val coPhId = getData(elem, "coPhId", 0)
        App.tourney.getCoPh(coId, coPhId) match { case Right(coph) => coph.noWinSets = getInput(gUE(s"Winset_${coId}_${coPhId}"), 0); case _ => {} }
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
        updComp()      
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
        setPant2Comp(Pant2Comp.double(plId1, plId2, coId, PantStatus(status))) map {
          case Left(err)  => DlgShowError.show(List(err))
          case Right(p2c) => {
            info("RegDouble", s"success: ${p2c}")
            tourney.pl2co((p2c.sno, p2c.coId)) = p2c
            App.saveLocalTourney(App.tourney)
            updComp()
          }
        }
      }
    }
  }


  /** START COMPETITION PHASE DIALOG
   * 
   */
  def startCompPhaseDlg(coId: Long, baseCoPhId: Int, qualify: QualifyTyp.Value, pants: ArrayBuffer[PantSelect])
                         (implicit trny: Tourney): Future[Either[Error, (CompPhase, ArrayBuffer[PantEntry]) ]] = {
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

  def setCompPhaseTab(coId: Long, selectedId: Int) = {
    import org.scalajs.dom.raw.HTMLElement
    val aNodes = gE("CoPhCardLinks").getElementsByTagName("a")  
    // set register/tab active
    for( i <- 0 to aNodes.length-1) {
      val elem   = aNodes.item(i).asInstanceOf[HTMLElement]
      val coPhId = getData(elem, "coPhId", 0)
      val activ = (coPhId == selectedId)

      // set link and content
      setClass(elem, activ, "active")
      setVisible(gE(s"CoPhCardContent_${coId}_${coPhId}"), activ)
    } 
    App.tourney.comps(coId).setCurCoPhId(selectedId)
    App.tourney.getCoPh(coId,selectedId) match { case Right(coph) => updCoPh(coph); case _ => {} }
  }   


  def chkCoPhName(input: String): Either[String, Boolean] = {
    val coId = App.tourney.getCurCoId
    val coPhNames = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => x.name).toList
    if      (input == "")               Left(gM("err0233.noInput")) 
    else if (coPhNames.contains(input)) Left(gM("err0234.coph.already.exists", input))
    else if (input.length < 2)          Left(gM("err0235.coph.name.short"))
    else                                Right(true)
  }


  // sysOptions generate all possible options for given size
  def sysOptions(size: Int): List[CompPhaseCfg.Value] = {
    def sysOptions21to128(size: Int): List[CompPhaseCfg.Value] = {
      val result = ArrayBuffer[CompPhaseCfg.Value]()  
      if (size % 3 == 0) result += CompPhaseCfg.GRPS3 else result += CompPhaseCfg.GRPS34
      if (size % 4 == 0) result += CompPhaseCfg.GRPS4 else result += CompPhaseCfg.GRPS45
      if (size % 5 == 0) result += CompPhaseCfg.GRPS5 else result += CompPhaseCfg.GRPS56
      result += CompPhaseCfg.KO
      result += CompPhaseCfg.SW
      result.to(List)
    }

    size match {
      case 3 | 4 | 5 => List(CompPhaseCfg.JGJ,    CompPhaseCfg.KO,     CompPhaseCfg.SW)
      case 6         => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.KO,     CompPhaseCfg.SW,     CompPhaseCfg.JGJ)
      case 7         => List(CompPhaseCfg.GRPS34, CompPhaseCfg.KO,     CompPhaseCfg.SW,     CompPhaseCfg.JGJ)
      case 8         => List(CompPhaseCfg.GRPS4,  CompPhaseCfg.KO,     CompPhaseCfg.SW,     CompPhaseCfg.JGJ     )
      case 9         => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.JGJ)
      case 10        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS5,  CompPhaseCfg.KO,     CompPhaseCfg.SW)
      case 11        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS56, CompPhaseCfg.KO,     CompPhaseCfg.SW)
      case 12        => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS4,  CompPhaseCfg.GRPS6,  CompPhaseCfg.KO, CompPhaseCfg.SW )
      case 13        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW )
      case 14        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW)
      case 15        => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS5,  CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 16        => List(CompPhaseCfg.GRPS4,  CompPhaseCfg.GRPS56, CompPhaseCfg.KO,     CompPhaseCfg.SW)
      case 17        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.GRPS56, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 18        => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS45, CompPhaseCfg.GRPS6,  CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 19        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW)
      case 20        => List(CompPhaseCfg.GRPS4,  CompPhaseCfg.GRPS5,  CompPhaseCfg.KO,     CompPhaseCfg.SW)

      case i if (i > 21 && i <= 128) => sysOptions21to128(i)
      case _                         => List()        
    }
  }


  // setCophSystemOptions
  def setCophSystemOptions(coId: Long, coPhId: Int, size: Int): Unit = {
    val cfgOptions = sysOptions(size)
    val selOptions = new StringBuilder(s"<option value='${CompPhaseCfg.CFG.id}' selected>${gM(CompPhaseCfg.CFG.msgCode)}</option>")
    for (cfg <- cfgOptions) {
      selOptions ++= s"<option value='${cfg.id}' selected>${gM(cfg.msgCode)}</option>" 
    }
    setHtml(s"GameSystem_${coId}_${coPhId}", selOptions.toString)
  }



  
}