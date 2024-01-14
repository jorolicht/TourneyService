package scalajs.usecase.organize

/* DEBUG LINKS
** http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20coph%20%2Dn%204
*/
import scala.collection.mutable.{ ArrayBuffer }
import scala.concurrent._
import scala.util.{Success, Failure }

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.dom
import org.scalajs.dom.raw.{ HTMLElement, HTMLInputElement } 

import shared.model._
import shared.utils._

import scalajs.service._
import scalajs.{ App, AppEnv }
import scalajs.usecase.component._
import scalajs.usecase.dialog._
import clientviews.organize.competition.html._


// ***
// COMPETITION Administration
// ***
@JSExportTopLevel("OrganizeCompetition")
object OrganizeCompetition extends UseCase("OrganizeCompetition")  
  with TourneySvc with ViewServices
{
  
  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    setMainContent(clientviews.organize.html.Competition())
    updViewComp(App.tourney.getCurCoId)
  }

  def updViewComp(coId: Long) = {
    val coPhId = App.tourney.getCurCoPhId

    removeClass(gE("APP__Sidebar__OrganizeCompetition"), "collapse")
    setData(gUE("BtnAddCoPh"), "coId", coId)

    val compList = (for { co <- App.tourney.comps.values.toSeq } yield {
      val (cnt, cntActiv) = App.tourney.getCompCnt(co)
      (co, cnt, cntActiv)
    }).toSeq.sortBy(_._1.startDate)  
    setHtml(gUE("CompCard"), CompCard(compList, coId, AppEnv.getLang))

    // set play card and select competition
    if (coId > 0) {
      val comp = App.tourney.comps(coId)
      setVisible(gUE("CoPhCardMain"), true)
      setVisible(gUE("ParticipantCardMain"), true)

      setHtml(gUE("CoPhCardHdr"), getMsg("coph.card", comp.name))
      setHtml(gUE("ParticipantCardHdr"), getMsg("participant.card", comp.name))

      setDisabled("BtnRegParticipant", !comp.status.equalsTo(CompStatus.CFG,CompStatus.READY) )

      // prepare CompCard
      collapse("CompCard", false)
      
      // prepare CoPhCard
      val coPhNameId = App.tourney.cophs.filter(x => x._1._1 == coId).values.map(x => (x.name, x.coPhId)).toList
      setHtml(gUE("CoPhCard"), CoPhCard(coId, coPhNameId))

      App.tourney.getCoPh(coId, coPhId) map { coph => {
        if (coph.candidates.length == 0) setPantCandidates(coph)
        updViewCoPhContent(coph)
      }}  
      
      collapse("CoPhCard", false)

      val editable = comp.status.equalsTo(CompStatus.CFG, CompStatus.READY)

      // prepare ParticipantCard
      setVisible(gUE("ParticipantCardMain"), coPhId == 1 || coPhId == 0 )
      comp.typ match {
        case CompTyp.SINGLE => setHtml(gUE("ParticipantCard"), SingleCard(coId, genSingleTblData(App.tourney, coId), editable )) 
        case CompTyp.DOUBLE => setHtml(gUE("ParticipantCard"), DoubleCard(genDoubleTblData(App.tourney, coId), editable )) 
        case _              => error("update", s"competition typ not yet supported") 
      }
      
      //showSBMenu("OrganizeCompetition")
    } else {
      setVisible(gUE("CoPhCardMain"), false)
      setVisible(gUE("ParticipantCardMain"), false)
    }
    setHeader()
  }



  @JSExport 
  override def actionEvent(key: String, elem: HTMLElement, event: dom.Event) = {  
    key match {
      //
      // Action with competition
      //
      case "AddCompetition"     => {    
        App.tourney.setCurCoId(0)
        DlgCardComp.show(Competition.init, App.tourney, AppEnv.getLang, DlgOption.New).map {
          case Left(err)    => debug("AddCompetition", s"dialog DlgCardComp.show failed/canceled: ${err}")
          case Right(comp)  => addComp(comp).map { 
            case Left(err)  => DlgShowError.show(List(err))
            case Right(co)  => App.tourney.setCurCoId(co.id); updViewComp(co.id)
          }
        }
      }

      case "DeleteCompetition"      => {
        val coId = getCoId(elem)
        val coName = App.tourney.comps(coId).name

        event.stopPropagation()
        dlgCancelOk(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", coName)) { 
          delComp(coId).map { 
            case Left(err)  => DlgInfo.error(gM("msg.error.hdr"), getError(err))
            case Right(res) => App.tourney.setCurCoId(0); updViewComp(0)
          }
        }
      }

      case "ShowCompetition"      => {
        val coId = getCoId(elem) 
        event.stopPropagation()
        App.tourney.setCurCoId(coId)
        DlgCardComp.show(App.tourney.comps(coId), App.tourney, AppEnv.getLang, DlgOption.View )
        updViewComp(coId)
      }

      case "EditCompetition"      => {
        val coId = getCoId(elem)  
        App.tourney.setCurCoId(coId)
        selTableRow(uc(coId.toString))  
        event.stopPropagation()
        DlgCardComp.show(App.tourney.comps(coId), App.tourney, AppEnv.getLang, DlgOption.Edit ).map {
          case Left(err)   =>
          case Right(comp) => setComp(comp).map { 
            case Left(err)    => setHtmlVisible("CompError", true, getError(err.asInstanceOf[Error]))
            case Right(co)    => App.tourney.setCurCoId(co.id); updViewComp(coId)
          }
        } 
      }    

      case "CheckCompetition"      => App.tourney.comps(getCoId(elem)).setWebRegister(getCheckbox(elem)) 

      case "SelectCompetition"    => {   
        val coId = getCoId(elem)
        App.tourney.setCurCoId(coId)
        updViewComp(coId)
      }

      //
      // Action with participants
      //
      // activate/deactivate participant
      case "CheckPant"      => { 
        val status = if (elem.asInstanceOf[dom.raw.HTMLInputElement].checked) PantStatus.REDY else PantStatus.REGI 
        actionCheckPant(App.tourney.getCurCoId, SNO(getSNO(elem)), status)
      } 

      case "UploadPant"      => actionUploadPant(getCoId(elem))

      case "DeletePant"      => actionDeletePant(App.tourney.getCurCoId, getSNO(elem))

      case "SortPant"        => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => 
        actionSortPant(coph, getData(elem,"sort",""), getData(elem,"sortDir",""))
      }  

      case "SortSingle"      => actionSortSingle(getCoId(elem), getData(elem,"sort",""), getData(elem,"sortDir",""))
    

      //def show(coId: Long, trny: Tourney, lang: String): Future[Either[Error, (Player, Int)]] = {
      case "RegParticipant"    => {    
        val coId = App.tourney.getCurCoId
        App.tourney.comps(coId).typ match {
          case CompTyp.SINGLE => actionRegSingle(coId, AppEnv.getLang)
          case CompTyp.DOUBLE => actionRegDouble(coId, AppEnv.getLang)
          case _              => error("update", s"competition typ not yet supported") 
        }  
      }
      
      case "ParticipantCard"    => togCollapse("ParticipantCard")


      //
      // Action with competition phase
      // 
      case "StartDrawCoPh"   =>  actionStartDrawCoPh(getCoId(elem), getCoPhId(elem)) 

      case "StartInputCoPh"   => {
        //val (coId, coPhId) = (getCoId(elem), getCoPhId(elem))
        actionStartInputCoPh(getCoId(elem), getCoPhId(elem)) 
      }        

      case "SelectCoPh"   => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => {
        App.tourney.comps(coph.coId).setCurCoPhId(coph.coPhId)
        updViewCoPhContent(coph)
      }}  

      case "AddCoPh"        => actionAddCoPh(getCoId(elem))
      case "ResetCfgCoPh"   => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph =>
        dlgCancelOk(getMsg("confirm.resetCfg.hdr"), getMsg("confirm.resetCfg.msg", coph.name)) { actionResetCfgCoPh(coph) } 
      }

      case "ResetDrawCoPh"  => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph =>
        dlgCancelOk(getMsg("confirm.resetDraw.hdr"), getMsg("confirm.resetDraw.msg", coph.name)) { actionResetDrawCoPh(coph) }  
      }

      case "ResetInputCoPh" => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph =>
        dlgCancelOk(getMsg("confirm.resetInput.hdr"), getMsg("confirm.resetInput.msg", coph.name)) { actionResetInputCoPh(coph) }
      }

      case "DeleteCoPh" => App.tourney.getCoPh(App.tourney.getCurCoId, App.tourney.getCurCoPhId) map { coph => {
        dlgCancelOk(getMsg("confirm.delete.hdr"), getMsg("confirm.delete.msg", coph.name)) { actionDeleteCoPh(coph) } 
      }}

      case "CollapseCoPh" => App.tourney.getCoPh(App.tourney.getCurCoId, App.tourney.getCurCoPhId) map { coph => {
        updViewCoPhContent(coph) 
        togCollapse("CoPhCard")     
      }}

      case "GameSystem" => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => {
        coph.coPhCfg = CompPhaseCfg(getInput(gUE(s"GameSystem_${coph.coId}_${coph.coPhId}"), 0))
        updViewCoPhContent(coph)
      }}

      case "Winset" => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => {
        coph.noWinSets = getInput(gUE(s"Winset_${coph.coId}_${coph.coPhId}"), 0)
        updViewCoPhContent(coph)
      }}
  
      case "QualifyTyp" => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => {
        coph.quali   = QualifyTyp(getRadioBtn(s"QualifyTypName_${coph.coId}_${coph.coPhId}", 0))
        coph.coPhCfg = CompPhaseCfg.CFG
        setPantCandidates(coph)
        updViewCoPhContent(coph)
      }}

      case "BaseCoPh" => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => {
        println(s"BaseCoPh coId: ${coph.coId} cophId: ${coph.coPhId}")

        coph.baseCoPhId = getInput(gUE(s"BaseCoPh_${coph.coId}_${coph.coPhId}"), "None").toIntOption
        coph.coPhCfg  = CompPhaseCfg.CFG
        setPantCandidates(coph)

        updViewCoPhContent(coph)
      }}

      case "CheckPantCand" => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => {
        val index = getData(elem, "index", 0)
        if (coph.candidates.isDefinedAt(index)) coph.candidates(index) = coph.candidates(index).copy(_2 = elem.asInstanceOf[HTMLInputElement].checked)
        coph.noPlayers = coph.candidates.filter(_._2).size
        coph.coPhCfg   = CompPhaseCfg.CFG
        updViewCoPhContent(coph)
      }}  
  
      case "DemoBtn" => actionDemoBtn(getCoId(elem), getCoPhId(elem), elem.asInstanceOf[HTMLInputElement].checked)

      case "Click2Status" => App.tourney.getCoPh(getCoId(elem), getCoPhId(elem)) map { coph => coph.status match {
        case CompPhaseStatus.AUS => App.execUseCase("OrganizeCompetitionDraw", "", "") 
        case CompPhaseStatus.EIN => App.execUseCase("OrganizeCompetitionInput", "", "") 
        case CompPhaseStatus.FIN => App.execUseCase("OrganizeCompetitionView", "", "")    
      }}

      case _          => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }

  /** actionRegSingle
   */
  def actionRegSingle(coId: Long, lang: String): Unit = {
    import cats.data.EitherT
    import cats.implicits._
    
    (for {
      plSt    <- EitherT(DlgCardRegSingle.show(coId, App.tourney, lang))
      player  <- EitherT(regSingle(coId, plSt._1, plSt._2))
    } yield { (plSt, player) }).value.map {
      case Left(err)  => if (!err.equal2Code("dlg.canceled")) DlgShowError.show(List(err)) else info("DlgCardRegSingle", s"dialog canceled: ${err}")
      case Right(res) => updViewComp(coId)
    }
  }


  /** actionRegDouble
   */ 
  def actionRegDouble(coId: Long, lang: String):Unit = {
    import cats.data.EitherT
    import cats.implicits._

    (for {
      plSt   <- EitherT(DlgCardRegDouble.show(coId, App.tourney, lang))
      double <- EitherT(regDouble(coId, (plSt._1._1, plSt._1._2), plSt._2))
    } yield { (plSt, double) }).value.map {
      case Left(err)  => if (!err.equal2Code("dlg.canceled")) DlgShowError.show(List(err)) else info("DlgCardRegDouble", s"dialog canceled: ${err}")
      case Right(res) => updViewComp(coId)
    }
  }

  //
  //  VIEW Routines
  //
  def updViewCoPhContent(coId: Long, coPhId: Int):Unit = App.tourney.getCoPh(coId, coPhId) map { coph => updViewCoPhContent(coph) }
  def updViewCoPhContent(coph: CompPhase):Unit = {

    import clientviews.organize.competition.CoPhCard
    import org.scalajs.dom.raw.HTMLInputElement 
    import org.scalajs.dom.document

    // set visiblity of central participant card
    setVisible(gUE("ParticipantCardMain"), coph.coPhId == 1 || coph.coPhId == 0 )

    //println(s"updViewCoPhContent ${coph.coId}  ${coph.coPhId}")
    setHtml(gE(s"CoPhCardContent_${coph.coId}_${coph.coPhId}"), clientviews.organize.competition.CoPhCard.html.Content(coph))

    // set pant candiates view 
    if (coph.coPhId > 1) {
      // val content = CoPhCard.html.Pants(coph).toString
      // println(s"updViewCoPhContent PantCard: ${coph.candidates.size} ${content.take(20)}")

      setHtml(gUE(s"PantCard_${coph.coId}_${coph.coPhId}"), CoPhCard.html.Pants(coph) )
      collapse(s"PantCard_${coph.coId}_${coph.coPhId}", false)
      setViewCoPhBase(coph)
    }
    // calculate no of players selected candidates
    setViewCoPhGameSystem(coph)
    activateCoPhTab(coph)
  }

  // setPantCandidates - initialize pant candidates 
  def setPantCandidates(coph: CompPhase) = {
    if (coph.coPhId == 1) {
      coph.candidates = (App.tourney.pl2co.filterKeys(_._2 == coph.coId).map {
        x => (App.tourney.getPant(App.tourney.comps(coph.coId).typ, SNO(x._2.sno)), x._2.status == PantStatus.REDY)
      }).to(ArrayBuffer)
    } else coph.baseCoPhId match {
      case None        => coph.candidates = ArrayBuffer[(Pant, Boolean)]()
      case Some(bCoId) => if (bCoId == 0) {
        coph.candidates = (App.tourney.pl2co.filterKeys(_._2 == coph.coId).map {
          x => (App.tourney.getPant(App.tourney.comps(coph.coId).typ, SNO(x._2.sno)), x._2.status == PantStatus.REDY)
        }).to(ArrayBuffer)
      } else App.tourney.getCoPh(coph.coId, bCoId) map { bCoph => 
        coph.candidates     = bCoph.getQualiPants(coph.quali) 
        coph.candInfo       = bCoph.getQualiInfo
      }
    } 
    //println(s"PandCandidate: ${coph.candidates.mkString(":")}")
    coph.noPlayers = coph.candidates.filter(_._2 == true).size
  }  


  // set pre selected base
  def setViewCoPhBase(coph: CompPhase) = {
    def o2S(value: Option[Int]):String = value match { case None => "None"; case Some(x) => x.toString }

    val coPhBaseList = new scala.collection.mutable.ListBuffer[(Int,String)]()
    for (i <- 1 until coph.coPhId) App.tourney.getCoPh(coph.coId, i) map { coph => coPhBaseList += ((i, coph.name)) }

    // set selectable options
    val selOptions = new StringBuilder()
    selOptions ++= s"<option value='None'>${getMsg("selectPants")}</option>"
    selOptions ++= s"<option value='0'>${getMsg("entireCompetition")}</option>"
      
    for (bE <- coPhBaseList) { selOptions ++= s"<option value='${bE._1}'>${bE._2}</option>" }
    setHtml(gUE(s"BaseCoPh_${coph.coId}_${coph.coPhId}"), selOptions)
    selectOption(s"BaseCoPh_${coph.coId}_${coph.coPhId}", o2S(coph.baseCoPhId))
  }
  

  // setViewGameSystem - calculate possible game systems
  //                     set the corresponding options
  def setViewCoPhGameSystem(coph: CompPhase) = {
    val cfgOptions = sysOptions(coph.noPlayers)
    //println(s"setViewGameSystem coId ${coph.coId} coPhId: ${coph.coPhId} size: ${coph.noPlayers } options: ${cfgOptions}")
    val selOptions = new StringBuilder(s"<option value='${CompPhaseCfg.CFG.id}' selected>${gM(CompPhaseCfg.CFG.msgCode)}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</option>")    
    for (cfg <- cfgOptions) { selOptions ++= s"<option value='${cfg.id}'>${gM(cfg.msgCode)}</option>" }

    if (!cfgOptions.contains(coph.coPhCfg)) coph.coPhCfg = CompPhaseCfg.CFG
    setHtml(gUE(s"GameSystem_${coph.coId}_${coph.coPhId}"), selOptions.toString)
    selectOption(s"GameSystem_${coph.coId}_${coph.coPhId}", coph.coPhCfg.id.toString)
  }    


  def activateCoPhTab(coph: CompPhase) = {
    import org.scalajs.dom.raw.HTMLElement
    val aNodes = gE("CoPhCardLinks").getElementsByTagName("a")  
    // set register/tab active
    for( i <- 0 to aNodes.length-1) {
      val elem   = aNodes.item(i).asInstanceOf[HTMLElement]
      val id     = getData(elem, "coPhId", 0)
      val activ = (coph.coPhId == id)

      // set link and content active/inactive
      setClass(elem, activ, "active")
      setVisible(gE(s"CoPhCardContent_${coph.coId}_${id}"), activ)
    }
  }   


  //
  //  Helper Routines
  //

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
      if (size <= Tourney.MaxSizeRR) result += CompPhaseCfg.RR
      // if (size <= Tourney.MaxSizeSW) result += CompPhaseCfg.SW
      result.to(List)
    }

    size match {
      case 3 | 4 | 5 => List(CompPhaseCfg.RR,     CompPhaseCfg.KO,     CompPhaseCfg.SW)
      case 6         => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.KO,     CompPhaseCfg.SW,     CompPhaseCfg.RR)
      case 7         => List(CompPhaseCfg.GRPS34, CompPhaseCfg.KO,     CompPhaseCfg.SW,     CompPhaseCfg.RR)
      case 8         => List(CompPhaseCfg.GRPS4,  CompPhaseCfg.KO,     CompPhaseCfg.SW,     CompPhaseCfg.RR)
      case 9         => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 10        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS5,  CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 11        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS56, CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 12        => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS4,  CompPhaseCfg.GRPS6,  CompPhaseCfg.KO, CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 13        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 14        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 15        => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS5,  CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 16        => List(CompPhaseCfg.GRPS4,  CompPhaseCfg.GRPS56, CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 17        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.GRPS56, CompPhaseCfg.KO, CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 18        => List(CompPhaseCfg.GRPS3,  CompPhaseCfg.GRPS45, CompPhaseCfg.GRPS6,  CompPhaseCfg.KO, CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 19        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)
      case 20        => List(CompPhaseCfg.GRPS4,  CompPhaseCfg.GRPS5,  CompPhaseCfg.KO,     CompPhaseCfg.SW, CompPhaseCfg.RR)

      case i if (i >= 21 && i <= 128) => sysOptions21to128(i)
      case _                          => List()        
    }
  }


//
// ACTIONS
//
def actionAddCoPh(coId: Long): Unit = 
  DlgInputTxt.show(getMsg("dlg.coph.new.hdr"), getMsg("dlg.coph.new.lbl"), getMsg("dlg.coph.new.plh"), gM("std.btn.close"), gM("std.btn.add"), chkCoPhName) map { 
    case Left(err)    => error("AddCompPhase.action", s"DlgInputTxt ${err}")
    case Right(value) => addCompPhase(coId, value).map {
      case Left(err)    => error("AddCompPhase.action", s"addCompPhase ${err}")
      case Right(coph)  => {
        App.tourney.comps(coId).setCurCoPhId(coph.coPhId)
        updViewComp(coId)             
        collapse("CoPhCard", false)
      }     
    }
  }

  def actionCheckPant(coId: Long, sno: SNO, pStatus: PantStatus.Value ) = 
    setPantStatus(coId, sno, pStatus).map { 
      case Left(err)  => DlgShowError.show(List(err)) 
      case Right(res) => {
        // set statistic/numbers
        val p2c = App.tourney.pl2co.values.filter(_.coId == coId).toSeq
        val (total, activ) = (p2c.length, p2c.filter(_.status > PantStatus.REGI).length )
        setHtml(gUE(s"Counter_${coId}"), s"${total}/${activ}")

        //update competition phase
        App.tourney.getCoPh(coId, App.tourney.getCurCoPhId) map { coph => {
          setPantCandidates(coph)
          updViewCoPhContent(coph)
        }}
      }
    }


  def actionStartDrawCoPh(coId: Long, coPhId: Int) = App.tourney.getCoPh(coId, coPhId) map { coph => {
    info("StartDrawCoPh", s"START coId: ${coph.coId} coPhId: ${coph.coPhId}") 
    coph.draw(App.tourney.comps(coph.coId).typ, App.tourney.getCoPhList(coId, coPhId)) match {
      case Left(err)  => error("actionStartDrawCoPh", s"draw -> ${err}")
      case Right(res) => updateCompStatus(coph.coId).map {
        case Left(err)  => error("actionStartDrawCoPh", s"setCompStatus -> ${err}")
        case Right(res) => saveCompPhase(coph).map {
          case Left(err)  => error("actionStartDrawCoPh", s"saveCompPhase -> ${err}")
          case Right(res) => {
            App.execUseCase("OrganizeCompetitionDraw", "", "")
            info("StartDrawCoPh", s"SUCCESS coId: ${coph.coId} coPhId: ${coph.coPhId}") 
          }
        }
      }  
    }
  }}    


  // actionStartInputCoPh
  def actionStartInputCoPh(coId: Long, coPhId: Int) = App.tourney.getCoPh(coId, coPhId) map { coph => {  
    coph.setStatus(CompPhaseStatus.EIN)
    updateCompStatus(coph.coId).map {
      case Left(err)  => error("actionStartInputCoPh", s"updateCompStatus -> ${err}")
      case Right(res) => saveCompPhase(coph).map {
        case Left(err)   => error("StartInputCoPh", s"${err}") 
        case Right(res)  => App.execUseCase("OrganizeCompetitionInput", "", "")  
      }
    }    
  }}

  def actionDeletePant(coId: Long, sno: String) =
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
            updViewComp(coId)
          }
        }
        case false => info("DeleteParticipant", s"not confirmed for: ${sno}")
      }}
    }

  def actionUploadPant(coId: Long) = {
    import shared.model.SexTyp

    DlgBoxFile.show(gUM("loadPantsHdr"), gUM("loadPantsContent")).map { 
      case Left(err)      => {}
      case Right(content) => {
        val pantList = scala.collection.mutable.ListBuffer[Player]()
        val lineErrList = scala.collection.mutable.ListBuffer[Int]()
        val lines = content.split("\n")
        lines.zipWithIndex.foreach { case (line, index) => Player.fromCSV(line) match { 
          case Left(err)     => if (!err.is("return001.csv.hdr.player")) lineErrList += (index+1)
          case Right(player) => if (!List("name","lastname").contains(player.lastname.toLowerCase()) ) pantList += player
        }}
        
        regSingle(coId, pantList.toList, PantStatus.REDY).map {
          case Left(err)   => DlgInfo.error(gM("uploadCSVError"), getError(err))
          case Right(res)  => updViewComp(coId) 
        }
      }  
    }
  }    


  def actionDemoBtn(coId : Long, coPhId: Int, value: Boolean) = App.tourney.getCoPh(coId, coPhId) map { coph => {  
    coph.demo = value
    setVisible(gE(s"InputDemoBtn_${coId}_${coPhId}"), value)
  }}

  def actionResetCfgCoPh(coph: CompPhase) = { 
    coph.reset(CompPhaseStatus.CFG)
    updViewCoPhContent(coph)
    updateCompStatus(coph.coId).map {
      case Left(err)  => error("actionResetCfgCoPh", s"updateCompStatus -> ${err}")
      case Right(res) => saveCompPhase(coph).map {
        case Left(err)  => error("actionResetCfgCoPh", s"saveCompPhase -> ${err}")
        case Right(res) => info("StartDrawCoPh", s"SUCCESS coId: ${coph.coId} coPhId: ${coph.coPhId}") 
      }
    }
  }

  def actionResetDrawCoPh(coph: CompPhase) = {  
    coph.reset(CompPhaseStatus.AUS)
    updViewCoPhContent(coph)
    updateCompStatus(coph.coId).map {
      case Left(err)  => error("actionStartDrawCoPh", s"updateCompStatus -> ${err}")
      case Right(res) => saveCompPhase(coph).map {
        case Left(err)  => error("actionStartDrawCoPh", s"saveCompPhase -> ${err}")
        case Right(res) => App.execUseCase("OrganizeCompetitionDraw", "", "")
      }
    }
  }

  def actionResetInputCoPh(coph: CompPhase) = resetMatches(coph.coId, coph.coPhId) map { 
    case Left(err)  => error("actionResetInputCoPh", s"resetMatches -> ${err}")
    case Right(res) => {
      updViewCoPhContent(coph)
      updateCompStatus(coph.coId).map {
        case Left(err)  => error("actionResetInputCoPh", s"updateCompStatus -> ${err}")
        case Right(res) => saveCompPhase(coph).map {
          case Left(err)  => error("actionResetInputCoPh", s"saveCompPhase -> ${err}")
          case Right(res) => App.execUseCase("OrganizeCompetitionInput", "", "")
        }
      }
    }  
  }  

  def actionDeleteCoPh(coph: CompPhase) = delCompPhase(coph.coId, coph.coPhId) map {
    case Left(err)  => DlgInfo.error(gM("msg.error.hdr"), getError(err))
    case Right(res) => render()
  }

  def actionSortPant(coph: CompPhase, sortTyp: String, direction: String): Unit = {
    import clientviews.organize.competition.CoPhCard
    val updown = (direction == "none" || direction == "down")  // up -> true else false
    def newDir(updo: Boolean) = if (updo) "up" else "down" 
    sortTyp match {
      case "name"   => coph.candidates = if (updown) coph.candidates.sortBy( _._1.name ) else coph.candidates.sortBy( _._1.name ).reverse
      case "sno"    => coph.candidates = if (updown) coph.candidates.sortBy( _._1.sortSno ) else coph.candidates.sortBy( _._1.sortSno ).reverse
      case "ttr"    => coph.candidates = if (updown) coph.candidates.sortBy( _._1.rating ) else coph.candidates.sortBy( _._1.rating ).reverse
      case "club"   => coph.candidates = if (updown) coph.candidates.sortBy( _._1.club ) else coph.candidates.sortBy( _._1.club ).reverse
      case "signup" => coph.candidates = if (updown) coph.candidates.sortBy( _._2 ) else coph.candidates.sortBy( _._2 ).reverse
      case "info"   => coph.candidates = if (updown) coph.candidates.sortBy( _._1.qInfo ) else coph.candidates.sortBy( _._1.qInfo ).reverse
    }

    setHtml(gUE(s"PantCard_${coph.coId}_${coph.coPhId}"), CoPhCard.html.Pants(coph) )
    // set new direction  
    val elem = gEqS(s"PantCandTbl_${coph.coId}_${coph.coPhId}", s"[data-sort='${sortTyp}']")
    setData(elem, "sortDir", newDir(updown) )

    println(s"actionSortPant: direction: ${direction} ${newDir(updown)}")
    
  }


  // Single Participants (sno, lastname, firstname, ttr, clubname, birthyear, status)
  // actionSortSingle based on genSingleTblData
  def actionSortSingle(coId: Long, sortTyp: String, direction: String): Unit = {
    import shared.utils.Routines._

    val updown = (direction == "none" || direction == "down")  // up -> true else false
    val editable = App.tourney.comps(coId).status.equalsTo(CompStatus.CFG, CompStatus.READY)
    def newDir(updo: Boolean) = if (updo) "up" else "down" 

    val singleTbl = sortTyp match {
      case "sno"    => if (updown) genSingleTblData(App.tourney, coId).sortBy ( x => getMDLongArr(x._1)(0)) 
                       else genSingleTblData(App.tourney, coId).sortBy ( x => getMDLongArr(x._1)(0)).reverse
      case "name"   => if (updown) genSingleTblData(App.tourney, coId).sortBy ( _._2 ) else genSingleTblData(App.tourney, coId).sortBy ( _._2 ).reverse
      case "ttr"    => if (updown) genSingleTblData(App.tourney, coId).sortBy ( _._4 ) else genSingleTblData(App.tourney, coId).sortBy ( _._4 ).reverse
      case "club"   => if (updown) genSingleTblData(App.tourney, coId).sortBy ( _._5 ) else genSingleTblData(App.tourney, coId).sortBy ( _._5 ).reverse
      case "bYear"  => if (updown) genSingleTblData(App.tourney, coId).sortBy ( _._6 ) else genSingleTblData(App.tourney, coId).sortBy ( _._6 ).reverse
      case "status" => if (updown) genSingleTblData(App.tourney, coId).sortBy ( _._7 ) else genSingleTblData(App.tourney, coId).sortBy ( _._7 ).reverse
    }
    setHtml(gUE("ParticipantCard"), SingleCard(coId, singleTbl, editable )) 

    val elem = gEqS(uc(s"SingleTbl_${coId}"), s"[data-sort='${sortTyp}']")
    setData(elem, "sortDir", newDir(updown) )
  }
}