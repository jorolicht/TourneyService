package scalajs.usecase.organize

// Start TestCases in Javascript Console
// test -s compphase -n 1

import scala.collection.mutable.{ ArrayBuffer }
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import shared.model._
import shared.model.CompPhase._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils.Constants._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionCtrl")
object OrganizeCompetitionCtrl extends UseCase("OrganizeCompetitionCtrl")  
  with TourneySvc
{
  import org.scalajs.dom.raw.Element
  import org.scalajs.dom.raw.HTMLElement
  import org.scalajs.dom.raw.HTMLTableElement
  import scala.collection.mutable.ListBuffer

  case class PantSelect(sno: SNO, name: String, info: String, var checked: Boolean)
  var pants = new Array[PantSelect](0)

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Ctrl")
  }

  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    import org.scalajs.dom.html.Input
    import scalajs.usecase.dialog.DlgShowError
    //val (coPhase, coId, coPhId) = getCompEnv(elem) 
    //debug("actionEvent", s"key: ${key} coId: ${coId} coPhId: ${coPhId}")

    key match {

      case "Start" => {
        val (coId, coPhId) = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0))

        addCompPhase(coId, coPhId, getInput(s"CfgOptions_${coId}", 0), getInput("name",""), getInput("CfgWinset", 0)) map {
          case Left(err)    => DlgShowError.show(List(err))
          case Right(coph)  => {
      
            // set pants in player competition mapping
            if (coPhId == 0) { pants.foreach { p => checkPantEntry(p.sno.value, coId, p.checked) } } 

            val pEntrys = (pants.filter { pant => pant.checked } map { x => x.sno.getPantEntry(coId)(App.tourney) }).to(ArrayBuffer)

            // val pEntrys = (App.tourney.pl2co.filterKeys(_._2 == coId).filter { case (x1,x2) => x2.status == Pant.REDY } map { x =>
            //   SNO(x._2.sno).getPantEntry(coId)(App.tourney)
            // }).to(ArrayBuffer)
            
            coph.draw(pEntrys, App.tourney.comps(coId).typ)

            AppEnv.coPhIdMap(coId) = coph.coPhId
            App.execUseCase("OrganizeCompetitionDraw", "", "")
            debug("Start", s"success ${coph.toString}")
          }  
        }

        debug("Start", s"coId: ${coId} coPhId: ${coPhId}")
      } // Start


      case "WinLooAll" => {
        val (coPhase, coId, coPhId) = getCompEnv(elem)
        val wlaSelection   = getRadioBtn("WinLooAll", 0)  
        var cnt            = 0
        coPhase.groups.foreach { g => for (i <- 0 until g.size) {
          val checkElt = getElemById(s"PantTbl").asInstanceOf[HTMLTableElement].querySelector(s"[data-check='${g.pants(i).sno}']")
          val check = evalPantSelOpt(wlaSelection, g.size, g.pants(i).place._1)
          setCheckbox(checkElt, check)
          if (check) cnt = cnt +1

        }}
        setHtml(s"NoPlayer_${coId}_${coPhId}", cnt.toString)
        debug("WinLooAll", s"value: ${wlaSelection}")
      } // WinLooAll

      case "Check" => elem.asInstanceOf[Input].value.toIntOption match {
        case None        => error("actionEvent", s"key: 'Check' - invalid index ")
        case Some(index) => {
          val coId = getData(elem, "coId", 0L)
          pants(index).checked = elem.asInstanceOf[Input].checked
          val size = pants.filter(_.checked).size
          setPlayerInfo(coId, size, pants.length)
          setCfgOptions(coId, size)
          debug("Check", s"coId: ${coId} size: ${size} ${index}  ${pants(index).checked} ")
        }
      } // Check

      case "Selection" => elem.asInstanceOf[Input].value.toIntOption match {
        case None        => error("Selection", s"invalid value")
        case Some(value) => {
          debug("Selection", s"value: ${value}")
        }
      } // Selection



      case "DemoBtn"          => {
        val (coPhase, coId, coPhId) = getCompEnv(elem)
        coPhase.demo = getCheckbox("DemoBtn")
        setVisible(s"${coId}_${coPhId}", coPhase.demo)(UCP("InputDemoBtn"))
      }  

      case _  => error("Invalid Key", s"value: ${key}")

    }
  }

  // set control page for competition start
  def setStartPage(coId: Long): Unit = {
    debug("setStartPage", s"coId: ${coId}")
    pants = getPants(coId)(App.tourney)
    val size = pants.filter(_.checked).size
    setMainContent(clientviews.organize.competition.ctrl.html.CtrlCardStart(coId, pants).toString)
  
    setPlayerInfo(coId, size, pants.length)
    setCfgOptions(coId, size)
  }


  // possible configuration options
  def setCfgOptions(coId: Long, size: Int): Unit = {
    val cfgOptions = sysOptions(size)
    val selOptions = new StringBuilder(s"<option value='${CPC_UNKN}' selected>---</option>")
    for (cfg <- cfgOptions) {
      val msg = getMsg(cfg.toString)(UCP("","competition.phase.option"))
      selOptions ++= s"<option value='${cfg}'>${msg}</option>" 
    }  
    setHtml(s"CfgOptions_${coId}", selOptions.toString)
  }  

  // set player information e.g. number of participating, registered player
  def setPlayerInfo(coId: Long, participating: Int, registered: Int): Unit = {
    setHtml(s"PlayerInfo_${coId}",getMsg("val.noPlayers", participating.toString, registered.toString))
  }


    // val cfgOptions = sysOptions(size)
    // val selOptions = new StringBuilder(s"<option value='${CPC_UNKN}' selected>---</option>")
    // for (cfg <- cfgOptions) {
    //   val msg = getMsg(s"option.${cfg}")
    //   selOptions ++= s"<option value='${cfg}'>${msg}</option>" 
    // }
    // setHtml("lbl.Selection", getMsg("lbl.Selection", size.toString)) 
    // setHtml("CfgSelection", selOptions.toString)
    // setInput("CfgSelection", "")
    // setHtml("CfgInfo", getMsg("option.info.2"))


  // set control page for competition phase
  def setPage(coId: Long, coPhId: Int)(implicit coPhase: CompPhase): Unit = {

    def initContent(coId: Long, coPhId: Int, elem: Element, value: String) = {
      if (!exists(s"Ctrl_${coId}_${coPhId}")) { elem.innerHTML = value }
    }

    val coPhCfg  = coPhase.coPhCfg 
    val ctrlBase = getElemById_(s"CtrlContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']")

    coPhCfg match {
      case CPC_GRPS3 | CPC_GRPS34 | CPC_GRPS4 | CPC_GRPS45| CPC_GRPS5 | CPC_GRPS56 | CPC_GRPS6 => {
        initContent(coId, coPhId, ctrlBase, clientviews.organize.competition.ctrl.html.CtrlCardGr(coId, coPhId).toString)
        setHtml(s"Status_${coId}_${coPhId}", getMsg(s"_competition.phase.status.${coPhase.status}"))

        val pantTblElt = getElemById(s"PantTbl").asInstanceOf[HTMLTableElement]
        val pantSelOpt = getRadioBtn("WinLooAll", 0)

        var selCount   = 0
        // check if table already initialized
        if (pantTblElt.rows.length == 1) {
          var rowCount = 1
          coPhase.groups.foreach { g => for (i <- 0 until g.size) {
            val rowElt = pantTblElt.insertRow(rowCount).asInstanceOf[HTMLElement]
            val check = evalPantSelOpt(pantSelOpt, g.size, g.pants(i).place._1)
            if (check) selCount = selCount + 1 
            setHtml(rowElt, clientviews.organize.competition.ctrl.html.PantEntry(rowCount, 
                              getMsg("fmt.single", g.pants(i).name, g.pants(i).club), 
                              getMsg("fmt.grp.info", g.name, g.pants(i).getPlace),
                              g.pants(i).sno, check ))
            rowCount = rowCount + 1
          }}
        } else {
          coPhase.groups.foreach { g => for (i <- 0 until g.size) {
            val infoElt  = pantTblElt.querySelector(s"[data-info='${g.pants(i).sno}']").asInstanceOf[HTMLElement]
            val checkElt = pantTblElt.querySelector(s"[data-check='${g.pants(i).sno}']")
            val check = evalPantSelOpt(pantSelOpt, g.size, g.pants(i).place._1)
            if (check) selCount = selCount + 1
            setHtml(infoElt, getMsg("fmt.grp.info", g.name, g.pants(i).getPlace))
            setCheckbox(checkElt, check)
          }} 
        }
        setCheckbox("DemoBtn", coPhase.demo)
        setHtml(s"NoPlayer_${coId}_${coPhId}", selCount.toString) 
        setHtml(s"Status_${coId}_${coPhId}", getMsg(s"${coPhase.getStatus}")(UCP("","competition.phase.status"))) 
        setHtml(s"Config_${coId}_${coPhId}", coPhase.getDescription(BasicHtml.getMsg_)) 
        setHtml(s"Finished_${coId}_${coPhId}", getMsg("finished", s"${coPhase.mFinished}", s"${coPhase.mTotal}")) 

        val finished = (coPhase.mFinished == coPhase.mTotal)
        setVisible("SuccCompPhaseDisabled", !finished) 
        setVisible("SuccCompPhaseEnabled", finished)
        setDisabled(s"StartFollowingBtn_${coId}_${coPhId}", !finished)
        setDisabled(s"StartBtn_${coId}_${coPhId}", !(coPhase.status == CPS_AUS))
        setDisabled(s"ResetBtn_${coId}_${coPhId}", !(coPhase.mFinished != CPS_AUS))
        setDisabled(s"DeleteBtn_${coId}_${coPhId}", !(coPhase.status != CPS_UNKN))
      }

      case CPC_KO => {
        initContent(coId, coPhId, ctrlBase, clientviews.organize.competition.ctrl.html.CtrlCardKo(coId, coPhId).toString)

      }
      case CPC_SW => {
        initContent(coId, coPhId, ctrlBase, clientviews.organize.competition.ctrl.html.CtrlCardSw(coId, coPhId).toString)

      }
      case _ => {
        initContent(coId, coPhId, ctrlBase, clientviews.organize.competition.ctrl.html.CtrlCardUnknown(coId, coPhId).toString)
      }

    }
  }

  def evalPantSelOpt(option: Int, grpSize: Int, position: Int): Boolean = {
    option match {
      case PANT_SELECT_ALL    => true
      case PANT_SELECT_WINNER => (grpSize + grpSize%2)/2 >= position  // 5 -> 1,2,3
      case PANT_SELECT_LOOSER => position > (grpSize + grpSize%2)/2   // 5 -> 4,5
      case _                  => false  
    }
  }

 
  // getPants with status signed or ready
  def getPants(coId: Long)(implicit trny: Tourney): Array[PantSelect] = {
    // initialize participants to be shown 
    // only participants with status signed or ready
    (trny.pl2co.filterKeys(_._2 == coId).filter { case (x1,x2) => x2.status == Pant.SIGN || x2.status == Pant.REDY } map { x =>
      val sno = SNO(x._2.sno) 
      val (snoValue, name, club, ttr) = sno.getInfo(trny.comps(coId).typ)
      val enabled = (x._2.status == Pant.REDY)
      // show name, club name and ttr value
      PantSelect(sno, s"${name} [${club}]", s"TTR: ${ttr}", enabled) 
    }).to(Array).sortBy(x => (!x.checked, x.name))
  }


    // size = pants.filter(_.checked).size
    // //set pant view - init view for participant selection
    // setHtml("PantTbl", html.Pants(pants))

  // sysOptions generate all possible options for given size
  def sysOptions(size: Int): List[Int] = {
    def sysOptions21to128(size: Int): List[Int] = {
      val result = ArrayBuffer[Int]()  
      if (size % 3 == 0) result += CPC_GRPS3 else result += CPC_GRPS34
      if (size % 4 == 0) result += CPC_GRPS4 else result += CPC_GRPS45
      if (size % 5 == 0) result += CPC_GRPS5 else result += CPC_GRPS56
      result += CPC_KO
      result += CPC_SW
      result.to(List)
    }

    size match {
      case 3 | 4 | 5 => List(CPC_JGJ, CPC_KO, CPC_SW)
      case 6         => List(CPC_GRPS3, CPC_KO, CPC_SW, CPC_JGJ)
      case 7         => List(CPC_GRPS34, CPC_KO, CPC_SW, CPC_JGJ)
      case 8         => List(CPC_GRPS4, CPC_KO, CPC_SW, CPC_JGJ )
      case 9         => List(CPC_GRPS3, CPC_GRPS45, CPC_KO, CPC_SW, CPC_JGJ)
      case 10        => List(CPC_GRPS34, CPC_GRPS5, CPC_KO, CPC_SW)
      case 11        => List(CPC_GRPS34, CPC_GRPS56, CPC_KO, CPC_SW)
      case 12        => List(CPC_GRPS3, CPC_GRPS4, CPC_GRPS6, CPC_KO, CPC_SW )
      case 13        => List(CPC_GRPS34, CPC_GRPS45, CPC_KO, CPC_SW )
      case 14        => List(CPC_GRPS34, CPC_GRPS45, CPC_KO, CPC_SW)
      case 15        => List(CPC_GRPS3, CPC_GRPS34, CPC_GRPS5, CPC_KO, CPC_SW)
      case 16        => List(CPC_GRPS4, CPC_GRPS56, CPC_KO, CPC_SW)
      case 17        => List(CPC_GRPS34, CPC_GRPS45, CPC_GRPS56, CPC_KO, CPC_SW)
      case 18        => List(CPC_GRPS3, CPC_GRPS45, CPC_GRPS6, CPC_KO, CPC_SW)
      case 19        => List(CPC_GRPS34, CPC_GRPS45, CPC_KO, CPC_SW)
      case 20        => List(CPC_GRPS4, CPC_GRPS5, CPC_KO, CPC_SW)

      case i if (i > 21 && i <= 128) => sysOptions21to128(i)
      case _                         => List()        
    }
  }

  def checkPantEntry(sno: String, coId: Long, checked: Boolean): Unit = {
    val status = App.tourney.pl2co((sno, coId)).status
    if (status == Pant.REDY || status == Pant.SIGN) {
      App.tourney.pl2co((sno, coId)).status = if (checked) Pant.REDY else Pant.SIGN 
    }
  }
  

}