package scalajs.usecase.dialog

// Start TestCases
//                                       

import scala.collection.mutable.{ ArrayBuffer }
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Success, Failure }
import scala.util.matching
import scala.concurrent._

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.document
import org.scalajs.dom.html.Input
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext._

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.model.{ Tourney, CompPhase, CompPhaseCfg ,Player, SNO, PantStatus }
import shared.model.CompPhase._
import shared.utils._
import clientviews.dialog.DlgCardCfgCompPhase.html

@JSExportTopLevel("DlgCardCfgCompPhase")
object DlgCardCfgCompPhase extends BasicHtml 
  with TourneySvc 
{
  this: BasicHtml =>
  case class Result(var name: String, var config: CompPhaseCfg.Value, var category: Int, var winSets: Int, var qualify: Boolean, var pants: ArrayBuffer[PantSelect])
  case class PantSelect(sno: SNO, name: String, info: String, var checked: Boolean, val qualify: QualifyTyp.Value=QualifyTyp.None)
  
  object QualifyTyp extends Enumeration {
    val None   = Value("0") 
    val Winner = Value("1")
    val Looser = Value("2")
    val All    = Value("3") 
    def get(value: String):QualifyTyp.Value = {
      value match {
        case "0" => QualifyTyp.None
        case "1" => QualifyTyp.Winner
        case "2" => QualifyTyp.Looser
        case "3" => QualifyTyp.All
      }
    }
  } 

  implicit val ucp  = UseCaseParam("APP__DlgCardCfgCompPhase", "dlg.card.cfg.compphase", "DlgCardCfgCompPhase", "dlgcardcfgcompphase", scalajs.AppEnv.getMessage _ )

  var coId  = 0L
  var size  = 0
  var qualifyMode = QualifyTyp.None
  var pants = ArrayBuffer[PantSelect]()

  val result = Result("", CompPhaseCfg.CFG, CompPhase.Category_Start, 0, true, pants)


  /** show dialog returns either tupel result or an error
   *  result tupel (name, config, category, winSets, pants)
   */
  def show(coIdInput: Long, qualify: QualifyTyp.Value, pantsInput: ArrayBuffer[PantSelect]): Future[Either[Error, Result]] = 
  {
    val p = Promise[Boolean]()
    val f = p.future

    def cancel() = {   
      offEvents(gE(uc("Modal")), "hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = {
      validate() match {
        case Left(err) => DlgShowError.show(List(err))
        case Right(res)   => {
          if (!p.isCompleted) p success res
          //disable modal first, then hide
          offEvents(gE(uc("Modal")), "hide.bs.modal")
          doModal(gE(uc("Modal")), "hide")
        }  
      }
    }
    
    // load modal dialog 
    loadModal(html.Main(), ucp)
    coId = coIdInput

    pants       = pantsInput
    size        = pants.filter(_.checked).size
    //set pant view - init view for participant selection
    setHtml("PantCard", html.Pants(pants))
    
    // set qualify checkbox
    setVisible("qualifyWinners", qualify!=QualifyTyp.None)
    if (qualify!=QualifyTyp.None) {
      setDisabled("Winners", qualify!=QualifyTyp.All)
      setCheckbox("Winners", qualify==QualifyTyp.All | qualify==QualifyTyp.Winner) 
      qualifyMode = ite(qualify==QualifyTyp.All | qualify==QualifyTyp.Winner, QualifyTyp.Winner, QualifyTyp.Looser)
    } else {
      qualifyMode = QualifyTyp.None
    }
    setMainView(size)

    // register routines for cancel and submit
    onEvents(gUE("Modal"), "hide.bs.modal", () => cancel())
    onClick(gUE("Submit"), (e: Event) => submit(e))
    doModal(gUE("Modal"), "show")

    f.map(x => Right(result)).recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }


  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {     
      case "Selection" => { 
        val cst = getInput(gE(uc("CfgSelection")), CompPhaseCfg.UNKN)  // competition phase category
        setDisabled("Submit", cst == CompPhaseCfg.UNKN)
        setHtml("CfgInfo", CompPhase.getDescription(cst, size, gM )) 
        setInput("CfgName", genCfgName(cst, 0, true))
      }
      
      case "Check" => elem.asInstanceOf[Input].value.toIntOption match {
        case None        => error("actionEvent", s"key: 'Check' - invalid index ")
        case Some(index) => {
          pants(index).checked = elem.asInstanceOf[Input].checked
          size = pants.filter(_.checked).size
          setMainView(size)
          debug("actionEvent", s"value: size: ${size} ${index}  ${pants(index).checked} ")
        }
      }

      case "Winners" =>  {
        val pantTbl = gUE("PantTbl")
        val winner  = getCheckbox(gUE("Winners"))

        qualifyMode = ite(winner, QualifyTyp.Winner, QualifyTyp.Looser)
        pants.foreach { pant => 
          val elem = pantTbl.querySelector(s"[data-sno='${pant.sno.value}']").asInstanceOf[HTMLElement] 
          setCheckbox(elem, if (winner) pant.qualify == QualifyTyp.Winner else pant.qualify == QualifyTyp.Looser)
        }
      }

      case "Close"        => offEvents(gUE("Modal"), "hide.bs.modal"); doModal(gUE("Modal"), "hide")

      case _              => {}
    }
  }


  /** validate input configuration input return selected option or a List of Errors
   * 
   */ 
  def validate(): Either[Error, Boolean] = {
    result.config  = getInput(gUE("CfgSelection"), CompPhaseCfg.UNKN)
    result.name    = getInput(gUE("CfgName"))
    result.winSets = getInput(gUE("CfgWinset"), 0)
    result.qualify = qualifyMode == QualifyTyp.None || qualifyMode == QualifyTyp.Winner || qualifyMode == QualifyTyp.All
    result.pants   = pants

    //set participant status 
    //pants.foreach { entry => trny.setPantStatus(coId, entry.sno.value, if (entry.checked) Pant.REDY else Pant.SIGN) }
    if (result.config == CompPhaseCfg.UNKN || result.name == "" || result.winSets == 0 ) Left(Error("err0175.DlgCardCfgSection")) else Right(true)
  }

  
  // setMainView
  def setMainView(size: Int): Unit = {
    val cfgOptions = sysOptions(size)
    val selOptions = new StringBuilder(s"<option value='${CompPhaseCfg.UNKN}' selected>---</option>")
    for (cfg <- cfgOptions) {
      val msg = getMsg(s"option.${cfg}")
      selOptions ++= s"<option value='${cfg}'>${msg}</option>" 
    }
    setHtml("lbl.Selection", getMsg("lbl.Selection", size.toString)) 
    setHtml("CfgSelection", selOptions.toString)
    setInput("CfgSelection", "")
    setHtml("CfgInfo", getMsg("option.info.2"))
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
      case 3 | 4 | 5 => List(CompPhaseCfg.JGJ, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 6         => List(CompPhaseCfg.GRPS3, CompPhaseCfg.KO, CompPhaseCfg.SW, CompPhaseCfg.JGJ)
      case 7         => List(CompPhaseCfg.GRPS34, CompPhaseCfg.KO, CompPhaseCfg.SW, CompPhaseCfg.JGJ)
      case 8         => List(CompPhaseCfg.GRPS4, CompPhaseCfg.KO, CompPhaseCfg.SW, CompPhaseCfg.JGJ )
      case 9         => List(CompPhaseCfg.GRPS3, CompPhaseCfg.GRPS45, CompPhaseCfg.KO, CompPhaseCfg.SW, CompPhaseCfg.JGJ)
      case 10        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS5, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 11        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS56, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 12        => List(CompPhaseCfg.GRPS3, CompPhaseCfg.GRPS4, CompPhaseCfg.GRPS6, CompPhaseCfg.KO, CompPhaseCfg.SW )
      case 13        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO, CompPhaseCfg.SW )
      case 14        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 15        => List(CompPhaseCfg.GRPS3, CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS5, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 16        => List(CompPhaseCfg.GRPS4, CompPhaseCfg.GRPS56, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 17        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.GRPS56, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 18        => List(CompPhaseCfg.GRPS3, CompPhaseCfg.GRPS45, CompPhaseCfg.GRPS6, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 19        => List(CompPhaseCfg.GRPS34, CompPhaseCfg.GRPS45, CompPhaseCfg.KO, CompPhaseCfg.SW)
      case 20        => List(CompPhaseCfg.GRPS4, CompPhaseCfg.GRPS5, CompPhaseCfg.KO, CompPhaseCfg.SW)

      case i if (i > 21 && i <= 128) => sysOptions21to128(i)
      case _                         => List()        
    }
  }

  // genCfgName - generate configuration name proposal
  def genCfgName(option: CompPhaseCfg.Value, coPhId: Int, winner: Boolean=true): String = {
    option match {
      case CompPhaseCfg.GRPS3 | CompPhaseCfg.GRPS34 | CompPhaseCfg.GRPS4 | CompPhaseCfg.GRPS45 | CompPhaseCfg.GRPS5 | CompPhaseCfg.GRPS56 | CompPhaseCfg.GRPS6  => {
        if      (coPhId == 0)  { getMsg(s"name.1") } 
        else if (winner)       { getMsg(s"name.2") } 
        else                   { getMsg(s"name.4") }
      }
      case CompPhaseCfg.KO | CompPhaseCfg.SW | CompPhaseCfg.JGJ => if (winner) getMsg(s"name.3") else getMsg(s"name.4") 
      case CompPhaseCfg.UNKN         => ""
      case _                         => getMsg(s"name.3") 
    }
  }  

}