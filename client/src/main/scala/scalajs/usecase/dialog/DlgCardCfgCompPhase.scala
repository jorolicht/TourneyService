package scalajs.usecase.dialog

// Start TestCases
// DlgCardCfgSection: http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardCfgSection&ucInfo=1#
//                    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardCfgSection&ucInfo=4#
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


import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import shared.model.{ Tourney, CompPhase, Player, PantSelect, SNO }
import shared.model.CompPhase._
import shared.utils._
import clientviews.dialog.DlgCardCfgCompPhase.html


@JSExportTopLevel("DlgCardCfgCompPhase")
object DlgCardCfgCompPhase extends BasicHtml 
  with TourneySvc with DrawSvc
{
  this: BasicHtml =>
  case class Result(var name: String, var config: Int, var category: Int, var winSets: Int, var pants: ArrayBuffer[PantSelect])

  implicit val ucp     = UseCaseParam("APP__DlgCardCfgCompPhase", "dlg.card.cfg.compphase", "DlgCardCfgCompPhase", "dlgcardcfgcompphase", scalajs.AppEnv.getMessage _ )
  private def load()   = if (!checkId("Modal")) insertHtml_("APP__Load", "afterbegin", html.Main().toString)

  var coId:      Long   = 0L
  var preCoPhId: Int    = 0
  var size:      Int    = 0
  var pants             = new ArrayBuffer[PantSelect]()

  val result = Result("", CompPhase.CPC_UNKN, CompPhase.Category_Start, 0, new ArrayBuffer[PantSelect]() )


  /** show dialog returns either tupel result or an error
   *  result tupel (name, config, category, winSets, pants)
   */
  def show(coIdP: Long, preCoPhIdP: Int, pantsP: ArrayBuffer[PantSelect])
    (implicit trny: Tourney): Future[Either[Error, Result]] = 
  {
    val p = Promise[Boolean]()
    val f = p.future

    def cancel() = {
      $(getId("Modal","#")).off("hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = {
      validate() match {
        case Left(err) => DlgShowError.show(List(err))
        case Right(res)   => {
          if (!p.isCompleted) p success res
          //disable modal first, then hide
          $(getId("Modal","#")).off("hide.bs.modal")
          $(getId("Modal","#")).modal("hide")
        }  
      }
    }
    
    load()
    coId         = coIdP
    preCoPhId    = preCoPhIdP
    pants        = pantsP
    result.pants = pants.filter(_.checked)
    size         = pants.filter(_.checked).size
    setView()

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event)     => submit(e)) 
    $(getId("Modal","#")).modal("show")

    f.map(x => Right(result))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }


  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {     
      case "Selection" => { 
        val cst = getInput("CfgSelection", CPC_UDEF)  // competition phase category
        setDisabled("Submit", cst == CPC_UDEF)
        setHtml("CfgInfo", options2msg(cst, size)) 
        setInput("CfgName", genCfgName(cst, preCoPhId, (preCoPhId==0) || getRadioBtn("WinLoo", false)))
      }

      case "Winner" | "Looser" => {
        val cst = getInput("CfgSelection", CPC_UDEF)
        setInput("CfgName", genCfgName(cst, preCoPhId, (preCoPhId==0) || getRadioBtn("WinLoo", false)))
      }
      
      case "Check" => elem.asInstanceOf[Input].value.toIntOption match {
        case None        => error("actionEvent", s"key: 'Check' - invalid index ")
        case Some(index) => {
          pants(index).checked = elem.asInstanceOf[Input].checked
          result.pants = pants.filter(_.checked)
          size = pants.filter(_.checked).size
          setViewInfoOption(size)
          debug("actionEvent", s"value: size: ${size} ${index}  ${result.pants(index).checked} ")
        }
      }

      case "Close"        => { $(getId("Modal","#")).off("hide.bs.modal");  $(getId("Modal","#")).modal("hide") }

      case _              => {}
    }
  }


  /** validate input configuration input return selected option or a List of Errors
   * 
   */ 
  def validate(): Either[Error, Boolean] = {
    result.config  = getInput("CfgSelection", CPC_UDEF)
    result.name    = getInput("CfgName", "")
    result.winSets = getInput("CfgWinset", 0)
    
    //winSets = getInput()
    if (result.config == CPC_UDEF) Left(Error("err0175.DlgCardCfgSection")) else Right(true)
  }

  // set possible configuration section 
  def setView()(implicit trny: Tourney): Unit = {
    // for now switch off looser round
    // setVisible("BtnRadioWinLoo", coPhId != 0)
    setVisible("BtnRadioWinLoo", false)
    setViewInfoOption(size)
    setHtml("PantTbl", html.Pants(result.pants.toArray))
  }
  
  // setViewInfoOption
  def setViewInfoOption(size: Int): Unit = {
    val cfgOptions = sysOptions(size)
    val selOptions = new StringBuilder(s"<option value='${CPC_UDEF}' selected>---</option>")
    for (cfg <- cfgOptions) {
      val msg= getMsg(s"option.${cfg}")
      selOptions ++= s"<option value='${cfg}'>${msg}</option>" 
    }
    setHtml("lbl.Selection", getMsg("lbl.Selection", size.toString)) 
    setHtml("CfgSelection", selOptions.toString)
    setInput("CfgSelection", "")
    setHtml("CfgInfo", getMsg("option.info.2"))
  }


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

  // options2msg - convert option to message
  def options2msg(option: Int, noPlayer: Int): String = {
    import shared.model.Group
    option match {
      case CPC_GRPS3  => { val size1 = noPlayer / 3; getMsg(s"option.info.${option}", size1.toString) }   
      case CPC_GRPS34 => { val (size1, size2) = Group.genGrpSplit(noPlayer, 3); getMsg(s"option.info.${option}", size1.toString, size2.toString) }
      case CPC_GRPS4  => { val size1 = noPlayer / 4; getMsg(s"option.info.${option}", size1.toString) } 
      case CPC_GRPS45 => { val (size1, size2) = Group.genGrpSplit(noPlayer, 4); getMsg(s"option.info.${option}", size1.toString, size2.toString) } 
      case CPC_GRPS5  => { val size1 = noPlayer / 5; getMsg(s"option.info.${option}", size1.toString) }
      case CPC_GRPS56 => { val (size1, size2) = Group.genGrpSplit(noPlayer, 5); getMsg(s"option.info.${option}", size1.toString, size2.toString) }   
      case CPC_GRPS6  => { val size1 = noPlayer / 6; getMsg(s"option.info.${option}", size1.toString) }
      case CPC_KO     => { getMsg(s"option.info.${option}", genKOSize(noPlayer).toString) }
      case CPC_SW     => { val size1 = noPlayer+(noPlayer%2); getMsg(s"option.info.${option}", size1.toString) }
      case CPC_JGJ    => { getMsg(s"option.info.${option}", noPlayer.toString) }
      case _          => { getMsg(s"option.info.2") }
    }
  }

  // genCfgName - generate configuration name proposal
  def genCfgName(option: Int, coPhId: Int, winner: Boolean=true): String = {
    option match {
      case CPC_GRPS3 | CPC_GRPS34 | CPC_GRPS4 | CPC_GRPS45 | CPC_GRPS5 | CPC_GRPS56 | CPC_GRPS6  => {
        if      (coPhId == 0)  { getMsg(s"name.1") } 
        else if (winner)        { getMsg(s"name.2") } 
        else                    { getMsg(s"name.4") }
      }
      case CPC_KO | CPC_SW | CPC_JGJ => if (winner) getMsg(s"name.3") else getMsg(s"name.4") 
      case CPC_UDEF                  => ""
      case _                         => getMsg(s"name.3") 
    }
  }  

}