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
import shared.model.{ Tourney, Player, PantSelect, SNO}
import shared.utils._
import shared.utils.Constants._
import clientviews.dialog.DlgCardCfgSection.html


@JSExportTopLevel("DlgCardCfgSection")
object DlgCardCfgSection extends BasicHtml 
  with TourneySvc with DrawSvc
{
  this: BasicHtml =>
  implicit val ucp     = UseCaseParam("APP__DlgCardCfgSection", "dlg.card.cfg.section", "DlgCardCfgSection", "dlgcardcfgsection", scalajs.AppEnv.getMessage _ )
  private def load()   = if (!checkId("Modal")) insertHtml_("APP__Load", "afterbegin", html.Main().toString)
  
  var pants          = new ArrayBuffer[PantSelect]()
  var coId:     Long = 0L
  var size:     Int  = 0
  var pSectId:  Int  = 0

  // show dialog return result (Future) or Failure when canceled
  def show(coIdP: Long, pSectIdP: Int, pantsP: ArrayBuffer[PantSelect])(implicit trny: Tourney): Future[Either[Error, Int]] = {
    val p     = Promise[Int]()
    val f     = p.future

    def cancel() = {
      $(getId("Modal","#")).off("hide.bs.modal")
      if (!p.isCompleted) { p failure (new Exception("dlg.canceled")) }
    }

    def submit(e: Event): Unit = {
      validate() match {
        case Left(eList) => DlgShowError.show(eList)
        case Right(result)   => {
          if (!p.isCompleted) p success result
          //disable modal first, then hide
          $(getId("Modal","#")).off("hide.bs.modal")
          $(getId("Modal","#")).modal("hide")
        }  
      }
    }
    
    load()
    coId     = coIdP
    pSectId  = pSectIdP
    pants    = pantsP
    size     = pants.filter(_.checked).size
    setView()

    // register routines for cancel and submit
    $(getId("Modal","#")).on("hide.bs.modal", () => cancel())
    $(getId("Submit","#")).click( (e: Event)     => submit(e)) 
    $(getId("Modal","#")).modal("show")

    f.map(Right(_))
     .recover { case e: Exception =>  Left(Error(e.getMessage)) }
  }


  @JSExport
  def actionEvent(key: String, elem: HTMLElement, event: Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {     
      case "Selection" => { 
        val cst = getInput("CfgSelection", CST_UDEF)  // competition section type
        setDisabled("Submit", cst == CST_UDEF)
        setHtml("CfgInfo", options2msg(cst, size)) 
        setInput("CfgName", genCfgName(cst, pSectId, (pSectId==0) || getRadioBtn("WinLoo", false)))
      }

      case "Winner" | "Looser" => {
        val cst = getInput("CfgSelection", CST_UDEF)
        setInput("CfgName", genCfgName(cst, pSectId, (pSectId==0) || getRadioBtn("WinLoo", false)))
      }
      
      case "Check" => elem.asInstanceOf[Input].value.toIntOption match {
        case None        => error("actionEvent", s"key: 'Check' - invalid index ")
        case Some(index) => {
          pants(index).checked = elem.asInstanceOf[Input].checked
          size = pants.filter(_.checked).size
          setViewInfoOption(size)
          debug("actionEvent", s"value: size: ${size} ${index}  ${pants(index).checked} ")
        }
      }

      case "Close"        => { $(getId("Modal","#")).off("hide.bs.modal");  $(getId("Modal","#")).modal("hide") }

      case _              => {}
    }
  }


  /** validate input configuration input return selected option or a List of Errors
   * 
   */ 
  def validate(): Either[List[Error], Int] = {
    Right(7)
  }

  // set possible configuration section 
  def setView()(implicit trny: Tourney): Unit = {

    // for now switch off looser round
    // setVisible("BtnRadioWinLoo", pSectId != 0)
    setVisible("BtnRadioWinLoo", false)
    setViewInfoOption(size)
    
    setHtml("PantTbl", html.Pants(pants.toArray))
  }
  
  // setViewInfoOption
  def setViewInfoOption(size: Int): Unit = {
    val cfgOptions = sysOptions(size)
    val selOptions = new StringBuilder(s"<option value='${CST_UDEF}' selected>---</option>")
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
      if (size % 3 == 0) result += CST_GRPS3 else result += CST_GRPS34
      if (size % 4 == 0) result += CST_GRPS4 else result += CST_GRPS45
      if (size % 5 == 0) result += CST_GRPS5 else result += CST_GRPS56
      result += CST_KO
      result += CST_SW
      result.to(List)
    }

    size match {
      case 3 | 4 | 5 => List(CST_KO, CST_SW, CST_JGJ)
      case 6         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS3)
      case 7         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS34)
      case 8         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS4)
      case 9         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS45)
      case 10        => List(CST_KO, CST_SW, CST_GRPS34, CST_GRPS5)
      case 11        => List(CST_KO, CST_SW, CST_GRPS34, CST_GRPS56)
      case 12        => List(CST_KO, CST_SW, CST_GRPS4, CST_GRPS6)
      case 13        => List(CST_KO, CST_SW, CST_GRPS45)
      case 14        => List(CST_KO, CST_SW, CST_GRPS45)
      case 15        => List(CST_KO, CST_SW, CST_GRPS3, CST_GRPS5)
      case 16        => List(CST_KO, CST_SW, CST_GRPS4, CST_GRPS56)
      case 17        => List(CST_KO, CST_SW, CST_GRPS45, CST_GRPS56)
      case 18        => List(CST_KO, CST_SW, CST_GRPS3, CST_GRPS45, CST_GRPS6)
      case 19        => List(CST_KO, CST_SW, CST_GRPS34, CST_GRPS45)
      case 20        => List(CST_KO, CST_SW, CST_GRPS4, CST_GRPS5)

      case i if (i > 21 && i <= 128) => sysOptions21to128(i)
      case _                         => List()        
    }
  }

  // options2msg - convert option to message
  def options2msg(option: Int, noPlayer: Int): String = {
    option match {
      case CST_GRPS3  => { val size1 = noPlayer / 3; getMsg(s"option.info.${option}", size1.toString) }   
      case CST_GRPS34 => { val (size1, size2) = genGrpSplit(noPlayer, 3); getMsg(s"option.info.${option}", size1.toString, size2.toString) }
      case CST_GRPS4  => { val size1 = noPlayer / 4; getMsg(s"option.info.${option}", size1.toString) } 
      case CST_GRPS45 => { val (size1, size2) = genGrpSplit(noPlayer, 4); getMsg(s"option.info.${option}", size1.toString, size2.toString) } 
      case CST_GRPS5  => { val size1 = noPlayer / 5; getMsg(s"option.info.${option}", size1.toString) }
      case CST_GRPS56 => { val (size1, size2) = genGrpSplit(noPlayer, 5); getMsg(s"option.info.${option}", size1.toString, size2.toString) }   
      case CST_GRPS6  => { val size1 = noPlayer / 6; getMsg(s"option.info.${option}", size1.toString) }
      case CST_KO     => { getMsg(s"option.info.${option}", genKOSize(noPlayer).toString) }
      case CST_SW     => { val size1 = noPlayer+(noPlayer%2); getMsg(s"option.info.${option}", size1.toString) }
      case CST_JGJ    => { getMsg(s"option.info.${option}", noPlayer.toString) }
      case _          => { getMsg(s"option.info.2") }
    }
  }

  // genCfgName - generate configuration name proposal
  def genCfgName(option: Int, pSectId: Int, winner: Boolean=true): String = {
    option match {
      case CST_GRPS3 | CST_GRPS34 | CST_GRPS4 | CST_GRPS45 | CST_GRPS5 | CST_GRPS56 | CST_GRPS6  => {
        if      (pSectId == 0) { getMsg(s"name.1") } 
        else if (winner)        { getMsg(s"name.2") } 
        else                    { getMsg(s"name.4") }
      }
      case CST_KO | CST_SW | CST_JGJ => if (winner) getMsg(s"name.3") else getMsg(s"name.4") 
      case CST_UDEF                  => ""
      case _                         => getMsg(s"name.3") 
    }
  }  

}