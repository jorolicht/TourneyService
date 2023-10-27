package scalajs.usecase.component

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"

// for scalajs dom routines
import org.scalajs.dom.document
import org.scalajs.dom.html.Input
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLElement
import org.scalajs.dom.raw.NodeListOf
import org.scalajs.dom.raw.NodeList
import org.scalajs.dom.raw.DOMList
import org.scalajs.dom.raw.Node
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext._


// tourney service imports
import shared.model.{ TournBase, Tourney, Competition }
import shared.utils.UseCaseParam
import shared.utils.Error

import scalajs.service._
import scalajs.{ App, AppEnv }

class BasicHtml
{
  implicit class NodeListSeq[T <: Node](nodes: DOMList[T]) extends IndexedSeq[T] {
    override def foreach[U](f: T => U): Unit = {
      for (i <- 0 until nodes.length) {
        f(nodes(i))
      }
    }
    override def length: Int = nodes.length
    override def apply(idx: Int): T = nodes(idx)
  }

  def debug(func: => String, msg: =>String)(implicit ucp: UseCaseParam)  = AppEnv.logger.debug(s"${ucp.idBase}.${func}-> ${msg}")
  def info(func:  => String, msg: =>String)(implicit ucp: UseCaseParam)  = AppEnv.logger.info(s"${ucp.idBase}.${func}-> ${msg}")
  def warn(func:  => String, msg: =>String)(implicit ucp: UseCaseParam)  = AppEnv.logger.warn(s"${ucp.idBase}.${func}-> ${msg}")
  def error(func: => String, msg: =>String)(implicit ucp: UseCaseParam)  = AppEnv.logger.error(s"${ucp.idBase}.${func}-> ${msg}")
  def errLog(text: String): Unit = {
    import org.scalajs.dom
    dom.console.log(s"%c ERROR: ${text}", "color: #B22222")
  }  


  /* ScalaJs: getElementById does not know what kind of element nodeValue is. 
   * It does not have access to the .html to figure that out, and hence it returns a very 
   * generic Element. But in fact, you know that it is an input element, i.e., 
   * an org.scalajs.dom.Input. You can tell that to the compiler with .asInstanceOf[Input]. 
   * A generic Element does not have a value property, only Inputs have.
   * -> asInstanceOf[dom.Input]
   */


  // getId - adds usecase prefix to id  
  def getIdHa(id: String)(implicit ucp: UseCaseParam) = { s"#${ucp.idBase}__${id}"}

  def onEvents(elem: HTMLElement, events: String, handler:()=>Unit) = $(elem).on(events, handler)
  def offEvents(elem: HTMLElement, events: String) = $(elem).off(events)
  def onClick(elem: HTMLElement, handler:dom.raw.Event=>Unit) = $(elem).click(handler)
  def onClick2(selector: String, handler:dom.raw.Event=>Unit)  = $(selector).click(handler)
  def doModal(elem: HTMLElement, cmd: String) = $(elem).modal(cmd)
  def doLoad[C](elem: HTMLElement, text: C)  = if (!checkId(elem)) text match {
    case _:String => insertHtml(gE("APP__Load"), "afterbegin", text.asInstanceOf[String])
    case _        => insertHtml(gE("APP__Load"), "afterbegin", text.toString) 
  }

  def checkId(elem: HTMLElement): Boolean = try (elem.textContent.length > 0) catch { case _: Throwable => false }

  def checkId[Context](id: String, ucp: Context): Boolean = { 
    try ucp match {
      case _:UseCaseParam => (document.getElementById(uc(id)(ucp.asInstanceOf[UseCaseParam])).textContent.length > 0)
      case _:String       => (document.getElementById(ucp.asInstanceOf[String] + id).textContent.length > 0)
    }  
    catch { case _: Throwable => false }
  }

  def loadModal[C](text: C, ucp: UseCaseParam) = if (!checkId("Modal", ucp)) text match {
    case _:String => insertHtml(gE("APP__Load"), "afterbegin", text.asInstanceOf[String])
    case _        => insertHtml(gE("APP__Load"), "afterbegin", text.toString) 
  }
  def loadModal[C](text: C, id: String="")  = if (!checkId(id,"")) text match {
    case _:String => insertHtml(gE("APP__Load"), "afterbegin", text.asInstanceOf[String])
    case _        => insertHtml(gE("APP__Load"), "afterbegin", text.toString) 
  }


  def ite[T](cond: Boolean, valA: T, valB:T): T = if (cond) valA else valB
  
  /** setHtml
    *
    * @param elemId  - id of html element
    * @param content - html content of element
    */ 

  def setHtml[I,C](idElt: I, content: C = "")(implicit ucp: UseCaseParam=UseCaseParam("","","","", (x:String,y:Seq[String])=>"")): Unit = {
    val value = content match {
        case _:String => content.asInstanceOf[String]
        case _        => content.toString
    }     
    try idElt match {
      case _:String => gE(uc(idElt.asInstanceOf[String])).innerHTML = value 
      case _        => idElt.asInstanceOf[HTMLElement].innerHTML = value
    } catch { case _: Throwable => error("setHtml", s"idElt: ${idElt} usecase: ${ucp.idBase} content: ${value.take(10)}") }
  }

  def setText(id: String, content: String)(implicit ucp: UseCaseParam): Unit = 
    doTry(s"setText ${id} ${content.take(10)}") { gE(uc(id)).innerText = content }
  
  def insertHtml(elem: HTMLElement, pos: String, content: String): Unit = {
    try elem.insertAdjacentHTML(pos, content)
    catch { case _: Throwable => AppEnv.error("insertHtml", s"elem: ${elem} pos: ${pos} content: ${content.take(10)}") } 
  }
  

  def setHtmlVisible(id: String, visible: Boolean, content: String="")(implicit ucp: UseCaseParam): Unit = {
    try {
      val elem = gE(uc(id))
      elem.style.setProperty("display", disProp(visible))
      if (visible) elem.innerHTML = content
    } catch { case _: Throwable => error(s"setHtmlVisible", s"id: ${uc(id)} visible: ${visible} content: ${content.take(10)}") } 
  }

  def setDisabled(id: String, value: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(uc(id)).asInstanceOf[Input].disabled = value
    catch { case _: Throwable => error(s"setDisabled", s"id: ${uc(id)} value: ${value}") } 
  }

  def setDisabled(elem: HTMLElement, value: Boolean): Unit = {
    try elem.asInstanceOf[Input].disabled = value
    catch { case _: Throwable => AppEnv.logger.error(s"setDisabled__ -> value: ${value}") } 
  }

  def setDisabledByName(name: String, value: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try {
      val container = document.getElementsByName(uc(name))
      container.map(_.asInstanceOf[Input].disabled = value)      
    } catch { case _: Throwable => error(s"setDisabledByName", s"name: ${uc(name)} value: ${value}") } 
  }


  def showModal(id: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(uc(id)).asInstanceOf[js.Dynamic].showModal()
    catch { case _: Throwable => error(s"showModal", s"id: ${uc(id)}") } 
  }


  /**
    * set visibility of all Html elements with id
    *
    * @param elemId - id of html element
    * @param visible - true or false
    */ 
  def setVisible(id: String, visible: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(uc(id)).asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible))
    catch { case _: Throwable => error(s"setVisible", s"id: ${uc(id)} visible: ${visible}") }
  }  

  def setVisible(elem: HTMLElement, visible: Boolean): Unit = {
    try elem.style.setProperty("display", disProp(visible))
    catch { case _: Throwable => AppEnv.error(s"setVisible", s"elem: ${elem} visible: ${visible}") }
  }  

  def togVisible(elem: HTMLElement): Unit = {
    try if (elem.style.display == "none") elem.style.display="block" else elem.style.display="none"
    catch { case _: Throwable => AppEnv.error("togVisible_", s"elem: ${elem}") }
  }



  /**
    * set visibility of all Html elements with data attribute has special value
    *
    * @param name - selector for data attributes <dataAttrPref>-<name>
    * @param attr - attribute value of data attribute
    * @param visible - true or false
    */ 
  def setVisibleByAttr(name: String, attr: String, visible: Boolean)(implicit ucp: UseCaseParam): Unit = {
    document.querySelectorAll(s"[data-${ucp.dataAttr}-${name}='${attr}']").map(_.asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible)))
  } 

  def setVisibleByAttr_(name: String, attr: String, visible: Boolean): Unit = {
    try document.querySelectorAll(s"[data-${name}='${attr}']").map(_.asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible)))
    catch { case _: Throwable => { AppEnv.error("setVisibleByAttr_", s"name: ${name} / ${attr}") }}
  } 

  def setVisibleByName(name: String, visible: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.querySelectorAll(s"[data-${ucp.dataAttr}-${name}]").map(_.asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible)))
    catch { case _: Throwable => AppEnv.error("setVisibleByName", s"name: ${name} visible: ${visible}") }
  }

  def setVisibleDataAttr(dataAttr: String, visible: Boolean): Unit = {
    val dProperty = if (visible) "" else "none"
    try  document.querySelectorAll(s"[data-${dataAttr}]").map(_.asInstanceOf[HTMLElement].style.setProperty("display", dProperty))
    catch { case _: Throwable => AppEnv.error("setVisibleDataAttr", s"dataAttr: ${dataAttr} visible: ${visible}") }
  }

  def showHelp(key: String, visible: Boolean = true)(implicit ucp: UseCaseParam) = {
    val id = s"${ucp.idBase}__${key}Hlp"
    try {
      document.getElementById(id).asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible))
      if (visible) document.getElementById(id).asInstanceOf[HTMLElement].innerHTML = AppEnv.getMessage(ucp.msgPrefix + ".hlp." + key)
    } catch { case _: Throwable => AppEnv.error("showHelp", s"id: ${id} visible: ${visible}") }
  }

  def showHelpMsg(key: String, msg: String, visible: Boolean = true)(implicit ucp: UseCaseParam) = {
    val id = s"${ucp.idBase}__${key}Hlp"
    try {
      val elem = document.getElementById(id).asInstanceOf[HTMLElement]
      elem.style.setProperty("display", disProp(visible))
      if (visible) elem.innerHTML = AppEnv.getMessage(ucp.msgPrefix + ".hlp." + msg)
    } catch { case _: Throwable => AppEnv.error("showHelpMsg", s"id: ${id} msg: ${msg} visible: ${visible}") }
  }

  def showHelpTxt(key: String, txt: String, visible: Boolean = true)(implicit ucp: UseCaseParam) = {
    val id = s"${ucp.idBase}__${key}Hlp"
    try {
      val elem = document.getElementById(id).asInstanceOf[HTMLElement]
      elem.style.setProperty("display", disProp(visible))
      if (visible) elem.innerHTML = txt
    } catch { case _: Throwable => AppEnv.error("showHelpTxt", s"id: ${id} txt: ${txt} visible: ${visible}") }
  }

  def markInput(elem: HTMLElement, markError: Option[Boolean])= {
    try {
      elem.classList.remove("border-success")
      elem.classList.remove("border-danger") 
      markError match {
        case Some(value) => { 
          if (value) { elem.classList.add("border-danger"); elem.classList.remove("border-success") } 
          else       { elem.classList.add("border-success"); elem.classList.remove("border-sucess") } 
        }
        case None        =>  { elem.classList.remove("border-success"); elem.classList.remove("border-danger") }
      }
    } catch { case _: Throwable => AppEnv.error("markInput", elem.toString) }
  }


  /**
    * getNow return date of today 
    *
    * @return
    */
  def getNow(): Int = {
    val date = new js.Date()
    val y = date.getFullYear.toInt
    val m = date.getMonth.toInt
    val d = date.getDate.toInt
    y*10000 + (m+1)*100 + d
  }


  /**
    * getData read data attribute from Html element 
    *
    * @param elem
    * @param name
    * @param default
    * @return
    */

  def getData[A](elem: HTMLElement, name: String, default: A): A = {
    try {
      default match {
        case _:Int    => elem.getAttribute(s"data-${name}").toIntOption.getOrElse(default.asInstanceOf[Int]).asInstanceOf[A]
        case _:Long   => elem.getAttribute(s"data-${name}").toLongOption.getOrElse(default.asInstanceOf[Long]).asInstanceOf[A]
        case _:String => elem.getAttribute(s"data-${name}").asInstanceOf[A]
        case _        => { AppEnv.error("getData", s"elem: ${elem} default: ${default}"); default }
      }
    }  catch { case _: Throwable => AppEnv.error("getData", s"elem: ${elem} name: ${name} default: ${default}"); default }
  }

  def setData[A](elem: HTMLElement, attr: String, value: A) = {
    try elem.setAttribute(s"data-${attr}", value.toString)
    catch { case _: Throwable => AppEnv.error("setData", s"attribute: ${attr} value: ${value}") }
  }

  /**
    * getInput read input value from Html input element 
    *
    * @param name
    * @param defVal
    * @return
    */

  def getInput[R](elem: HTMLElement, defVal: R = ""): R = 
    try defVal match {
        case _:Int    => elem.asInstanceOf[Input].value.toIntOption.getOrElse(defVal).asInstanceOf[R]
        case _:Long   => elem.asInstanceOf[Input].value.toLongOption.getOrElse(defVal).asInstanceOf[R]
        case _:String => elem.asInstanceOf[Input].value.asInstanceOf[R]
        case _        => AppEnv.logger.error(s"getInput -> elem: ${elem} defVal: ${defVal}"); defVal
    } catch { case _: Throwable => AppEnv.logger.error(s"getInput elem: ${elem} defVal: ${defVal}"); defVal }

  def getBooleanOption(name: String)(implicit ucp: UseCaseParam): Option[Boolean] = {
    try document.getElementById(uc(name)).asInstanceOf[Input].value.toBooleanOption
    catch { case _: Throwable => AppEnv.error("getBooleanOption", s"${name}"); None }
  }

  def getIntOption(name: String)(implicit ucp: UseCaseParam): Option[Int] = {
    try document.getElementById(uc(name)).asInstanceOf[Input].value.toIntOption
    catch { case _: Throwable => AppEnv.error("getIntOption", s"${name}"); None }
  }

  def setBooleanOption(name: String, input: Option[Boolean])(implicit ucp: UseCaseParam): Unit = {
    try input match {
      case None        => document.getElementById(uc(name)).asInstanceOf[Input].value = "None"
      case Some(value) => document.getElementById(uc(name)).asInstanceOf[Input].value = value.toString
    }
    catch { case _: Throwable => error(s"setBooleanOption", s"name: ${uc(name)}") }
  }

  def setIntOption(name: String, input: Option[Int])(implicit ucp: UseCaseParam): Unit = {
    try input match {
      case None        => document.getElementById(uc(name)).asInstanceOf[Input].value = "None"
      case Some(value) => document.getElementById(uc(name)).asInstanceOf[Input].value = value.toString
    }
    catch { case _: Throwable => error(s"setIntOption", s"name: ${uc(name)}") }
  }  


  def resetInput(name: String)(implicit ucp: UseCaseParam): Unit = {
    try {
      val divNode = gE(uc(name))
      val inNodes = divNode.getElementsByTagName("INPUT")
      inNodes.foreach { node => {
        node.classList.remove("border-danger")
        node.classList.remove("border-success") 
      }}
      
      val selNodes = divNode.getElementsByTagName("SELECT")
      selNodes.foreach { node => {
        node.classList.remove("border-danger")
        node.classList.remove("border-success") 
      }}
    }
    catch { case _: Throwable =>  AppEnv.error("resetInput", s"${name}") }
  }


  /** setDateTimePicker - initialize date/time input
    * 
    * @param ident
    * @param lang
    * @param year
    * @param month
    * @param day
    * @param hour
    * @param minute
    * @param ucp
    */
  def setDateTimePicker(ident: String, lang: String, ymd: (Int, Int, Int), hm:(Int,Int) = (-1,-1))
                        (implicit ucp: UseCaseParam) = {
    val withTime = hm._1!=(-1)
    val sDateTime =  if (withTime) new js.Date(ymd._1, ymd._2-1, ymd._3, hm._1, hm._2) else new js.Date(ymd._1, ymd._2-1, ymd._3)

    //debug("setDateTimePicker", s"${ymd._1}-${ymd._2}-${ymd._3} sDateTime: ${sDateTime}")
    trait DateTimeParam extends js.Object {
      val format: String
      val locale: String
      val date: js.Object
    }
    val tParam = new DateTimeParam {
      val format = if (withTime) "YYYY-MM-DD HH:mm" else "YYYY-MM-DD"
      val locale = lang
      val date = sDateTime
    }
    val curDateTime = if (withTime) f"${ymd._1}-${ymd._2}%02d-${ymd._3}%02d ${hm._1}%02d:${hm._2}%02d" else f"${ymd._1}-${ymd._2}%02d-${ymd._3}%02d"
    
    //debug(s"setDateTimePicker", s"id: ${getId(ident, "#")}")

    $(s"#${ucp.idBase}__${ident}").datetimepicker(tParam)
    setInput(ident, curDateTime)
  }


  /** setInput sets value of Html input element
    * 
    * @param id attribut of element
    * @param text value of element
    * @param ucp usecase param
    */
  def setInput(id: String, text: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(uc(id)).asInstanceOf[Input].value = text
    catch { case _: Throwable => error(s"setInput", s"id: ${uc(id)}") }
  } 
  def setInput(elem: HTMLElement, text: String): Unit = {
    try elem.asInstanceOf[Input].value = text
    catch { case _: Throwable => AppEnv.error(s"setInput", s"elem: ${elem}") }
  } 


  def getTextContent(id: String, defVal: String="")(implicit ucp: UseCaseParam): String = {
    try document.getElementById(uc(id)).textContent
    catch { case _: Throwable => error("getTextContent", s"id: ${uc(id)}"); defVal }
  }


  /* getElementById does not know what kind of element nodeValue is. 
   * It does not have access to the .html to figure that out, and hence it returns a very 
   * generic Element. But in fact, you know that it is an input element, i.e., 
   * an org.scalajs.dom.Input. You can tell that to the compiler with .asInstanceOf[Input]. 
   * A generic Element does not have a value property, only Inputs have.
   * -> asInstanceOf[dom.Input]
   */

  /** get/set/removeAttribute
   * 
   * @param id of html element
   * @param attrName value of attribute
   * @param defVal if no value is present
   * @param attrValue value of attribute
   */
  def getAttribute(elem: HTMLElement, attrName: String, defVal: String=""): String = 
    try elem.getAttribute(attrName)
    catch { case _: Throwable => { AppEnv.error("getAttribute", s"elem: ${elem} attrName: ${attrName}"); defVal }}

  def setAttribute(elem: HTMLElement, attrName: String, attrValue: String): Unit = 
    try elem.setAttribute(attrName, attrValue)
    catch { case _: Throwable => AppEnv.error("setAttribute", s"elem:${elem} ${attrName }-> ${attrValue}") } 

  def removeAttribute(elem: HTMLElement, attrName: String): Unit = 
    try elem.removeAttribute(attrName)
    catch { case _: Throwable => AppEnv.error("removeAttribute", s"elem: ${elem} attrName: ${attrName}") } 

  def setPlaceholder(elem: HTMLElement, value: String) =
    try elem.asInstanceOf[Input].placeholder = value
    catch { case _: Throwable => AppEnv.error("setPlaceholder", s"elem: ${elem}") } 


  def selectOption(id: String, selValue: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(uc(id)).asInstanceOf[Input].value = selValue
    catch { case _: Throwable => error("selectOption", s"id: ${uc(id)} option: ${selValue}") } 
  }

  def addClass(elem: HTMLElement, _class: String*): Unit = 
    try _class.foreach(cValue => elem.classList.add(cValue))
    catch { case _: Throwable => AppEnv.error(s"addClass", s"elem: ${elem}  class: ${_class}") } 


  def removeClass(elem: HTMLElement, _class: String*): Unit = 
    try _class.foreach(cValue => elem.classList.remove(cValue))
    catch { case _: Throwable => AppEnv.error(s"removeClass", s"elem: ${elem}  class: ${_class}") } 


  def setClass(id: String, value: Boolean, _class: String*)(implicit ucp: UseCaseParam): Unit = 
    try if (value) { _class.foreach(cValue => gE(uc(id)).classList.add(cValue)) } 
        else       { _class.foreach(cValue => gE(uc(id)).classList.remove(cValue)) }   
    catch { case _: Throwable => error("setClass", s"id: ${uc(id)}  class: ${_class}") } 


  def setRadioBtn(id: String, value: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(uc(id)).asInstanceOf[Input].checked = value
    catch { case _: Throwable => error("setRadioBtn", s"id: ${uc(id)} -> ${value}") } 
  }

  def setRadioBtnByValue(name: String, value: String)(implicit ucp: UseCaseParam): Unit = {
    val inpNodes = document.getElementsByName(uc(name)).asInstanceOf[NodeList]
    val node = inpNodes.filter(_.asInstanceOf[Input].value == value).head
    node.asInstanceOf[Input].checked= true
  }

  def getRadioBtn[A](name: String, defVal: A)(implicit ucp: UseCaseParam): A = {
    try {
      val inpNodes = document.getElementsByName(uc(name)).asInstanceOf[NodeList]
      val node = inpNodes.filter(_.asInstanceOf[Input].checked).head
      defVal match {
        case _:Int    => node.asInstanceOf[Input].value.toIntOption.getOrElse(defVal).asInstanceOf[A]
        case _:Long   => node.asInstanceOf[Input].value.toLongOption.getOrElse(defVal).asInstanceOf[A]
        case _:String => node.asInstanceOf[Input].value.asInstanceOf[A]
        case _        => { error("getRadioBtn", s"name: ${uc(name)} invalid default value"); defVal }
      }   
    } catch { case _: Throwable => error("getRadioBtn", s"name: ${uc(name)}"); defVal }
  }


  def setCheckbox[A](idElt: A, value: Boolean)(implicit ucp: UseCaseParam): Unit = 
    try idElt match {
      case _:String => document.getElementById(uc(idElt.asInstanceOf[String])).asInstanceOf[Input].checked = value
      case _        => idElt.asInstanceOf[Input].checked = value
    } catch { case _: Throwable => error("setCheckbox", s"idElt: ${idElt} -> ${value}") } 
  
  def getCheckbox(elem: HTMLElement): Boolean = 
    try elem.asInstanceOf[Input].checked
    catch { case _: Throwable => AppEnv.error("getCheckbox", s"elem: ${elem}"); false } 
  
  /** togCollapse ads/removes class collapse
    * 
    * @param id attribute of element
    */
  def togCollapse(id1: String, id2: String="")(implicit ucp: UseCaseParam): Unit = 
    if (id2=="") togCollapse_(uc(id1)) else togCollapse_(uc(id1), uc(id2) )  
  
  def togCollapse_(id1: String, id2: String=""): Unit = {
    try {
      val elem1 = document.getElementById(id1)
      if (elem1.classList.contains("collapse")) {
        elem1.classList.remove("collapse")
        if (id2 != "") document.getElementById(id2).classList.remove("collapse")
      } else {
        elem1.classList.add("collapse")
        if (id2 != "") document.getElementById(id2).classList.add("collapse")
      }
    } catch { case _: Throwable => AppEnv.error("togCollapse", s"id1: ${id1} id2: ${id2}") }
  }

  def collapse(name: String, hide: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try {
      val elem = gE(uc(name)) 
      if (hide) {
        if (!elem.classList.contains("collapse")) elem.classList.add("collapse") 
      } else {
        if (elem.classList.contains("collapse")) elem.classList.remove("collapse")
      } 
    } catch { case _: Throwable => AppEnv.error("collapse", s"name: ${uc(name)} value: ${hide}") }
  }

  // setTableRow
  def selTableRow(id: String, classText: String="text-white", classBg: String="bg-secondary") = {
    val elems2rem = document.getElementById(id).parentNode.asInstanceOf[HTMLElement].getElementsByTagName("tr").asInstanceOf[NodeListOf[HTMLElement]]
    val selElem   = document.getElementById(id)
    elems2rem.map { elem => removeClass(elem.asInstanceOf[HTMLElement], classText, classBg) }  
    selElem.classList.add(classText)
    selElem.classList.add(classBg)
  } 


  /** getMsg/getError
    *
    * @param key  - message key
    * @param args - inserts into message
    */
  def getMsg(key: String, args: String*)(implicit ucp: UseCaseParam): String = {
    if (key.startsWith("_")) AppEnv.getMessage(key.substring(1), args: _*) else AppEnv.getMessage(ucp.msgPrefix + "." + key, args: _*)
  }
  
  def gM(key: String, args: String*): String = AppEnv.getMessage(key, args: _*)

  def mP(key: String)(implicit ucp: UseCaseParam): String = {
    if(key.startsWith("_")) key.substring(1) else s"${ucp.msgPrefix}.${key}"
  }



  def getTypName[T](value: T): String = value match {
    case _:shared.model.CompTyp.Value => gM(s"CompTyp.${value.asInstanceOf[shared.model.CompTyp.Value].id}")
    case _                            => "XXX"
  }

  def setMainContent[C](content: C): Unit = content match {
    case _:play.twirl.api.Html => document.getElementById("mainContent").asInstanceOf[HTMLElement].innerHTML = content.toString
    case _:String              => document.getElementById("mainContent").asInstanceOf[HTMLElement].innerHTML = content.asInstanceOf[String]
  }
      
  def disProp(visible: Boolean): String = if (visible) "block" else "none"

  def startSpinner() = addClass(gE("APP__Spinner"), "spinner-grow")
  def stopSpinner()  = removeClass(gE("APP__Spinner"), "spinner-grow") 

    // showAlert  
  def showAlert(text: String): String = {
    s"""
      |<div class="alert alert-info" role="alert">
      |  <span class="tuse-font-1">${text}</span>
      |</div>
    """.stripMargin('|')
  }

  def doTry(msg: String)(op: => Unit) = {
    try op catch { case _: Throwable => AppEnv.error("doTry", msg) }
  } 

  def gE(id: String): HTMLElement = 
    try document.getElementById(id).asInstanceOf[HTMLElement]
    catch { case _: Throwable => AppEnv.error("gE", s"id: ${id}"); null } 

  def gEqS(id: String, qS: String): HTMLElement = 
    try document.getElementById(id).querySelector(qS).asInstanceOf[HTMLElement]
    catch { case _: Throwable => AppEnv.error("gEqS", s"id: ${id} querySelector: ${qS}"); null } 

  def gEqSA(id: String, qS: String) = 
    try document.getElementById(id).querySelectorAll(qS)
    catch { case _: Throwable => AppEnv.error("gEqS", s"id: ${id} querySelector: ${qS}"); null } 

  def uc(id: String)(implicit ucp: UseCaseParam) = { if (ucp.idBase == "") id else s"${ucp.idBase}__${id}"}

  def getError(key: String, args: String*): String = AppEnv.getMessage(key, args: _*)
  def getError(err: shared.utils.Error): String    = AppEnv.getMessage(err.msgCode, err.in1, err.in2)
  def getErrStack(err: shared.utils.Error): String = s"CallStack: ${err.callStack} -> ${AppEnv.getMessage(err.msgCode, err.in1, err.in2)}"

  def showResult(visi: Boolean, header: String="", body: String="", alertTyp: String="success") = {
    if (visi) {
      addClass(gE("APP__Result"), "alert", s"alert-${alertTyp}", "alert-dismissible", "fade", "show")
      setHtml(gE("APP__Result__Head"), header)
      setHtml(gE("APP__Result__Body"), body)
    } 
    setVisible(gE("APP__Result"), visi)
  }  

  /** DIALOGS
   *  
   */ 
  def dlgCancelOk(hdrMsg: String, confirmMsg: String)(fun: => Unit): Future[Boolean] = {
    import scalajs.usecase.dialog.DlgBox
    DlgBox.confirm(hdrMsg, confirmMsg).map { 
      case true => fun; true
      case _    => false
    } 
  }  


  /** setHeader
   *  
   */ 
  def setHeader(): Unit = {
    val login = AppEnv.getOrgId > 0
    //debug("setHeader", s"runModeLocal: ${AppEnv.isRunModeLocal} login: ${login}")
    if (AppEnv.getOrganizer != "") { 
      setHtml(gE("APP__Title"), AppEnv.getMessage("app.header.title.club", AppEnv.getOrganizer))
    } else {
      println(s"setHeader APP_Title")
      setHtml(gE("APP__Title"), AppEnv.getMessage("app.header.title",""))
    }  
    
    setVisible(gE("APP__Download"), !AppEnv.isRunModeLocal)
    setVisible(gE("APP__Register"), !AppEnv.isRunModeLocal & !login)

    setVisible(gE("APP__Logout"), login)
    setVisible(gE("APP__Configuration"), login)
    setVisible(gE("APP__User"), login)
    setVisible(gE("APP__Club"), login)
    setVisible(gE("APP__Login"), !login)
    

    setHtml(gE("APP__UserName"), if (AppEnv.getFullName != "") AppEnv.getFullName else AppEnv.getEmail)
    setHtml(gE("APP__ClubName"), AppEnv.getOrganizer)
    setVisible(gE("APP__UserName"), login)
    setVisible(gE("APP__ClubName"), login)
    
    val HContent = s"<strong>${App.tourney.name}</strong> ${App.tourney.getCompName(0,1)}"
    //println(s"APP__Headline__Content: ${HContent}")
    setHtml(gE("APP__Headline__Content"), HContent)
  }

  // setFooter
  def setFooter(author: String="", changeDate: String=""): Unit = {
    if (author == "") {
      setVisible(gE("APP__Footer_Content_MetaData"), false)
    } else {
      setVisible(gE("APP__Footer_Content_MetaData"), true)
      setHtml(gE("APP__Footer_Content_Author"), AppEnv.getMessage("app.footer.author", author))
      setHtml(gE("APP__Footer_Content_ChangeDate"), AppEnv.getMessage("app.footer.changedate", changeDate))
    }
  }


  def markSBEntry(ucName: String) = {
    println(s"Call markSBEntry: ${ucName}")
    val elem = document.querySelectorAll(s"[data-sbentry='${ucName}']").head.asInstanceOf[HTMLElement].parentElement
    if (elem!=null) elem.classList.remove("collapse")    
    document.querySelectorAll(s"[data-sbtext]").map(_.asInstanceOf[HTMLElement].classList.remove("text-light"))
    document.querySelectorAll(s"[data-sbtext='${ucName}']").map(_.asInstanceOf[HTMLElement].classList.add("text-light"))
  }

  def unmarkSBEntry(ucName: String) = {
    val elem = document.querySelectorAll(s"[data-sbentry='${ucName}']").head.asInstanceOf[HTMLElement].parentElement
    document.querySelectorAll(s"[data-sbtext]").map(_.asInstanceOf[HTMLElement].classList.remove("text-light"))
  }


  def showSBMenu(name: String) = {
    val liElem = document.querySelector(s"[data-sbentry='${name}']").asInstanceOf[HTMLElement]
    liElem.querySelector(s"[data-toggle='collapse']").asInstanceOf[HTMLElement].classList.remove("collapsed")
    gE(s"APP__Sidebar__${name}").classList.add("show")
  }

  def hideSBMenu(name: String) = {
    val liElem = document.querySelector(s"[data-sbentry='${name}']").asInstanceOf[HTMLElement]
    liElem.querySelector(s"[data-toggle='collapse']").asInstanceOf[HTMLElement].classList.add("collapsed")
    gE(s"APP__Sidebar__${name}").classList.remove("show")
  }

}