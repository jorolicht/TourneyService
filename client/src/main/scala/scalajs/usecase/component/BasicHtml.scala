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
import org.scalajs.dom.ext._


// tourney service imports
import shared.model.{ TournBase, Tourney, Competition }
import shared.utils.UseCaseParam
import shared.utils.Error

import scalajs.service._
import scalajs.usecase.Helper
import scalajs.{ App, AppEnv }


/** BasicHtml most methods have a implicit parameter type UseCaseParam
 *  
 *  - idBase: Html id attribute (idBase)  
 *  - msgPrefix: prefix of message label
 *  - objName: name of exported objecet accessable form javascript
 *  - dataAttr: Html universal data attribute (data-<dataAttrPref>-<xxx>)
 */
object BasicHtml {

  def disProp(visible: Boolean): String = if (visible) "block" else "none"

  def setResult(text: String) = insertHtml_("APP__Load", "afterbegin", s"""<textarea id="APP_Result" style="display:none;">${text}</textarea>""")


  def setHtml_(id: String, content: => String): Unit = {
    try document.getElementById(id).asInstanceOf[HTMLElement].innerHTML = content
    catch { case _: Throwable => Helper.error("setHtml_", s"id: ${id} -> ${content}") } 
  }

  def insertHtml_(id: String, pos: String, content: String): Unit = {
    try document.getElementById(id).asInstanceOf[HTMLElement].insertAdjacentHTML(pos,content)
    catch { case _: Throwable => AppEnv.logger.error(s"insertHtml_ -> id: ${id} pos: ${pos} content: ${content.take(10)}") } 
  }  
  
  def setVisible_(id: String, visible: Boolean): Unit = {
    try document.getElementById(id).asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible))
    catch { case _: Throwable => Helper.error("setVisible_", s"id: ${id} -> ${visible}") }
  }

  def setAttribute_(id: String, attrName: String, attrValue: String): Unit = {
    try document.getElementById(id).setAttribute(attrName, attrValue)
    catch { case _: Throwable => Helper.error("setAttribute_", s"id: ${id} -> ${attrValue}") } 
  }
  
  def getAttribute_(id: String, attrName: String, defVal: String=""): String = {
    try document.getElementById(id).getAttribute(attrName)
    catch { case _: Throwable => { Helper.error("getAttribute_", s"id: ${id} / ${attrName}"); defVal }}
  }

  def setVisibleByAttr_(name: String, attr: String, visible: Boolean): Unit = {
    try document.querySelectorAll(s"[data-${name}='${attr}']").map(_.asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible)))
    catch { case _: Throwable => { Helper.error("setVisibleByAtrr_", s"name: ${name} / ${attr}") }}
  } 
  
  def togVisible_(id: String): Unit = {
    val elem = document.getElementById(id).asInstanceOf[HTMLElement]
    try if (elem.style.display == "none") elem.style.display="block" else elem.style.display="none"
    catch { case _: Throwable => Helper.error("togVisible_", s"id: ${id}") }
  }

  def addClass_(elemId: String, _class: String*): Unit = {
    try _class.foreach(cValue => document.getElementById(elemId).classList.add(cValue) )
    catch { case _: Throwable => Helper.error("addClass", s"id: ${elemId} class: ${_class}") } 
  }

  def removeClass_(elemId: String, _class: String*): Unit = {
    try _class.foreach(cValue => document.getElementById(elemId).classList.remove(cValue) ) 
    catch { case _: Throwable => Helper.error("removeClass", s"id: ${elemId} class: ${_class}") } 
  }

  def getMsg_(key: String, args: String*): String = AppEnv.getMessage(key, args: _*)
}

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

  // getElemById - get the html element
  def getElemById(id: String)(implicit ucp: UseCaseParam) = {
    val elem = document.getElementById(ucp.idBase + "__" + id)
    if (elem == null) document.createElement("div") else elem
  }  

  // getElemById_ - get the html element
  def getElemById_(id: String) = {
    val elem = document.getElementById(id)
    if (elem == null) document.createElement("div") else elem
  }  

  // getId - adds usecase prefix to id
  def uc(id: String)(implicit ucp: UseCaseParam) = { s"${ucp.idBase}__${id}"}
  def getId(id: String, prefix: String = "")(implicit ucp: UseCaseParam) = { s"${prefix}${ucp.idBase}__${id}"}
  def getIdHa(id: String)(implicit ucp: UseCaseParam) = { s"#${ucp.idBase}__${id}"}

  def checkId(id: String)(implicit ucp: UseCaseParam):Boolean = { 
    try (document.getElementById(ucp.idBase + "__" + id).textContent.length > 0)
    catch { case _: Throwable => false }
  }  


  /** setHtml
    *
    * @param elemId  - id of html element
    * @param content - html content of element
    */ 
  def setHtml(elemId: String, content: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + elemId).asInstanceOf[HTMLElement].innerHTML = content
    catch { case _: Throwable => error("setHtml", s"id: ${ucp.idBase}__${elemId} content: ${content.take(10)}") } 
  }

  def setHtml(elemId: String, content: play.twirl.api.Html)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + elemId).asInstanceOf[HTMLElement].innerHTML = content.toString
    catch { case _: Throwable => error("setHtml", s"id: ${ucp.idBase}__${elemId} content: ${content.toString.take(10)}") } 
  }

  def setHtml(elem: HTMLElement, content: play.twirl.api.Html)(implicit ucp: UseCaseParam): Unit = {
    try elem.innerHTML = content.toString
    catch { case _: Throwable => error("setHtml", s"content: ${content.toString.take(10)}") } 
  }  
  
  def setHtml(elem: HTMLElement, content: String)(implicit ucp: UseCaseParam): Unit = {
    try elem.innerHTML = content
    catch { case _: Throwable => error("setHtml", s"content: ${content.toString.take(10)}") } 
  }  


  def insertHtml(elemId: String, pos: String, content: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + elemId).asInstanceOf[HTMLElement].insertAdjacentHTML(pos,content)
    catch { case _: Throwable => error("insertHtml", s"id: ${ucp.idBase}__${elemId} pos: ${pos} content: ${content.take(10)}") } 
  }
  


  def setHtmlVisible(id: String, visible: Boolean, content: String="")(implicit ucp: UseCaseParam): Unit = {
    try {
      val elem = document.getElementById(ucp.idBase + "__" + id).asInstanceOf[HTMLElement]
      elem.style.setProperty("display", disProp(visible))
      if (visible) elem.innerHTML = content
    } catch { case _: Throwable => error(s"setHtmlVisible", s"id: ${ucp.idBase}__${id} visible: ${visible} content: ${content.take(10)}") } 
  }

  def setText(id: String, content: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[HTMLElement].innerText = content
    catch { case _: Throwable => error(s"setHtml", s"id: ${ucp.idBase}__${id} content: ${content.take(10)}") } 
  }

  def setDisabled(id: String, value: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[Input].disabled = value
    catch { case _: Throwable => error(s"setDisabled", s"id: ${ucp.idBase}__${id} value: ${value}") } 
  }

  def setDisabledByName(name: String, value: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try {
      val container = document.getElementsByName(ucp.idBase + "__" + name)
      container.map(_.asInstanceOf[Input].disabled = value)      
    } catch { case _: Throwable => error(s"setDisabledByName", s"name: ${ucp.idBase}__${name} value: ${value}") } 
  }

  def setDisabled(elem:HTMLElement, value: Boolean): Unit = {
    try elem.asInstanceOf[Input].disabled = value
    catch { case _: Throwable => AppEnv.logger.error(s"setDisabled__ -> value: ${value}") } 
  }

  def showModal(id: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[js.Dynamic].showModal()
    catch { case _: Throwable => error(s"showModal", s"id: ${ucp.idBase}__${id}") } 
  }


  /**
    * set visibility of all Html elements with id
    *
    * @param elemId - id of html element
    * @param visible - true or false
    */ 
  def setVisible(id: String, visible: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible))
    catch { case _: Throwable => error(s"setVisible", s"id: ${ucp.idBase}__${id} visible: ${visible}") }
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

  def setVisibleByName(name: String, visible: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.querySelectorAll(s"[data-${ucp.dataAttr}-${name}]").map(_.asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible)))
    catch { case _: Throwable => Helper.error("setVisibleByName", s"name: ${name} visible: ${visible}") }
  }

  def showHelp(key: String, visible: Boolean = true)(implicit ucp: UseCaseParam) = {
    val id = s"${ucp.idBase}__${key}Hlp"
    try {
      document.getElementById(id).asInstanceOf[HTMLElement].style.setProperty("display", disProp(visible))
      if (visible) document.getElementById(id).asInstanceOf[HTMLElement].innerHTML = AppEnv.getMessage(ucp.msgPrefix + ".hlp." + key)
    } catch { case _: Throwable => Helper.error("showHelp", s"id: ${id} visible: ${visible}") }
  }

  def showHelpMsg(key: String, msg: String, visible: Boolean = true)(implicit ucp: UseCaseParam) = {
    val id = s"${ucp.idBase}__${key}Hlp"
    try {
      val elem = document.getElementById(id).asInstanceOf[HTMLElement]
      elem.style.setProperty("display", disProp(visible))
      if (visible) elem.innerHTML = AppEnv.getMessage(ucp.msgPrefix + ".hlp." + msg)
    } catch { case _: Throwable => Helper.error("showHelpMsg", s"id: ${id} msg: ${msg} visible: ${visible}") }
  }

  def showHelpTxt(key: String, txt: String, visible: Boolean = true)(implicit ucp: UseCaseParam) = {
    val id = s"${ucp.idBase}__${key}Hlp"
    try {
      val elem = document.getElementById(id).asInstanceOf[HTMLElement]
      elem.style.setProperty("display", disProp(visible))
      if (visible) elem.innerHTML = txt
    } catch { case _: Throwable => Helper.error("showHelpTxt", s"id: ${id} txt: ${txt} visible: ${visible}") }
  }

  def markInput__(elem: HTMLElement, markError: Option[Boolean])= {
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
    } catch { case _: Throwable => Helper.error("markInput__", elem.toString) }
  }

  def markInput_(id: String, markError: Option[Boolean])= {
    try markInput__(document.getElementById(id).asInstanceOf[HTMLElement], markError) 
    catch { case _: Throwable => Helper.error("markInput_", id) }
  }

  def markInput(id: String, markError: Option[Boolean])(implicit ucp: UseCaseParam) = markInput_(ucp.idBase + "__" + id, markError)


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
  def getData(id: String, name: String, default: Int)(implicit ucp: UseCaseParam): Int = {
    try getData(document.getElementById(s"${ucp.idBase}__${id}").asInstanceOf[HTMLElement], name, default)
    catch { case _: Throwable => Helper.error("getData", s"id: ${id} name: ${name} as Int"); default }
  }  

  def getData(id: String, name: String, default: Long)(implicit ucp: UseCaseParam): Long = {
    try getData(document.getElementById(s"${ucp.idBase}__${id}").asInstanceOf[HTMLElement], name, default)
    catch { case _: Throwable => Helper.error("getData", s"id: ${id} name: ${name} as Long"); default }
  }  

  def getData(id: String, name: String, default: String)(implicit ucp: UseCaseParam): String = {
    try getData(document.getElementById(s"${ucp.idBase}__${id}").asInstanceOf[HTMLElement], name, default)
    catch { case _: Throwable => Helper.error("getData", s"id: ${id} name: ${name} as String"); default }
  }

  def getData(elem: HTMLElement, name: String, default: Int): Int = {
    try { elem.getAttribute(s"data-${name}").toIntOption.getOrElse(default)}
    catch { case _: Throwable => Helper.error("getData", s"${name} as Int"); default }
  }
  

  def getData(elem: HTMLElement, name: String, default: Long): Long = {
    try { elem.getAttribute(s"data-${name}").toLongOption.getOrElse(default)}
    catch { case _: Throwable => Helper.error("getData", s"${name} as Long"); default }
  }
  
  def getData(elem: HTMLElement, name: String, default: String=""): String = {
    try   elem.getAttribute(s"data-${name}")
    catch { case _: Throwable => Helper.error("getData", s"${name} as String"); default }
  }

  def setData[A](id: String, attr: String, value: A)(implicit ucp: UseCaseParam) = {
    try document.getElementById(s"${ucp.idBase}__${id}").asInstanceOf[HTMLElement].setAttribute(s"data-${attr}", value.toString)
    catch { case _: Throwable => Helper.error("setData", s"id: ${ucp.idBase}__${id} attribute: ${attr} value: ${value}") }
  }

  def setData[A](elem: HTMLElement, attr: String, value: A) = {
    try elem.setAttribute(s"data-${attr}", value.toString)
    catch { case _: Throwable => Helper.error("setData", s"attribute: ${attr} value: ${value}") }
  }

  /**
    * getInput read input value from Html input element 
    *
    * @param name
    * @param defVal
    * @return
    */
  def getInput(name: String, defVal: String = "")(implicit ucp: UseCaseParam): String = {
    try { if (name=="") "" else document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value }
    catch { case _: Throwable => defVal }
  }

  def getInput_(name: String, defVal: String = ""): String = {
    try { if (name=="") "" else document.getElementById(name).asInstanceOf[Input].value }
    catch { case _: Throwable => defVal }
  }

  def getInput(name: String, defVal: Long)(implicit ucp: UseCaseParam): Long = {
    try document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value.toLong 
    catch { case _: Throwable => defVal }
  }
  def getInput(name: String, defVal: Int)(implicit ucp: UseCaseParam): Int = {
    try document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value.toInt 
    catch { case _: Throwable => defVal }
  }
  def getInput(name: String, defVal: Boolean)(implicit ucp: UseCaseParam): Boolean = {
    try document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].checked
    catch { case _: Throwable => defVal }
  }
  def getInput(elem: HTMLElement, defVal: Long)(implicit ucp: UseCaseParam): Long = {
    try elem.asInstanceOf[Input].value.toLong
    catch { case _: Throwable =>  Helper.error("getInput", "element as long");  defVal }
  }
  def getInput(elem: HTMLElement, defVal: String)(implicit ucp: UseCaseParam): String = {
    try elem.asInstanceOf[Input].value
    catch { case _: Throwable =>  Helper.error("getInput", "element as string");  defVal }
  }

  def getBooleanOption(name: String)(implicit ucp: UseCaseParam): Option[Boolean] = {
    try document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value.toBooleanOption
    catch { case _: Throwable => Helper.error("getBooleanOption", s"${name}"); None }
  }

  def getIntOption(name: String)(implicit ucp: UseCaseParam): Option[Int] = {
    try document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value.toIntOption
    catch { case _: Throwable => Helper.error("getIntOption", s"${name}"); None }
  }

  def setBooleanOption(name: String, input: Option[Boolean])(implicit ucp: UseCaseParam): Unit = {
    try input match {
      case None        => document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value = "None"
      case Some(value) => document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value = value.toString
    }
    catch { case _: Throwable => error(s"setBooleanOption", s"name: ${ucp.idBase}__${name}") }
  }

  def setIntOption(name: String, input: Option[Int])(implicit ucp: UseCaseParam): Unit = {
    try input match {
      case None        => document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value = "None"
      case Some(value) => document.getElementById(ucp.idBase + "__" + name).asInstanceOf[Input].value = value.toString
    }
    catch { case _: Throwable => error(s"setIntOption", s"name: ${ucp.idBase}__${name}") }
  }  


  def resetInput(name: String)(implicit ucp: UseCaseParam): Unit = {
    try {
      val divNode = document.getElementById(ucp.idBase + "__" + name)
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
    catch { case _: Throwable =>  Helper.error("resetInput", s"${name}") }
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
    $(getId(ident, "#")).datetimepicker(tParam)
    setInput(ident, curDateTime)
  }


  /** setInput sets value of Html input element
    * 
    * @param id attribut of element
    * @param text value of element
    * @param ucp usecase param
    */
  def setInput(id: String, text: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[Input].value = text
    catch { case _: Throwable => error(s"setInput", s"id: ${ucp.idBase}__${id}") }
  }  

  def setInput_(id: String, text: String): Unit = {
    try document.getElementById(id).asInstanceOf[Input].value = text
    catch { case _: Throwable => AppEnv.logger.error(s"setInput_ -> id: ${id} value: ${text}") }
  } 

  def getTextContent(id: String, defVal: String="")(implicit ucp: UseCaseParam): String = {
    try document.getElementById(ucp.idBase + "__" + id).textContent
    catch { case _: Throwable => error("getTextContent", s"id: ${ucp.idBase}__${id}"); defVal }
  }

  def getTextAsLong(id: String, defVal: Long=0L)(implicit ucp: UseCaseParam): Long = {
    try document.getElementById(ucp.idBase + "__" + id).textContent.toLongOption.getOrElse(defVal)
    catch { case _: Throwable => error("getTextAsLong", s"id: ${ucp.idBase}__${id}"); defVal }
  }


  def exists(id: String)(implicit ucp: UseCaseParam): Boolean = { (document.getElementById(ucp.idBase + "__" + id) != null) }  
  def exists_(id: String)(implicit ucp: UseCaseParam): Boolean = { (document.getElementById(id) != null) } 

  /* getElementById does not know what kind of element nodeValue is. 
   * It does not have access to the .html to figure that out, and hence it returns a very 
   * generic Element. But in fact, you know that it is an input element, i.e., 
   * an org.scalajs.dom.Input. You can tell that to the compiler with .asInstanceOf[Input]. 
   * A generic Element does not have a value property, only Inputs have.
   * -> asInstanceOf[dom.Input]
   */

  /** get/setAttribute
   * 
   * @param elemId of html element
   * @param attrName value of attribute
   * @param defVal if no value is present
   * @param attrValue value of attribute
   */
  def getAttribute(elemId: String, attrName: String, defVal: String="")(implicit ucp: UseCaseParam): String = {
    try document.getElementById(ucp.idBase + "__" + elemId).getAttribute(attrName)
    catch { case _: Throwable => { error("getAttribute", s"id: ${ucp.idBase}__${elemId} attrName: ${attrName}"); defVal }}
  }

  def setAttribute(elemId: String, attrName: String, attrValue: String)(implicit ucp: UseCaseParam): Unit = {
    BasicHtml.setAttribute_(ucp.idBase + "__" + elemId, attrName, attrValue)
  }  

  def removeAttribute(id: String, attrName: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).removeAttribute(attrName)
    catch { case _: Throwable => error("removeAttribute", s"id: ${ucp.idBase}__${id} attrName: ${attrName}") } 
  }

  def selectOption(id: String, selValue: String)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[Input].value = selValue
    catch { case _: Throwable => error("selectOption", s"id: ${ucp.idBase}__${id} option: ${selValue}") } 
  }

  def addClass(elemId: String, _class: String*)(implicit ucp: UseCaseParam): Unit = {
    try _class.foreach(cValue => document.getElementById(ucp.idBase + "__" + elemId).classList.add(cValue) )
    catch { case _: Throwable => error("addClass", s"id: ${ucp.idBase}__${elemId} class: ${_class}") } 
  }

  def removeClass(elemId: String, _class: String*)(implicit ucp: UseCaseParam): Unit = {
    try _class.foreach(cValue => document.getElementById(ucp.idBase + "__" + elemId).classList.remove(cValue))
    catch { case _: Throwable => error("removeClass", s"id: ${ucp.idBase}__${elemId}  class: ${_class}") } 
  }

  /** add/removeDataClass
   * 
   * @param name of (user defined) data attribute
   * @param attr value of data attribute
   * @param className to add/remove
   */
  def addDataClass(name: String, attr: String, className: String)(implicit ucp: UseCaseParam): Unit = {
    val selector = if (attr == "") s"[data-${ucp.dataAttr}-${name}]" else s"[data-${ucp.dataAttr}-${name}='${attr}']"
    document.querySelectorAll(selector).map(_.asInstanceOf[HTMLElement].classList.add(className))
  }
  def removeDataClass(name: String, attr: String, className: String)(implicit ucp: UseCaseParam): Unit = {
    val selector = if (attr == "") s"[data-${ucp.dataAttr}-${name}]" else s"[data-${ucp.dataAttr}-${name}='${attr}']"
    document.querySelectorAll(selector).map(_.asInstanceOf[HTMLElement].classList.remove(className))
  }

  def setRadioBtn(id: String, value: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[Input].checked = value
    catch { case _: Throwable => error("setRadioBtn", s"id: ${ucp.idBase}__${id} -> ${value}") } 
  }

  def setRadioBtnByValue(name: String, value: String)(implicit ucp: UseCaseParam): Unit = {
    val inpNodes = document.getElementsByName(ucp.idBase + "__" + name).asInstanceOf[NodeList]
    val node = inpNodes.filter(_.asInstanceOf[Input].value == value).head
    node.asInstanceOf[Input].checked= true
  }

  def getRadioBtn(name: String)(implicit ucp: UseCaseParam): String = {
    try {
      val inpNodes = document.getElementsByName(ucp.idBase + "__" + name).asInstanceOf[NodeList]
      val node = inpNodes.filter(_.asInstanceOf[Input].checked).head
      node.asInstanceOf[Input].value
    }
    catch { case _: Throwable => error("getRadioBtn", s"name: ${ucp.idBase}__${name}"); "" }
  }

  def getRadioBtn(name: String, defVal: Boolean)(implicit ucp: UseCaseParam): Boolean = {
    try {
      val inpNodes = document.getElementsByName(ucp.idBase + "__" + name).asInstanceOf[NodeList]
      val node = inpNodes.filter(_.asInstanceOf[Input].checked).head
      node.asInstanceOf[Input].value.toBooleanOption.getOrElse(defVal)
    }
    catch { case _: Throwable => error("getRadioBtn", s"name: ${ucp.idBase}__${name}"); defVal }
  }


  def setCheckbox(id: String, value: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[Input].checked = value
    catch { case _: Throwable => error("setCheckbox", s"id: ${ucp.idBase}__${id} -> ${value}") } 
  }

  def getCheckbox(id: String)(implicit ucp: UseCaseParam): Boolean = {
    try document.getElementById(ucp.idBase + "__" + id).asInstanceOf[Input].checked
    catch { case _: Throwable => error("getCheckbox", s"id: ${ucp.idBase}__${id}"); false } 
  }

  
  /** togCollapse ads/removes class collapse
    * 
    * @param id attribute of element
    */
  def togCollapse(id1: String, id2: String="")(implicit ucp: UseCaseParam): Unit = 
    if (id2=="") togCollapse_(s"${ucp.idBase}__${id1}") else togCollapse_(s"${ucp.idBase}__${id1}", s"${ucp.idBase}__${id2}" )  
  
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
    } catch { case _: Throwable => Helper.error("togCollapse", s"id1: ${id1} id2: ${id2}") }
  }

  def collapse(name: String, hide: Boolean)(implicit ucp: UseCaseParam): Unit = {
    try {
      val elem = document.getElementById(s"${ucp.idBase}__${name}")
      if (hide) {
        if (!elem.classList.contains("collapse")) elem.classList.add("collapse") 
      } else {
        if (elem.classList.contains("collapse")) elem.classList.remove("collapse")
      } 
    } catch { case _: Throwable => Helper.error("collapse", s"name: ${ucp.idBase}__${name} value: ${hide}") }
  }

  // setTableRow
  def selTableRow(id: String, classText: String="text-white", classBg: String="bg-secondary") = {
    val elems2rem = document.getElementById(id).parentNode.asInstanceOf[HTMLElement].getElementsByTagName("tr").asInstanceOf[NodeListOf[HTMLElement]]
    val selElem   = document.getElementById(id)
    elems2rem.map { elem => elem.classList.remove(classText); elem.classList.remove(classBg) }  
    selElem.classList.add(classText)
    selElem.classList.add(classBg)
  }  


  def innerText(id: String, content: String): Unit = {
    try document.getElementById(id).asInstanceOf[HTMLElement].innerText = content
    catch { case _: Throwable => errLog(s"innerText => id: ${id} content: ${content.take(10)}") } 
  }


  /** getMsg/getError
    *
    * @param key  - message key
    * @param args - inserts into message
    */
  def getMsg(key: String, args: String*)(implicit ucp: UseCaseParam): String = AppEnv.getMessage(ucp.msgPrefix + "." + key, args: _*)

  def setMainContent(content: String): Unit = document.getElementById("mainContent").asInstanceOf[HTMLElement].innerHTML = content
  def setMainContent(content: play.twirl.api.Html): Unit = document.getElementById("mainContent").asInstanceOf[HTMLElement].innerHTML = content.toString
  def disProp(visible: Boolean): String = if (visible) "block" else "none"


    // showAlert  
  def showAlert(text: String): String = {
    s"""
      |<div class="alert alert-info" role="alert">
      |  <span class="tuse-font-1">${text}</span>
      |</div>
    """.stripMargin('|')
  }

   // confirm dialog
  def confirmDlg(title: String, msg: String): Future[Boolean] =
    scalajs.usecase.dialog.DlgBox.showStd(title, msg, Seq("cancel", "ok")).map { _ match {
     case 2 => true
     case _ => false
    }}


}