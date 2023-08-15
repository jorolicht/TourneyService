package scalajs.usecase.component

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom.raw._
import org.scalajs.dom._                 // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

// tourney service imports
import shared.model._

import shared.utils.UseCaseParam
import shared.utils.Routines._
import shared.utils.Error


import scalajs.usecase.component.BasicHtml._
import scalajs.service._
import scalajs.{ App, AppEnv }


@JSExportTopLevel("InputCtrl")
object InputCtrl extends BasicHtml with TourneySvc 
{
  /**  redirect
   *  
   */ 
  @JSExport
  def redirect(target: String): Unit = {
    target match {
      case "home" => window.location.href = scalajs.AppEnv.home 
      case _      =>
    }
  }  


  /**  setResult
   *  
   */ 
  @JSExport
  def setResult(visi: Boolean, header: String="", body: String="", alertTyp: String="success"): Unit = {
    showResult(visi, header, body, alertTyp)
  }  


  /**  setContent
   * 
   *  @param fileName 
   */ 
  @JSExport
  def setContent(fileName: String=""): Unit = {
    try  App.execUseCase("HomeMain", "Content", fileName)
    catch  { case _: Throwable => App.execUseCase("HomeMain", "Error", getError(Error("err0124.invalidActionRequest", s"HomeMain/Content/${fileName}"))) } 
  }  

  /**  eventSidebar - click on sidebar entry
   *  @param ucName - name of usecase
   *  @param redir  - flag signaling redirection
   */ 
  @JSExport
  def eventUnload(param: String=""): Unit = {
    //AppEnv.setLocalStorage("UNLOAD","XXX")
  }  

  @JSExport
  def eventHashchange(param: String=""): Unit = {  }  

  /**  eventSidebar - click on sidebar entry
   *  @param ucName - name of usecase
   *  @param redir  - flag signaling redirection
   */ 
  @JSExport
  def eventSidebar(ucName: String, redir: Boolean = false): Unit = {
    var effUcParam = ""
    var effUcInfo = ""
    val effUcName = 
      if (redir) {
        val node = document.getElementById(s"APP__Sidebar__${ucName}").asInstanceOf[HTMLElement]
        try {
          // class attribute show signal für collapsed or folded directory
          // when folded do nothing (usecase "HomeMain", "None")
          if (!node.classList.contains("show")) { 
            val nAttribute = node.getAttribute(s"data-sbredir-${ucName}") 
            AppEnv.info("eventSidebar", s"ucName: ${ucName} nAttribute: $nAttribute not show/hide") 
            nAttribute
          } else{
            AppEnv.info("eventSidebar", s"ucName: ${ucName} redirect: ${redir} show")  
            ucName
          } 
        } catch  { case _: Throwable => { 
          effUcParam = "ErrorCode"
          effUcInfo = "err0090.internal.sidebarRedirect"
          "HomeMain"  
        }}
      } else {
        AppEnv.info("eventSidebar", s"ucName: ${ucName} redirect: ${redir}")  
        ucName
      }
    App.execUseCase(effUcName, effUcParam, effUcInfo)
  }


  @JSExport
  def eventUseCase(ucName: String, ucParam: String = "", ucInfo: String = ""): Unit = { 
    try  App.execUseCase(ucName, ucParam, ucInfo)
    catch  { case _: Throwable => App.execUseCase("HomeMain", "Error", getError(Error("err0124.invalidActionRequest", ucName))) }
  }  


  // (un)collapse
  @JSExport
  def collapse(id1: String, id2: String="") = togCollapse_(id1, id2)
  
  // 
  @JSExport
  def eventCard(event: String, bodyId: String, buttonId: String) = {
    event match {
      case "toggle" => {
        togCollapse_(bodyId)
        togVisible_(buttonId)
      }
      case _        => AppEnv.error(s"eventCard", "unknown event")
    }
  }

  // eventRadioCard
  @JSExport
  def eventRadioCard(elem: raw.HTMLInputElement): Unit = {
    $(s"[data-radiocard=${elem.name}]").hide()
    if (elem.checked) {
      $(s"#${elem.id}Card").show()
    }
  }


  /*
  ** Text INPUT MANAGEMENT
  */
  /** eventText - change on a text input form
   * 
   */
  @JSExport
  def eventText(event: String, elem: raw.HTMLInputElement): Unit = {    
    val id = elem.id

    if (elem.value=="") {
      markInput(elem, None)
    } else {
      val length = try { elem.getAttribute("minlength").toInt } catch { case _: Throwable => 0 }
      val invalid = (elem.value.length < length)
      markInput(elem, Some(invalid))
      if (event == "onchange" & invalid) showHlp_(id, true)
      if (event == "oninput") showHlp_(id, false)
    }
  }

  /** getText - get input as String Option
   * 
   */  
  def getText(name: String, help:Boolean=true, minLength:Int=0)(implicit ucp: UseCaseParam): Option[String] = {    
    val value = getInput(name, "")
    if (value.length >= minLength) {
      if (help) showHlp(name, false)
      Some(value)
    } else {
      if (help) showHlp(name, true)
      None
    }
  }


  /*
  ** CLUB NAME INPUT MANAGEMENT
  */
  /** eventClub - change on clubname input form
   * 
   */
  @JSExport
  def eventClub(event: String, elem: raw.HTMLInputElement): Unit = {
    
    val id = elem.id
    showHlp_(id, false)

    if (elem.value=="") {
      markInput(elem, None)
    } else {
      val invalid = !validClub(elem.value)
      markInput(elem, Some(invalid))
      if (event == "onchange" & invalid) showHlp_(id, true)
    }
  }

  /** getClub - get club name  as String Option
   * 
   */  
  def getClub(name: String, help:Boolean=true, required:Boolean=true)(implicit ucp: UseCaseParam): Option[String] = {    
    val club = getInput(name, "")

    if (validClub(club)) {
      if (help) showHlp(name, false)
      Some(club)
    } else {
      if (help & required) showHlp(name, true)
      if (required) None else Some(club)
    }
  }  

  // check club name format
  def validClub(name: String): Boolean = {
    try (urify(name).length >= 8)
    catch { case _: Throwable => false }
  }


  /*
  ** COUNTRY INPUT MANAGEMENT
  */
  /** eventCountry - change on country input form
   * 
   */
  @JSExport
  def eventCountry(event: String, elem: raw.HTMLInputElement): Unit = {
    
    val id = elem.id
    showHlp_(id, false)
    AppEnv.info("eventCountry", s"id: ${id} event: ${event}")
  }

  def getCountry(id: String, help:Boolean=true, required:Boolean=true)(implicit ucp: UseCaseParam): Option[String] = {
    val country = getInput(id, "")
    //AppEnv.debug("getCountry", s"country: ${country}  id: ${id}")
    if (country == "") {
      if (help & required) showHlp(id, true)
      if (required) None else Some("")
    } else {
      if (help) showHlp(id, false)
      Some(country)
    }
  }


  /*
  ** NAME INPUT MANAGEMENT
  */
  /** eventName - change on name input form
   * 
   */
  @JSExport
  def eventName(event: String, elem: raw.HTMLInputElement): Unit = {    
    val id = elem.id
    showHlp_(id, false)

    if (elem.value=="") {
      markInput(elem, None)
    } else {
      val invalid = !validName(elem.value)
      markInput(elem, Some(invalid))
      if (event == "onchange" & invalid) showHlp_(id, true)
    }
  }

  /** getName - returns  Some valid name or None, sets help
   *                  
   * @param id of name html input field
   * @param show flag if help should be shown
   * @return optionally naem
   */ 
  def getName(id: String, help: Boolean=true, required:Boolean=true)(implicit ucp: UseCaseParam): Option[String] = {
    val name = getInput(id, "")

    if (validName(name)) {
      if (help) showHlp(id, false)
      Some(name)
    } else {
      if (help & required) showHlp(id, true)
      if (required) None else Some(name)
    }
  }

  // check name format
  def validName(name: String): Boolean = {
    val nArray = name.split(",")
    try (nArray.length == 2) & (nArray(0).trim.length >= 2) & (nArray(1).trim.length >= 2) 
    catch { case _: Throwable => false }
  }
 

  /*
  ** EMAIL INPUT MANAGEMENT
  */
  /** eventEmail - change on email input form
   * 
   */
  @JSExport
  def eventEmail(event: String, elem: raw.HTMLInputElement): Unit = {
    val id = elem.id
    // first reset help info 
    showHlp_(id, false)

    if (elem.value=="") {
      markInput(elem, None)
    } else {
      val invalid = !validEmail(elem.value)
      markInput(elem, Some(invalid))
      if (event == "onchange" & invalid) showHlp_(id, true)
    }
  }

  /** getEmail - returns valid Some valid email or None, sets help
   *                  
   * @param id of email html input field
   * @param show flag if help should be shown
   * @return optionally email
   */ 
  def getEmail(id: String, help: Boolean=true, required:Boolean=true)(implicit ucp: UseCaseParam): Option[String] = {
    val email = getInput(id, "")

    if (validEmail(email)) {
      if (help) showHlp(id, false)
      Some(email)
    } else {
      if (help & required) showHlp(id, true)
      if (required) None else Some("")
    }
  }


  /*
  ** PASSWORD INPUT MANAGEMENT
  */
  /** eventPassword - handle pasword change and input events
   *                  
   * @param event either onchange, oninput or show
   * @param elem html element of passwort input
   * @param firstId optional id of first password input field to compare (equalness)
   */  
  @JSExport
  def eventPassword(event: String, elem: raw.HTMLInputElement, auxId: String=""): Unit = {
    event match {
      case "show"    => togglePassword(auxId)

      case _         => {
        val value1 = elem.value
        val value2 = getInput(s"_${auxId}", "")
        val id = elem.id
        showHlp_(id, false)

        if (value1 == "") {
          markInput(elem, None)
        } else {
          //AppEnv.debug("eventPassword", s"value1: ${value1}  value2: ${value2}")
          val invalid = if (value2=="") !validPassword(value1) else (value1!=value2)
          markInput(elem, Some(invalid))
          if (!invalid & auxId!="") showHlp_(auxId, false)
          if (event == "onchange" & invalid) showHlp_(id, true)
        }
      }
    }
  }

  // check password
  def validPassword(pw: String): Boolean = {
    (pw.filter(_.isUpper).length>0) & (pw.filter(_.isLower).length>0) & 
    (pw.filter(_.isDigit).length>0) & (pw.length>=6)
  }

  /** togglePassword - show/hide password
    * 
    * @param pwdId
    */
  def togglePassword(pwdId: String): Unit = {
    val showId = pwdId + "Eye"
    if (getAttribute_(pwdId, "type") == "password") {
      setAttribute_(pwdId, "type", "text")
      addClass(gE(showId), "fa-eye"); removeClass(gE(showId),"fa-eye-slash")
    } else {
      setAttribute_(pwdId, "type", "password")
      addClass(gE(showId), "fa-eye-slash"); removeClass(gE(showId),"fa-eye")          
    }
  }

  /** getPassword - returns Some(password) or None
    *
    * @param id1 - id of password input field
    * @param id2 - id of repeated password (optional)
    * @param show - show help if input is not valid
    */
  def getPassword(id1: String, id2: String="", show:Boolean=true)(implicit ucp: UseCaseParam): Option[String] = {
    val password = getInput(id1, "")
    val password2 = if (id2 != "") getInput(id2, "") else ""

    try {
      //AppEnv.debug("getPassword", s"password: ${password}  password2: ${password2}")
      val valid = validPassword(password) & ((id2=="") | (password == password2))
      if (valid) {
        // AppEnv.debug("getPassword", s"id1: ${id1}")
        if (show) showHlp(id1, false)
        if (show & (id2!="")) showHlp(id2, false)
        Some(password)
      } else {
        if (show) showHlp(id1, !validPassword(password))
        if (show & id2!="") showHlp(id2, (password != password2) & password!="")
        None
      }
    } catch { case _: Throwable => AppEnv.error("getPassword", s"id1: ${id1} id2: ${id2}"); None }
  }


  /** showHlp - show help message to corresponding input (id with Suffix "Hlp")
    * 
    * @param key
    * @param visible
    * @param ucp
    */
  def showHlp(key: String, visible: Boolean = true)(implicit ucp: UseCaseParam) = showHlp_(ucp.idBase + "__" + key, visible)
  def showHlp_(key: String, visible: Boolean = true) = {
    val helpKey = key + "Hlp"
    //AppEnv.info("showHelp", s"key: ${helpKey} ${visible}")
    try { 
      val elem = document.getElementById(helpKey).asInstanceOf[HTMLElement]
      if (elem != null) elem.style.setProperty("display", disProp(visible))
    }
    catch { case _: Throwable => AppEnv.error("showHlp", s"key: ${helpKey} ${visible}") }
  }

}