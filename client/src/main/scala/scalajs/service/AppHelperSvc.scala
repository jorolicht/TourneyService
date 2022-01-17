package scalajs.service

import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom.raw._
import org.scalajs.dom._                 // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import scalajs.usecase.Helper
import scalajs.usecase.component.BasicHtml._
import scalajs. { App, AppEnv, AppStatus }

trait AppHelperSvc { 

  def getError(key: String, args: String*): String = AppEnv.getMessage(key, args: _*)
  def getError(err: shared.utils.Error): String    = AppEnv.getMessage(err.msgCode, err.in1, err.in2)
  def getErrStack(err: shared.utils.Error): String = s"CallStack: ${err.callStack} -> ${AppEnv.getMessage(err.msgCode, err.in1, err.in2)}"
  
  // setHeadline - show tourney name and optional competition name if selected
  def setHeadline(): Unit = setHeadline(App.getTourneyName, App.getCompName(AppEnv.getCoId))
  def setHeadline(content: String = "", optContent: String =  ""): Unit = {
    if (content != "") {
      setHtml_(s"APP__Headline__Content", s"<strong>${content}</strong>" + { if (optContent != "") s"[${optContent}]" else "" })
    } else {
      setHtml_("APP__Headline__Content", "")
    }
  }
    
  
  /** resetView
   * 
   */
  def resetView() = {
    setHeader()
    setHeadline()
    ctrlSidebar(AppEnv.status)
    AppEnv.setHistory("HomeMain","","")
  } 


  def showResult(visi: Boolean, header: String="", body: String="", alertTyp: String="success") = {
    if (visi) {
      addClass_("APP__Result", "alert", s"alert-${alertTyp}", "alert-dismissible", "fade", "show")
      setHtml_("APP__Result__Head", header)
      setHtml_("APP__Result__Body", body)
    } 
    setVisible_("APP__Result", visi)
  }  

    /** setHeader
   *  
   */ 
  def setHeader(): Unit = {
    val login = AppEnv.getOrgId > 0
    //debug("setHeader", s"runModeLocal: ${AppEnv.isRunModeLocal} login: ${login}")
    if (AppEnv.getOrganizer != "") { 
      setHtml_("APP__Title", getMsg_("app.header.title.club", AppEnv.getOrganizer))
    } else {
      setHtml_("APP__Title", getMsg_("app.header.title",""))
    }  
    
    setVisible_("APP__Download", !AppEnv.isRunModeLocal)
    setVisible_("APP__Register", !AppEnv.isRunModeLocal & !login)

    setVisible_("APP__Logout", login)
    setVisible_("APP__Configuration", login)
    setVisible_("APP__User", login)
    setVisible_("APP__Club", login)
    setVisible_("APP__Login", !login)
    

    setHtml_("APP__UserName", if (AppEnv.getFullName != "") AppEnv.getFullName else AppEnv.getEmail)
    setHtml_("APP__ClubName", AppEnv.getOrganizer)
    setVisible_("APP__UserName", login)
    setVisible_("APP__ClubName", login)
  }

  // setFooter
  def setFooter(author: String="", changeDate: String=""): Unit = {
    if (author == "") {
      setVisible_("APP__Footer_Content_MetaData", false)
    } else {
      setVisible_("APP__Footer_Content_MetaData", true)
      setHtml_("APP__Footer_Content_Author", AppEnv.getMessage("app.footer.author", author))
      setHtml_("APP__Footer_Content_ChangeDate", AppEnv.getMessage("app.footer.changedate", changeDate))
    }
  }

  // ctrlSidebar - initialize sidebar for normal user, organizer or admin
  def ctrlSidebar(status: AppStatus): Unit = {

    val ucName     = status.ucName
    val validToId  = (status.toId > 0)

    val validOrgId = (AppEnv.getOrgId > 0)
    val admin      = AppEnv.isAdmin

    Helper.debug("ctrlSidebar", s"ucName: ${ucName} toId: ${status.toId}")

    // show normal entries (not for organizer)
    setVisibleByAttr_("sbentry", "HomeSearch", !validOrgId)
    setVisibleByAttr_("sbentry", "InfoDisabled", false)
    setVisibleByAttr_("sbentry", "InfoEnabled",  !validOrgId & validToId )

    // special entries for organizer
    setVisibleByAttr_("sbentry", "OrganizeTourney", validOrgId)
    setVisibleByAttr_("sbentry", "OrganizeCompetition", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizePlayer", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizePlayfield", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizeCertificate", validOrgId & validToId)
    setVisibleByAttr_("sbentry", "OrganizeReport", validOrgId & validToId)

    setVisibleByAttr_("sbentry","Admin", admin)
    if (ucName != "" & ucName != "HomeMain") {
      try {
        val elem = document.querySelectorAll(s"[data-sbentry='${ucName}']").head.asInstanceOf[HTMLElement].parentElement
        if (elem!=null) elem.classList.remove("collapse")
        document.querySelectorAll(s"[data-sbtext]").map(_.asInstanceOf[HTMLElement].classList.remove("text-light"))
        document.querySelectorAll(s"[data-sbtext='${ucName}']").map(_.asInstanceOf[HTMLElement].classList.add("text-light"))
      } catch { case _: Throwable => Helper.warn("setSidebar", s"ucName: ${ucName}")}
    } 
    ()
  }

}  