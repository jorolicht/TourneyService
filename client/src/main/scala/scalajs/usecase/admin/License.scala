package scalajs.usecase.admin

import scala.util.Try

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service client imports
import shared.model._
import shared.utils._

import scalajs.usecase.component._
import scalajs.usecase.dialog._
import scalajs.service._


//***
// LICENSE MANAGMENT
//***

@JSExportTopLevel("AdminLicense")
object AdminLicense extends UseCase("AdminLicense") 
  with TourneySvc with LicenseSvc
{

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    getAllLicense.map { lics =>
      setMainContent(clientviews.admin.html.LicenseTmpl(lics).toString)
    }    
  }


  @JSExport
  // clickUpload - handle click on upload
  def buttonDownload() =  dom.window.open("/admin/exportLic") 

  @JSExport
  def buttonDelete() = {
    val clubId   = Try { $(getIdHa("FormClubId") ).value.asInstanceOf[String].toInt } getOrElse { 0 }
    val clubName = Try { $(getIdHa("FormClub") ).value.asInstanceOf[String] } getOrElse { "unknown Club" }
    if (clubId != 0) {
      DlgBox.showStd(getMsg("delete.header"), getMsg("delete.body", s"${clubName}"), 
                  Seq("cancel","ok"))
        .map { _ match {
          case 2 => deleteLicense(clubId).map { _ =>
            getAllLicense.map { lics =>
              setMainContent(clientviews.admin.html.LicenseTmpl(lics).toString)
              togCollapse("Header","Body")
            }
          }
        }}
    } else {
      DlgBox.showStd(getMsg("delete.header"), getMsg("delete.body.error"), Seq("ok"))
        .map { _ match { case _ => debug("delete", "OK") }}
    }
  }

  @JSExport
  def buttonUpdate() = {
    val clubId   = Try { $(getIdHa("FormClubId") ).value.asInstanceOf[String].toInt } getOrElse { 0 }
    val clubName = Try { $(getIdHa("FormClub") ).value.asInstanceOf[String] } getOrElse { "unknown club" }
    val name     = Try { $(getIdHa("FormName") ).value.asInstanceOf[String] } getOrElse { "unknown name" }
    val email    = Try { $(getIdHa("FormEMail") ).value.asInstanceOf[String] } getOrElse { "unknown email" }
    val updval   = Try { $(getIdHa("FormInfo") ).value.asInstanceOf[String] } getOrElse { "0" }
    val updates  = if (updval == "1") true else false

    if (clubId != 0) {
    //   DlgBox.showStd(getMsg("update.header"), getMsg("update.body", s"${clubName}"), Seq("cancel", "ok"))
    //     .map { _ match {
    //       case 2 => updLicense(clubId, name, email, updates ).map { _ =>
    //         getLicenses.map { lics =>
    //           setMainContent(clientviews.admin.html.LicenseTmpl(lics).toString)
    //           togCollapse("Header","Body")
    //         }
    //       }
    //     }}
    // } else {
    //   DlgBox.showStd(getMsg("update.header"), getMsg("update.body.error"), Seq("ok"))
    //     .map { _ match { case _ => debug("update", "OK") }}
    }
  }


  @JSExport
  def onclickSelect(elem: dom.raw.HTMLInputElement, licIdStr: String) = {
    val licId = licIdStr.toLongOption.getOrElse(0L)

    debug("onclickSelect",s"License Nr: ${licId}")

    $(s"[id^=${getId("License_")}]").removeClass("bg-secondary text-white")
    if (licId != 0) {
      $( ${getIdHa(s"License_${licIdStr}")}).addClass("bg-secondary text-white")
    } 


    if (licId != 0) {
      val idh = getIdHa(s"License_${licIdStr}")
      $(getIdHa("FormClubId")).`val`( $(idh).find("td:eq(0)").text() )
      $(getIdHa("FormClub")).`val`( $(idh).find("td:eq(1)").text() )
      $(getIdHa("FormName")).`val`( $(idh).find("td:eq(2)").text() )
      $(getIdHa("FormEMail")).`val`( $(idh).find("td:eq(3)").text() )
      $(getIdHa("FormOrgDir")).`val`( $(idh).find("td:eq(4)").text() )
      $(getIdHa("FormLicense")).`val`( $(idh).find("td:eq(5)").text() )
      
      if ( $(idh).find("td:eq(7)").text() == "true") {
        $(getIdHa("FormInfo")).`val`("1")
      } else {
        $(getIdHa("FormInfo")).`val`("0") 
      }
    } else {
      $(getIdHa("FormClubId")).`val`("")
      $(getIdHa("FormClub")).`val`("")
      $(getIdHa("FormName")).`val`("")
      $(getIdHa("FormEMail")).`val`("")
      $(getIdHa("FormOrgDir")).`val`("")
      $(getIdHa("FormLicense")).`val`("")
    }
  }

}
 