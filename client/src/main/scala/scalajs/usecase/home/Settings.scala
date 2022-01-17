package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils
import scala.scalajs._

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext._

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
//import scala.util.matching

import shared.utils._
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


@JSExportTopLevel("HomeSetting")
object HomeSetting extends UseCase("HomeSetting") 
   with AuthenticateSvc with LicenseSvc with AppHelperSvc
{
  var ctx = SettingData.init()

  /** render
    * 
    * @param ucParam
    * @param ucInfo
    * @param reload
    */  
  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = {
    import shared.model.Address
    getOwnLicense().map {
      case Left(err)      => {
        warn("render", getError(err))
        App.execUseCase("HomeMain", "Error", getError(err))
      }  
      case Right(license) => {
        debug(s"render", s"${license}" )
        ctx("user") = license.name
        ctx("club") = license.club
        ctx("email") = license.email

        // set address values if available
        license.address match {
          case Some(addr) => Address.decode(addr) match {
            case Left(err)      => error("render", "decoding address")
            case Right(address) => {
              ctx("zip")    = address.zip;    ctx("city")    = address.city
              ctx("street") = address.street; ctx("country") = address.country
            }
          }
          case None      => {
              ctx("zip") = "";    ctx("city")    = ""
              ctx("street") = ""; ctx("country") = ""
          }  
        }
        setMainContent(clientviews.home.html.Setting(ctx).toString)
      }
    }
  }


  /** actionEvent
    * 
    * @param key
    * @param elem
    * @param event
   
    */
  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    import scalajs.usecase.dialog._
    import shared.model._

    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {      
      case "ReqPasswordChange" => {
        //import shared.global.constants._

        // check standard input values
        val (email, pw2check, pwNew, valid) = (for {
          pwNew    <- InputCtrl.getPassword("pwNew", "pwRepeat", true)
          pw2check <- InputCtrl.getPassword("pw2Chk4Change", "", true)
          email    <- InputCtrl.getEmail("email", false)
          res      <- Some(true)
        } yield { (email, pw2check, pwNew, res) }).getOrElse(("","","",false))

        if (valid) {
          debug("actionEvent", s"key: ${key} email: ${email}") 
          authChange(email, pw2check, pwNew).map {
            case Left(err)  => {
              //if (err.code == error_login_blocked) setDisabled("BtnReqAuthenticate", true)
              DlgInfo.show(getMsg("pwChange.failure.dlg.hdg"), getError(err), "danger")
            }
            case Right(sessionCtx) => {
              //debug(s"autBasic OK: email: ${AppEnv.getEmail} organizer: ${AppEnv.getOrganizer}")
              DlgInfo.show(getMsg("pwChange.success.dlg.text"), "", "success")
              setInput("pwNew","")
              setInput("pwRepeat","")
              setInput("pw2Chk4Change","")
            }  
          }
        }
      }

      case "ReqContactChange" => {
        // check standard input values
        val (email, name, password, valid) = (for {
          email  <- InputCtrl.getEmail("newEmail", true)
          name   <- InputCtrl.getName("newUser", true)
          pw     <- InputCtrl.getPassword("pwChkContact", "", true)
          res    <- Some(true)
        } yield { (email, name, pw, res) }).getOrElse(("","","",false))

        if (valid) {
          debug("actionEvent", s"key: ${key} email: ${email} name: ${name}") 
          authUpdate(email, name,  Address("", InputCtrl.getCountry("country").getOrElse(""), getInput("zip"), getInput("city"), getInput("street")), password).map { 
            case Left(err)  => {
              DlgInfo.show(getMsg("contactChange.failure.dlg.hdg"), getError(err), "danger")
            }
            case Right(sessionCtx) => {
              DlgInfo.show(getMsg("contactChange.success.dlg.text"), "", "success")
              setInput("pwChkContact","")
              setInput("email", email)
            }  
          }
        }
      }
      case _                 => warn("actionEvent", s"invalid key: ${key}")
    }
  }  

}


/** 
  * Context for user/club settings
  *
  * @param user
  * @param email
  * @param orgDir
  * @param zip
  * @param city
  * @param street
  * @param coutnry
  */
case class SettingData(club: String, user: String, email: String, zip: String, city: String, street: String, country: String)

object SettingData{
  implicit val rw: RW[SettingData] = macroRW

  def init()(implicit ucp: UseCaseParam): ujson.Value = {
    ujson.read(write(SettingData("","","","","","","")))
  }
}