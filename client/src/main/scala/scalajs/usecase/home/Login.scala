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


@JSExportTopLevel("HomeLogin")
object HomeLogin extends UseCase("HomeLogin") with AuthenticateSvc 
{
  val ctx = LoginData.load

  override def sidebar: Boolean = false

  /** render
    * 
    * @param ucParam
    * @param ucInfo
    * @param reload
    */  
  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = {
    ucParam match {
      case "logout" => {
        authLogout(false).map {
          case Left(err)  => error("render", s"logout: ${err}") 
          case Right(res) => {
            AppEnv.resetContext()
            AppEnv.setHistory("HomeMain","","")
            App.tourney.setCurCoId(0)
            App.resetLocalTourney()
            setHeader()
            AppEnv.ctrlSidebar("HomeMain", App.tourney.id)
            setMainContent(clientviews.home.html.Bye(getMsg("bye")).toString) 
          }
        }
      }

      case "login"          => setMainContent(clientviews.home.html.Login(ctx).toString)
      case _                => setMainContent(clientviews.home.html.Login(ctx).toString)
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
    import shapeless._ 
    import shapeless.syntax.sized._ 
    import scalajs.usecase.dialog._ 
    import shared.utils.Routines._
    import scalajs.usecase.component.{InputCtrl => IC}

    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      
      case "ReqAuthenticate" => {
        import shared.utils.Constants._        
    
        // check input values by mapping to tuple
        val (user, password, valid) = List(IC.getText("user", true, 8), IC.getPassword("password")).flatten.sized(2).map(_.tupled) match {
          case Some(x) => (x._1, x._2, true)
          case None    => ("", "", false)
        } 

        if (valid) {
          val (email,licCode) = if (validEmail(user)) (user, "") else ("", user)          
          debug("actionEvent", s"ReqAuthenticate(${email}/${licCode}) password: ${password}")

          authBasic(email, licCode, password).map {
            case Left(err)  => {
              if (err.msgCode == "err0004.login.blocked" ) setDisabled("BtnReqAuthenticate", true)
              DlgInfo.show(getMsg("dlg.hdg"), getError(err), "danger")
            }
            case Right(sessionCtx) => {
              //debug(s"autBasic OK: email: ${AppEnv.getEmail} organizer: ${AppEnv.getOrganizer}")
              AppEnv.initContext(sessionCtx)
              App.execUseCase("HomeMain", "WelcomeOrganizer", "")
            }  
          }
        }
      }

      case "ReqNewPassword"    => {
        val user = getInput(gUE("user"))
        val (email, licCode) = if (validEmail(user)) (user, "") else ("", user) 
        authReset(email, licCode).map {
          case Left(err)  => DlgInfo.show(getMsg("dlg.title.err.reqPw"), getError(err), "danger")
          case Right(res) => DlgInfo.show(getMsg("dlg.title.ok.reqPw"), getMsg("dlg.body.ok.reqPw", res._1), "success")
        }

      }

      case _                 => warn(s"actionEvent", s"invalid key: ${key}")
    }
  }  


}


/** 
  * Context for usecase login
  *
  * @param user
  *
  */
case class LoginData(user: String) 

object LoginData{
  implicit val rw: RW[LoginData] = macroRW

  def load()(implicit ucp: UseCaseParam): ujson.Value = {
    try ujson.read(dom.window.localStorage.getItem(s"AppEnv.${ucp.objName}"))
    catch { case _: Throwable => ujson.read(write(LoginData(""))) }
  }

  def save(jsonVal: ujson.Value)(implicit ucp: UseCaseParam): Unit = {
    try dom.window.localStorage.setItem(s"AppEnv.${ucp.objName}", jsonVal.toString)
    catch { case _: Throwable => () }
  }

  def set(jsonVal: ujson.Value, key: String, keyValue: String)(implicit ucp: UseCaseParam) = {
    jsonVal(key) = keyValue; save(jsonVal)
  }
  
  def set(jsonVal: ujson.Value, key: String, keyValue: Boolean)(implicit ucp: UseCaseParam) = {
    jsonVal(key) = keyValue; save(jsonVal)
  }

  def set(jsonVal: ujson.Value, key: String, keyValue: Int)(implicit ucp: UseCaseParam) = {
    jsonVal(key) = keyValue; save(jsonVal)
  }

  def set(jsonVal: ujson.Value, key: String, keyValue: Double)(implicit ucp: UseCaseParam) = {
    jsonVal(key) = keyValue; save(jsonVal)
  }
}
