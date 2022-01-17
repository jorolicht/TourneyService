package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.yamlijs.YAML                  // facade for yaml
import org.highlight._                   // highlight.org

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext._

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.annotation.varargs
import scala.util.{Success, Failure }
//import scala.util.matching

import shared.model._
import shared.utils._

import scalajs.usecase.component._
import scalajs.service._
import scalajs.AppEnv

@JSExportTopLevel("HomeRegister")
object HomeRegister extends UseCase("HomeRegister") 
   with LicenseSvc with AppHelperSvc
{
  // val ctx = ujson.read(write(HomeRegisterCtx("","Otto", "", "robert.licht@icloud.com",57)))
  var ctx = RegisterData.load

  override def sidebar: Boolean = false

  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = {
    val date = new scala.scalajs.js.Date()
    
    debug("render", s"ucParam: ${ucParam} ucInfo: ${ucInfo}  state: ${ctx("state").str} time: ${ctx("time")}  country: ${ctx("country").str}")

    // overwrite data 
    if (ucInfo == "INIT") ctx = RegisterData.reset(date.getMilliseconds.toInt)
    RegisterData.set(ctx, "full", (ucParam == "full"))
    
    setMainContent(clientviews.home.html.Register(ctx).toString)
    setStartView(ctx)
  }

  
  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    debug("actionEvent", s"key: ${key} event: ${event.`type`}")
    key match {
      case "codeResend" => {
        showHelp("code", false) 
        sendLicenseVerifyCode(ctx("time").num.toInt, ctx("email").str, s"${ctx("name").str}")
      }

      case "FreeRequest" | "FullRequest" | "FreeBack" | "FreeConfirm" | "FullBack" | "FullConfirm"  => {
        execRequest(key)
      }
      case "FreeSelect"  => { setHistoryVersion(false); RegisterData.set(ctx, "full", false); setStartView(ctx) }
      case "FullSelect"  => { setHistoryVersion(true); RegisterData.set(ctx, "full", true);  setStartView(ctx) }

      case _          => { debug("actionEvent(error)", s"unknown key: ${key} event: ${event.`type`}") }
    }
  }


  /** execRequest - click on a request button
    * 
    * @param request
    */
  def execRequest(request: String): Unit = {
    import org.encoding.Base64._
    import cats.data.EitherT
    import cats.implicits._    
    import shared.model.Address
    debug("execRequest", s"request: ${request}  state: ${ctx("state")}")
    val event    = RegisterEvent(request)
    val curState = RegisterState(ctx("state").str)

    validateState(curState, event, ctx).map { valid => {
      debug("execRequest", s"state: (${request}) ${curState.toString} valid is ${valid}")
      val newState = nextState(curState, event)
      if (valid & newState != curState) {
        
        RegisterData.set(ctx, "state", newState.toString)

        debug("execRequest", s"state: (${request}) ${curState.toString} -> ${newState.toString}")
        // set new view - hide all - show only specific part
        setVisibleByName("view", false)
        setHistoryState(newState, ctx)

        newState match {
          case RegisterState.Start        => {
            setVisibleByAttr("view", "start", true)
            setStartView(ctx) 
          }

          case RegisterState.FreeConfirm  => {
            setHtml("FreeConfirm", clientviews.home.register.html.FreeConfirm(ctx).toString )
            setVisibleByAttr("view", "free-confirm", true)
            sendLicenseVerifyCode(ctx("time").num.toInt, ctx("email").str, s"${ctx("name").str}") 
          }

          case RegisterState.FreeFinish   =>  {  
            val licReq = LicRequest(ctx("name").str, ctx("email").str, Address("","","","","").stringify,"",ctx("clubname").str, false, false )
            
            // Process Request:                        
            // request a license, generate an invoice and send it to the user
            setHtml("Processing", clientviews.component.html.Spinner(BasicHtml.getMsg_("msg.processing","")).toString)
            setVisibleByAttr("view", "processing", true)
            val result = (for {
              licIdCpn <- EitherT(requestLicense(licReq))
              result   <- EitherT(sendLicense(licIdCpn._2, ""))
            } yield { (licIdCpn._1, licIdCpn._3, result) }).value.map {
              case Left(err)   => {
                setVisibleByAttr("view", "processing", false)
                setVisibleByAttr("view", "error", true)
                setHtml("ErrorMsg", getError(err))
              }   
              case Right(res)  => {
                setVisibleByAttr("view", "processing", false)
                setVisibleByAttr("view", "free-finish", true); 
                setHtml("FreeLicenseCode", res._1)
                RegisterData.reset()
              }
            }            
          }

          case RegisterState.FullConfirm  => {
            setHtml("FullConfirm", clientviews.home.register.html.FullConfirm(ctx).toString )
            setVisibleByAttr("view", "full-confirm", true)
            sendLicenseVerifyCode(ctx("time").num.toInt, ctx("email").str, s"${ctx("name").str}") 
          }

          case RegisterState.FullFinish   => {
            val addr = Address("", ctx("country").str,ctx("zip").str, ctx("city").str, ctx("street").str)
            val name = ctx("name").str
            val (lastname, firstname) = name.split(",") match { case Array(a,b) => (a.trim, b.trim); case _ => (name,"") }  
            val contact = Contact(lastname, firstname, "", ctx("email").str)

            val pw = InputCtrl.getPassword("password").getOrElse("")
            if (pw == "") error("execRequest", s"password not set")

            val licReq = LicRequest(ctx("name").str, ctx("email").str, addr.stringify, pw.getBytes.toBase64, ctx("clubname").str, false, false)
            // Process Request:                        
            // request a license, generate an invoice and send it to the user
            setHtml("Processing", clientviews.component.html.Spinner(BasicHtml.getMsg_("msg.processing","")).toString)
            setVisibleByAttr("view", "processing", true)

            val result = (for {
              licIdCpn <- EitherT(requestLicense(licReq))
              invoice  <- EitherT(genInvoice(contact, addr, licIdCpn._3, licIdCpn._2))
              result   <- EitherT(sendLicense(licIdCpn._2, invoice))
            } yield { (licIdCpn._1, licIdCpn._3, invoice, result) }).value.map {
              case Left(err)   => {
                setVisibleByAttr("view", "processing", false)
                setVisibleByAttr("view", "error", true) 
                setHtml("ErrorMsg", getError(err))
              }  
              case Right(res)  => {
                setVisibleByAttr("view", "processing", false)
                setVisibleByAttr("view", "error", false)
                setHtml("FullFinish", clientviews.home.register.html.FullFinish(res._1, res._2, res._3).toString)
                setVisibleByAttr("view", "full-finish", true)
                RegisterData.reset()
              }
            }
          }           
          case _                          => setVisibleByAttr("view", "error", true) 
        }
      }  
    }}
  }


  /** nextState
    *  
    * @param event
    * @param state
    * @return new state
    */
  def nextState(state: RegisterState.Value, event: RegisterEvent.Value): RegisterState.Value = {
    state match {
      case RegisterState.Start         => event match {
        case RegisterEvent.FreeRequest => RegisterState.FreeConfirm
        case RegisterEvent.FullRequest => RegisterState.FullConfirm
        case _                         => RegisterState.Unknown
      }

      case RegisterState.FreeConfirm   => event match {
        case RegisterEvent.FreeBack    => RegisterState.Start
        case RegisterEvent.FreeConfirm => RegisterState.FreeFinish
        case _                         => RegisterState.Unknown
      }

      case RegisterState.FullConfirm   => event match {
        case RegisterEvent.FullBack    => RegisterState.Start
        case RegisterEvent.FullConfirm => RegisterState.FullFinish
        case _                         => RegisterState.Unknown
      }

      case _                           => RegisterState.Unknown
    }    
  }

  /*
  **
  ** VALIDATE ROUTINES
  **
  */

  /** validate the (current) state
    * 
    * @param state
    * @param ctx
    * @return true if all inputs are valid, false otherwise
    */ 
  def validateState(state: RegisterState.Value, event: RegisterEvent.Value, ctx: ujson.Value): Future[Boolean]= {
    state match {
      case RegisterState.Start => 
        event match {
          case RegisterEvent.FreeRequest => validateFreeRequest(ctx)
          case RegisterEvent.FullRequest => validateFullRequest(ctx) 
          case _                         => Future(false)
        }

      case RegisterState.FreeConfirm => 
        if (event == RegisterEvent.FullBack | event == RegisterEvent.FreeBack) {
          Future(true)
        } else {
          val valid = chkCode(ctx, getInput("code", 0))
          showHelp("code", !valid)
          // debug("validateState", s"Confirm -> CodeInput ${valid}")
          Future(valid)
        }
      case RegisterState.FullConfirm => 
        if (event == RegisterEvent.FullBack | event == RegisterEvent.FreeBack) {
          Future(true)
        } else {
          val valid1 = chkCode(ctx, getInput("code", 0))
          showHelp("code", !valid1)

          val valid2 = getInput("withdrawal", false) & getInput("gtc", false)
          showHelp("ConfirmConditions", !valid2)          

          // debug("validateState", s"Confirm -> CodeInput ${valid}")
          Future(valid1 & valid2)
        }
      case _ => Future(false)
    }
  }

  def validateFreeRequest(ctx: ujson.Value): Future[Boolean] = {
    import shapeless._ 
    import shapeless.syntax.sized._
    import scalajs.usecase.component.InputCtrl._ 

    val valid = List(getName("name"), getClub("clubname"), getEmail("email")).flatten.sized(3).map(_.tupled) match {
      case Some(x) => ctx("name") = x._1;  ctx("clubname") = x._2; ctx("email") = x._3; RegisterData.save(ctx); true
      case None    => false
    } 
    if (!valid) Future(false) else validateClub(ctx)
  }  

  
  def validateFullRequest(ctx: ujson.Value): Future[Boolean] = {
    import shapeless._ 
    import shapeless.syntax.sized._    
    import scalajs.usecase.component.{InputCtrl => IC}
    
    // check input values by mapping to tuple
    val valid = List(
      IC.getName("name"),            IC.getClub("clubname"),     IC.getEmail("email"),
      IC.getText("street", true, 2), IC.getText("zip", true, 2),
      IC.getText("city", true, 2),   IC.getCountry("country"), 
      IC.getPassword("password")
    ).flatten.sized(8).map(_.tupled) match {
      case Some(x) => {
        ctx("name")   = x._1; ctx("clubname") = x._2; ctx("email") = x._3
        ctx("street") = x._4; ctx("zip")      = x._5 
        ctx("city")   = x._6; ctx("country")  = x._7
        RegisterData.save(ctx); true
      }  
      case None    => false
    }
    if (!valid) Future(false) else validateClub(ctx)
  }  

  def validateClub(ctx: ujson.Value):  Future[Boolean] =
    licenseAvailable(ctx("clubname").str).map { 
      case Left(err)         => {
        error("validateClub", getError(err))
        showHelpTxt("clubname", getError(err), true)
        false
      }  
      case Right(available)  => {
        debug("validateClub", "ok")
        showHelpMsg("clubname", "clubname.inuse", !available)
        available
      }  
    }

  /**
    * 
    *
    * @param ctx
    */

  def setStartView(ctx: ujson.Value): Unit = {
    val full = ctx("full").bool
    
    debug("setStartView", s"fullview: ${full}")

    ctx("state").str match {
      case "FreeConfirm" => {
        setVisibleByName("view", false)
        setHtml("FreeConfirm", clientviews.home.register.html.FreeConfirm(ctx).toString )
        setVisibleByAttr("view", "free-confirm", true)
      }
      case "FullConfirm" => {
        setVisibleByName("view", false)
        setHtml("FullConfirm", clientviews.home.register.html.FullConfirm(ctx).toString )
        setVisibleByAttr("view", "full-confirm", true)
      }

      case _ => {
        if (full) {
          removeClass("LicenseFull1","text-muted");  removeClass("LicenseFullX", "text-muted")
          removeClass("LicenseFull1","bg-light");    removeClass("LicenseFullX", "bg-light")
          addClass("LicenseFree1","text-muted");     addClass("LicenseFreeX","text-muted")
          addClass("LicenseFree1","bg-light");       addClass("LicenseFreeX","bg-light")
        } else { 
          addClass("LicenseFull1","bg-light");       addClass("LicenseFullX","bg-light")
          addClass("LicenseFull1","text-muted");     addClass("LicenseFullX","text-muted")
          removeClass("LicenseFree1","bg-light");    removeClass("LicenseFreeX","bg-light")
          removeClass("LicenseFree1","text-muted");  removeClass("LicenseFreeX","text-muted")
        }

        setRadioBtn("FullSelect", full)
        setRadioBtn("FreeSelect", !full)
        setVisibleByAttr("view", "full-start", full)
        setVisible("BtnFullRequest", full)
        setVisible("BtnFreeRequest", !full)
      }
    }
  }


  /** chkCode
    * 
    * @param ctx
    * @param code
    * @return
    */
  def chkCode(ctx: ujson.Value, code: Int): Boolean = {
    val chkCode = (ctx("time").num.toInt * 19) % 10000
    debug("chkCode", s"ctx: ${ctx("time").num.toInt} chkcode: ${chkCode} code: ${code}")
    code == chkCode
  }

  def setHistoryVersion(full: Boolean) = AppEnv.setHistory("HomeRegister", if (full) "full" else "" , "Start")
  
  def setHistoryState(state: RegisterState.Value, ctx: ujson.Value) = {
    val ucParam = if (ctx("full").bool) "full" else ""
    state match {
      case RegisterState.FreeConfirm => AppEnv.setHistory("HomeRegister", ucParam, "FreeConfirm")
      case RegisterState.FullConfirm => AppEnv.setHistory("HomeRegister", ucParam, "FullConfirm") 
      case _                         => AppEnv.setHistory("HomeRegister", ucParam, "Start") 
    }
  }

}

/*
**
** CONTEXT for Usecase Register
**
*/

/** 
  * Context for usecase register
  *
  * @param lastname
  * @param firstname
  * @param clubname
  * @param email
  * @param street
  * @param zip
  * @param city
  * @param country
  * @param acceptConditions
  * @param full
  * @param state
  */
case class RegisterData(name: String, clubname: String, email: String, 
                       street: String, zip: String, city: String, country: String,
                       acceptConditions: Boolean, full: Boolean, state: String, time: Int) 

object RegisterData{
  implicit val rw: RW[RegisterData] = macroRW

  def reset(time: Int = 0)(implicit ucp: UseCaseParam): ujson.Value = {
    ujson.read(write(RegisterData("", "", "", "", "", "", "", false, false, "Start", time))) 
  }

  def load()(implicit ucp: UseCaseParam): ujson.Value = {
    try ujson.read(dom.window.localStorage.getItem(s"AppEnv.${ucp.objName}"))
    catch { case _: Throwable => reset() }
  }

  def save(jsonVal: ujson.Value)(implicit ucp: UseCaseParam): Unit = {
    try dom.window.localStorage.setItem(s"AppEnv.${ucp.objName}", jsonVal.toString)
    catch { case _: Throwable => () }
  }

  // def set[A](jsonVal: ujson.Value, key: String, keyValue: A)(implicit ucp: UseCaseParam) = {
  //   jsonVal(key) = keyValue
  //   save(jsonVal)
  // }

  def set(jsonVal: ujson.Value, key: String, keyValue: String)(implicit ucp: UseCaseParam) = { jsonVal(key) = keyValue; save(jsonVal) }
  def set(jsonVal: ujson.Value, key: String, keyValue: Boolean)(implicit ucp: UseCaseParam) = { jsonVal(key) = keyValue; save(jsonVal) }
  def set(jsonVal: ujson.Value, key: String, keyValue: Int)(implicit ucp: UseCaseParam) = { jsonVal(key) = keyValue; save(jsonVal) }
  def set(jsonVal: ujson.Value, key: String, keyValue: Double)(implicit ucp: UseCaseParam) = { jsonVal(key) = keyValue; save(jsonVal) }
}


object RegisterEvent extends Enumeration {
  type RegisterEvent = Value
  val Unknown, FreeRequest, FreeConfirm, FullRequest, FullOrder, FullConfirm, FreeBack, FullBack = Value
  def apply(value: String) : RegisterEvent.Value = {
    value match {
      case "FreeRequest"       => RegisterEvent.FreeRequest
      case "FreeConfirm"       => RegisterEvent.FreeConfirm
      case "FreeBack"          => RegisterEvent.FreeBack
      
      case "FullRequest"       => RegisterEvent.FullRequest
      case "FullConfirm"       => RegisterEvent.FullConfirm
      case "FullBack"          => RegisterEvent.FullBack
      case _                   => RegisterEvent.Unknown
    }
  }
}


object RegisterState extends Enumeration {
  type RegisterState = Value
  val Unknown, Start, FreeConfirm, FreeFinish, FullConfirm, FullFinish = Value
  def apply(initValue: String) : RegisterState.Value = {
    initValue match {
      case "Start"        => RegisterState.Start
      case "FreeConfirm"  => RegisterState.FreeConfirm
      case "FreeFinish"   => RegisterState.FreeFinish
      case "FullConfirm"  => RegisterState.FullConfirm
      case "FullFinish"   => RegisterState.FullFinish
      case _              => RegisterState.Unknown
    }
  }
}