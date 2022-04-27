package scalajs.service

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{ Either, Success, Failure}

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.concurrent.duration._

import scala.scalajs._
import scala.scalajs.js.URIUtils

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

import shared.model.tabletennis.ResultEntry
import shared.utils._
import shared.model._
import scalajs.usecase.Helper
import scalajs._


@js.native
@js.annotation.JSGlobal
object TrnyJS extends js.Object {
  def crc32(message: String): String = js.native
  def crc32hex(message: String): String = js.native
}

trait AuthenticateSvc extends WrapperSvc {

  def genHashPlayer(pl: Player): String = {
    pl.lastname.substring(0,0) + pl.firstname.substring(0,0) + TrnyJS.crc32hex(s"${pl.lastname}${pl.firstname}${pl.getBYearStr}${pl.clubName}")
  }

  /** authBasicContext via email or license code
    * 
    * @param email
    * @param licCode
    * @param password
    * @return
    */ 
  def authBasicContext(email: String, licCode: String, password: String): Future[Either[Error, Boolean]] = {
    import org.encoding.Base64._
    
    val route = "/authenticate/basic"
    val params = s"version=${AppEnv.getVersion()}&email=${email}&licCode=${licCode}"
    val path = s"${route}?params=${enc(params)}"
    val authCode = "Basic " + password.getBytes.toBase64

    Ajax.get(path, headers = Map("Authorization"-> authCode)).map(_.responseText).map(context => {
      Right(AppEnv.initContext(context))
    }).recover({
      // Recover from a failed error code into a successful future
      case dom.ext.AjaxException(req) => Left( Error.decode(req.responseText, s"text: ${req.responseText.take(40)} path: ${path}", "authBasic") )
      case _: Throwable               => Left( Error("err0003.ajax.authBasic", "noRespons", "noStatus") )
    }) 
  }


  /** authBasic via email or license code
    * 
    * @param email
    * @param licCode
    * @param password
    * @return
    */ 
  def authBasic(email: String, licCode: String, password: String): Future[Either[Error, String]] = {
    import org.encoding.Base64._
    
    val route = "/authenticate/basic"
    val params = s"version=${AppEnv.getVersion()}&email=${email}&licCode=${licCode}"
    val path = s"${route}?params=${enc(params)}"
    val authCode = "Basic " + password.getBytes.toBase64

    Ajax.get(path, headers = Map("Authorization"-> authCode)).map(_.responseText).map(content => {
      Right(content)
    }).recover({
      // Recover from a failed error code into a successful future
      case dom.ext.AjaxException(req) => Left( Error.decode(req.responseText, s"text: ${req.responseText.take(40)} path: ${path}", "authBasic") )
      case _: Throwable               => Left( Error("err0003.ajax.authBasic", "noRespons", "noStatus") )
    }) 
  }


  /** authLogout 
    * 
    * @return either error or true/false
    */ 
  def authLogout(redirect: Boolean = false): Future[Either[Error, Boolean]] = {
    val path = s"/authenticate/logout?redirect=${redirect.toString}"
    Ajax.get(path).map(_.responseText)
      .map(content => Return.decode2Boolean(content))
      .recover({
        case dom.ext.AjaxException(req) => Left( Error.decode(req.responseText, s"text: ${req.responseText.take(40)} path: ${path}", "authLogout") )
        case _: Throwable               => Left( Error("err0091.ajax.logout","noResponse", "noStatus") )   
      })
  }


 /** authReset 
    * 
    * @return either error or true/false
    */ 
  def authReset(email: String, licCode: String = "", withPW: Boolean = false ): Future[Either[Error, String]] = {
    val route = "/authenticate/reset"
    val params = s"version=${AppEnv.getVersion()}&email=${email}&licCode=${licCode}&withPW=${withPW}"
    val path = s"${route}?params=${enc(params)}"

    Ajax.get(path).map(_.responseText).map(content => {
      Return.decode2String(content)
    }).recover({
      // Recover from a failed error code into a successful future
      case dom.ext.AjaxException(req) => Left( Error.decode(req.responseText, s"text: ${req.responseText.take(40)} path: ${path}", "authReset") )
      case _: Throwable               => Left( Error("err0002.ajax.reset","noResponse", "noStatus") ) 
    })
  }
        

  /** authChange change password
    * 
    * @param email
    * @param password
    * @return
    */ 
  def authChange(email: String, oldPassword: String, newPassword: String): Future[Either[Error, String]] = {
    import org.encoding.Base64._
    val params = s"email=${email}&pw2check=${oldPassword.getBytes.toBase64}"
    postJson("/authenticate/change", params, newPassword.getBytes.toBase64)
  } 

 /** authUpdate update contact data (email, name, address)
    * 
    * @param email
    * @param name
    * @param address
    * @param password
    * @return future either Remote Function Error or Boolean
    */ 
  def authUpdate(email: String, name: String, address: Address, pw2check: String): Future[Either[Error, Boolean]] = {
    import org.encoding.Base64._
    val params = s"email=${email}&name=${name}&pw2check=${pw2check.getBytes.toBase64}"
    postJson("/authenticate/update", params, address.encode).map {
      case Left(err)     => Left(err)
      case Right(result) => Return.decode2Boolean(result)
    }
  }   

}  