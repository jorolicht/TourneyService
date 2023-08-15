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


import shared.model._
import scalajs.usecase.component.BasicHtml._
import scalajs._
import shared.utils._

trait WrapperSvc {

  // ***
  //  CALL Wrapper
  // ***

  def enc(uri: String) = URIUtils.encodeURIComponent(uri.replace(",","_~2C~_").replace("=","_~3D~_").replace("&","_~4F~_")) 

  /**
    * Wrapper for get requests returning a RFuncRes
    *
    * Returns either an error or an simple result.There is a 
    * fix url path with tourney id additional parameters are escaped
    */


  /**
   * Wrapper for get requests returning a String
   *
   * Returns either an error or a string in json-format. There is a 
   * fix url path with tourney id and additional parameters which are escaped
   */
  def getAction(cmd: String, toId: Long, params: String=""): Future[Either[Error, String]] = {
    val papas = if (params=="") s"toId=${toId}&cmd=${cmd}" else s"toId=${toId}&cmd=${cmd}&${params}"
    getJson(s"/service/getAction", papas)
  }
  
  /** getJson - basic wrapper for get requests 
   *            All params are escaped through enc and version id is added
   *            Params added to route
   *
   * @return either an error or a string in json-format
   */
  def getJson(route: String, params: String="") : Future[Either[Error, String]] = {
    Ajax.get(genPath(route, params)).map(_.responseText)
      .map(content => Right(content))
      .recover({
        case dom.ext.AjaxException(req) => Left(Error.decodeWithDefault(Error("err0000.communication.error"), req.responseText, s"${route} / ${params}/ ${req.statusText} / ${req.responseText}", "getJson")) 
        case _: Throwable               => Left(Error.decodeWithDefault(Error("err0000.communication.error"), "", s"${genPath(route,params)} / request status and text unknown", "getJson")) 
    })
  }


  /**
   * Wrapper for post requests
   *
   * Returns either an error or an simple result string.There is a 
   * fix url path with tourney id and trigger as URL parameter
   */
  def postAction(cmd: String, toId: Long, params: String, data: String="", trigger: Boolean=false,
                      contType: String = "text/plain; charset=utf-8"): Future[Either[Error, String]] = {
    val papas = s"toId=${toId}&cmd=${cmd}&trigger=${trigger}&caId=${AppEnv.callerId.toString}" + (if (params!="") s"&${params}" else "")
    postJson("/service/postAction", papas, data, contType)
  }


  /** postJson - basic routine for a POST request 
   *
   */ 
  def postJson(route: String, params: String, data: String="", contType: String = "text/plain; charset=utf-8"): Future[Either[Error, String]] = {
    AppEnv.info("postJson", s"route: ${route}  params: ${params} \n data: ${data} \n Csrf-Token: ${AppEnv.getCsrf}")
    Ajax.post(genPath(route,params), data, headers = Map("Content-Type"->s"${contType}", "Csrf-Token" -> AppEnv.getCsrf))
      .map(_.responseText).map(content => Right(content))
      .recover({
        // Recover from a failed error code into a successful future
        case dom.ext.AjaxException(req) => Left(Error.decodeWithDefault(Error("err0000.communication.error"), req.responseText, s"${route} / ${params}/ ${req.statusText} / ${req.responseText}", "postJson"))  
        case _: Throwable               => Left(Error.decodeWithDefault(Error("err0000.communication.error"), "", s"${route} / ${params}/ request status and text unknown", "postJson"))    
      })
  }


  def postForm(route: String, params: String, formData: dom.FormData): Future[Either[Error, String]] = {
    Ajax.post(genPath(route,params), formData, headers = Map("Csrf-Token" -> AppEnv.getCsrf))
      .map(_.responseText).map(content => Right(content))
      .recover({
        // Recover from a failed error code into a successful future
        case dom.ext.AjaxException(req) => Left(Error.decodeWithDefault(Error("err0000.communication.error"), req.responseText, s"${route} / ${req.statusText} / ${req.responseText}", "postForm"))
        case _: Throwable               => Left(Error.decodeWithDefault(Error("err0000.communication.error"), "", s"${route} / request status and text unknown", "postForm"))
      })
  }

  def genPath(route: String, params: String): String = {
    val papas = if (params=="") s"version=${AppEnv.getVersion()}" else s"version=${AppEnv.getVersion()}&${params}"
    s"${route}?params=${enc(papas)}"
  } 

  def getHdrResponse(headers: String): Map[String, String] = {
    headers.split('\n').map( _.split(':') match { case Array(x,y) => x.toLowerCase.trim -> y.trim }).toMap
  }

  def getHdrParam(header: String): Map[String, String] = {
    header.split(';').map( _.split('=') match { case Array(x,y) => x.trim -> y.trim; case Array(x) => x.trim -> "" } ).toMap
  }  


}  