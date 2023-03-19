package scalajs

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Failure}

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import upickle.default._

case class AppStatus(var toId: Long, var ucName: String, var ucParam: String, var ucInfo: String) {
  def save = AppEnv.setLocalStorage("AppEnv.Status", write( (toId, ucName, ucParam, ucInfo) ))
}

object AppStatus {
  def load: AppStatus = {
    val (toId, ucName, ucParam, ucInfo) = {
      try read[(Long,String,String,String)](AppEnv.getLocalStorage("AppEnv.Status"))
      catch { case e: Throwable => AppEnv.setLocalStorage("AppEnv.Status", write((0L,"HomeMain","",""))); (0L,"HomeMain","","") }
    }
    AppStatus(toId, ucName, ucParam, ucInfo)
  } 
}