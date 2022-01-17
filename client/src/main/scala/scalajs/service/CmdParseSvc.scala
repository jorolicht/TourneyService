package scalajs.service

import scala.concurrent.duration._
import scala.concurrent._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global

import scalajs.{ App, AppEnv }
import scalajs.usecase.Helper

trait CmdParseSvc {

  def xor[T](x: Option[T], y: Option[T]): Boolean = (x, y) match {
    case (Some(_), None) => true
    case (None, Some(_)) => true
    case (None, None)    => false
    case _               => false
  } 

  def and[T](x: Option[T], y: Option[T]): Boolean = (x, y) match {
    case (Some(_), Some(_)) => true
    case _                  => false
  }

  def equals[T](x: Option[T], value: T): Boolean = x match {
    case Some(y)  => y == value
    case _        => false
  }

  def isSet[T](x: Option[T]): Boolean = x match {
    case Some(y)  => true
    case _        => false
  }



  /** getOptArg - looks for option in args, returns option and new args (consuming the option) 
    *             option can be in short or long version
    *             example: command -oargument or --option argument 
    * 
    * @param option
    * @param args
    * @return
    */
  def getOptArg(args: List[String], option: String): (List[String], Option[String]) = {
    var removed: List[String] = List.empty
    var arg: String = ""
    val op1 = s"--${option}"
    val op2 = s"-${option.slice(0,1)}"

    args.sliding(2, 1).toList.map { x => x match {
      case List(x: String, y: String) => {
        if      (x == op1 ) { arg = y; removed = removed ++ List(op1, y) }
        else if (x == op2 ) { arg = y; removed = removed ++ List(op2, y) }
        else if (x.startsWith(op2) & x.length > 2) { arg = x.slice(2,20); removed = removed ++ List(x) }
      }
      case List(x: String)            => {
        if (x.startsWith(op2) & x.length > 2) { arg = x.slice(2,20); removed = removed ++ List(x) }
      } 
      case _ => {}
    }}
    if (arg != "") (args diff removed, Some(arg)) else (args, None)
  }

  def getParam(args: List[String], matchList: List[String] = List.empty): (List[String], Option[String]) = {
    if (args.length>0) {
      if (matchList.isEmpty) { 
        (args.tail, Some(args.head))
      } else {
        if (matchList.contains(args.head)) (args.tail, Some(args.head)) else (args, None)
      }  
    } else {
      (args, None)
    }
  }  

}