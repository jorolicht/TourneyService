package shared.utils

import upickle.default

case class Error(msgCode:String, var in1:String="", var in2:String="", var callStack: String="") {
  def equal2Code(code: String): Boolean = { this.msgCode == code }
  def encode = s"${msgCode}^${in1}^${in2}^${callStack}^_"
  def add(func: String): Error = { callStack = s"${func}:${callStack}"; this} 
  def isDummy = (msgCode == "") 
}

object Error {
  def decode(errTx:String, hint: String="", func: String="_"): Error = {
    try   { val err = errTx.split("\\^"); Error(err(0), err(1), err(2), s"${func}:${err(3)}") }
    catch { case _: Throwable => Error("err0001.decode.error", errTx, hint, func) }
  }

  def decodeWithDefault(defErr: Error, errTx: String, hint: String="", func: String="_"): Error = {
    try   { val err = errTx.split("\\^"); Error(err(0), err(1), err(2), s"${func}:${err(3)}") }
    catch { case _: Throwable => { println(s"Decode error ->${errTx}<- failure func: ${func} hint: ${hint} return default"); defErr } }
  }

  def valid(errTx:String, hint: String="", func: String="_"): Boolean = {
    errTx.split("\\^").length == 4
  }
  def dummy = Error("","","","")
}

case class Return[A](value: A) {
  def encode = value match {
    case _: Int     => s"Int:${value.toString}"
    case _: Long    => s"Long:${value.toString}"
    case _: Boolean => s"Boolean:${value.toString}"
    case _: String  => s"String:${value}"
    case _          => s"Unknown:${value}"
  }  
}

object Return {
  def decode2Int(value: String, caller: String=""): Either[Error, Int] = value match {
    case s"Int:$rest"     => try Right(rest.toInt) catch { case _: Throwable => Left(Error("err0066.decode.Int", value, "", s"${caller}:decode2Int")) }  
    case _                => Left(Error("err0066.decode.Int", value, "", s"${caller}:decode2Int"))
  }

  def decode2Long(value: String, caller: String=""): Either[Error, Long] = value match {
    case s"Long:$rest"    => try Right(rest.toLong) catch { case _: Throwable => Left(Error("err0067.decode.Long", value, "", s"${caller}:decode2Long")) }   
    case _                => Left(Error("err0067.decode.Long", value, "", s"${caller}:decode2Long"))
  }

  def decode2Boolean(value: String, caller: String=""): Either[Error, Boolean] = value match {
    case s"Boolean:$rest" => try Right(rest.toBoolean) catch { case _: Throwable => Left(Error("err0068.decode.Boolean", value, "", s"${caller}:decode2Boolean")) }  
    case _                => Left(Error("err0068.decode.Boolean", value, "", s"${caller}:decode2Boolean"))
  }

  def decode2String(value: String, caller: String=""): Either[Error, String] = value match {
    case s"String:$rest"  => try Right(rest) catch { case _: Throwable => Left(Error("err0069.decode.String", value, "", s"${caller}:decode2String")) }  
    case _                => Left(Error("err0069.decode.String", value, "", s"${caller}:decode2String"))
  }

}