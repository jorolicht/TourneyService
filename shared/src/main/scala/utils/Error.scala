package shared.utils

import upickle.default

case class Error(msgCode:String, var in1:String="", var in2:String="", var callStack: String="") {
  def equal2Code(code: String): Boolean = { this.msgCode == code }
  def is(code: String): Boolean = { this.msgCode == code }
  def encode = s"${msgCode}^${in1}^${in2}^${callStack}^_"
  def add(func: String): Error = { callStack = s"${func}:${callStack}"; this} 
  def isDummy = (msgCode == "") 
}

object Error {
  def apply[T](msgCode: String, in: T) = new Error(msgCode, in.toString(), "", "")
  def apply[T,U](msgCode: String, in1: T, in2: U) = new Error(msgCode, in1.toString(), in2.toString(), "")
  def apply[T,U](msgCode: String, in1: T, in2: U, callStack: String) = new Error(msgCode, in1.toString(), in2.toString(), callStack)

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
