package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.Routines._
import shared.utils.{ Error, Return }

case class Contact(
  var lastname:  String,
  var firstname: String,
  var phone:     String,
  var email:     String
) {
  def stringify = s"${lastname}·${firstname}·${phone}·${email}·"
  def encode    = s"${lastname}·${firstname}·${phone}·${email}·"
  def getName(fmt: Int=0) = {
    fmt match {
      case 0 => if(firstname!="" & lastname!="") firstname + " " + lastname else firstname + lastname
      case 1 => if(firstname!="" & lastname!="") lastname + ", " + firstname else firstname + lastname
      case _ => if(firstname!="" & lastname!="") lastname + ", " + firstname else firstname + lastname
    }
  }
   
}

object Contact {
  implicit def rw: RW[Contact] = macroRW 
  def decode(x: String): Either[Error, Contact] = {
    try    Right( Contact(getMDStr(x,0), getMDStr(x,1), getMDStr(x,2), getMDStr(x,3)) )
    catch { case _: Throwable => Left(Error("err0042.decode.Contact", x)) }
  }
} 