package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.Routines._
import shared.utils.{ Error, Return }

case class Address(
  var description: String,
  var country:     String,
  var zip:         String,
  var city:        String,
  var street:      String
) {
  def stringify = s"${description}·${country}·${zip}·${city}·${street}·"
  def encode    = s"${description}·${country}·${zip}·${city}·${street}·"
}

object Address {
  implicit def rw: RW[Address] = macroRW 
  def decode(x: String): Either[Error, Address] = {
    try    Right( Address(getMDStr(x,0), getMDStr(x,1), getMDStr(x,2), getMDStr(x,3), getMDStr(x,4)) )
    catch { case _: Throwable => Left(Error("err0043.decode.Address", x)) }
  }
} 