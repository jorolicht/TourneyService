package shared.model

import scala.collection.mutable.{ ArrayBuffer, HashMap }

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.{ Error, Return }
import shared.utils.Constants._
import shared.utils.Routines._


sealed trait Placement
case class PlacementPos(pos: Int) extends Placement
case class PlacementRange(from: Int, to: Int) extends Placement

object PlacementPos   { implicit def rw: RW[PlacementPos] = macroRW   }
object PlacementRange { implicit def rw: RW[PlacementRange] = macroRW }

object Placement {
  def encode(place: Placement): String = {
    place match {
      case PlacementPos(pos)       => pos.toString
      case PlacementRange(from,to) => s"${from}·${to}"
    }
  }

  def decode(placeStr: String): Either[Error, Placement] = {
    try {
      val placeArr = getMDIntArr(placeStr)
      placeArr.length match {
        case 1 => if (placeArr(0) > 0) Right(PlacementPos(placeArr(0))) else Left(Error("err0059.decode.Placement", placeStr, "", "Placement.decode"))
        case 2 => if (placeArr(0) > 0 & placeArr(1) > 0) Right(PlacementRange(placeArr(0),placeArr(1))) else Left(Error("err0059.decode.Placement", placeStr, "", "Placement.decode"))
        case _ => Left(Error("err0059.decode.Placement", placeStr, "", "Placement.decode"))
      }  
    } catch { case _: Throwable => Left(Error("err0059.decode.Placement", placeStr, "", "Placement.decode")) }  
  }
}