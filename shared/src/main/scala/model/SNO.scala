package shared.model

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.utils.{ Error, Return }
import shared.utils.Constants._
import shared.utils.Routines._


case class SNO(value: String) {

  def getSinglePlayer()(implicit trny: Tourney): Either[Error, Player] = {
    try Right(trny.players(value.toLong))
    catch { case _: Throwable => Left(Error("err0173.trny.getSinglePlayer", value)) }
  }  

  def getDoublePlayers()(implicit trny: Tourney): Either[Error, (Player, Player)] = {
    try {
      val ids = getMDLongArr(value)
      Right( (trny.players(ids(0)), trny.players(ids(1))) )
    }  
    catch { case _: Throwable => Left(Error("err0174.trny.getDoublePlayer", value)) }
  }

  // getInfo returns tuple (SNO.value, Name, Club, TTR)
  def getInfo(coTyp: Int)(implicit trny: Tourney): (String, String, String, Int) = {
    coTyp match {
       case CT_SINGLE => getSinglePlayer() match {
         case Left(err)       => ("", "", "", 0)
         case Right(p)        => (value, p.getName(), p.clubName, p.getRating)
       }
       case CT_DOUBLE => getDoublePlayers() match {
         case Left(err)       => ("", "", "", 0)
         case Right((p1, p2)) => (value, p1.getDoubleName(p2), p1.getDoubleClub(p2), p1.getDoubleRating(p2))
       }
       case _         => ("", "", "", 0)    
    }
  }

  // getName returns name for all types of participants
  def getName(coTyp: Int)(implicit trny: Tourney): String  = {
    coTyp match {
       case CT_SINGLE => getSinglePlayer() match {
         case Left(err)       => ""
         case Right(p)        => p.getName()
       }
       case CT_DOUBLE => getDoublePlayers() match {
         case Left(err)       => ""
         case Right((p1, p2)) => p1.getDoubleName(p2)
       }
       case _         => ""    
    }
  }


}

object SNO {
  implicit def rw: RW[SNO] = macroRW
}