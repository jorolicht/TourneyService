package shared.model

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.model.Competition._
import shared.utils.{ Error, Return }
import shared.utils.Constants._
import shared.utils.Routines._


case class SNO(value: String) {

  // special player identifier
  //val SNO_BYE = "99500" // till 99999
  //val SNO_NN  = "99000" 
  //val SNO_MAX = "99999"

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
  def getInfo(coTyp: CompTyp.Value)(implicit trny: Tourney): (String, String, String, Int) = {
    coTyp match {
       case CompTyp.SINGLE => getSinglePlayer() match {
         case Left(err)       => ("", "", "", 0)
         case Right(p)        => (value, p.getName(), p.clubName, p.getRating)
       }
       case CompTyp.DOUBLE => getDoublePlayers() match {
         case Left(err)       => ("", "", "", 0)
         case Right((p1, p2)) => (value, p1.getDoubleName(p2), p1.getDoubleClub(p2), p1.getDoubleRating(p2))
       }
       case _         => ("", "", "", 0)    
    }
  }

  // getPantEntry returns Participant Entry
  def getPantEntry(coId: Long, place: (Int,Int) = (0,0))(implicit trny: Tourney): PantEntry = {
    trny.comps(coId).typ match {
       case CompTyp.SINGLE => getSinglePlayer() match {
         case Left(err)       => PantEntry(value, "", "", 0, (0,0)) 
         case Right(p)        => PantEntry(value, p.getName(), p.clubName, p.getRating, place)
       }
       case CompTyp.DOUBLE => getDoublePlayers() match {
         case Left(err)       => PantEntry(value, "", "", 0, (0,0)) 
         case Right((p1, p2)) => PantEntry(value, p1.getDoubleName(p2), p1.getDoubleClub(p2), p1.getDoubleRating(p2), place)
       }
       case _         => PantEntry(value, "", "", 0, (0,0))    
    }
  }  


  // getName returns name for all types of participants
  def getName(coTyp: CompTyp.Value, byeName: String="")(implicit trny: Tourney): String  = {
    if (value == "") {
      ""
    } else if (byeName != "" & isBye()) { 
      byeName
    } else {
      coTyp match {
        case CompTyp.SINGLE => getSinglePlayer() match {
          case Left(err)       => ""
          case Right(p)        => p.getName()
        }
        case CompTyp.DOUBLE => getDoublePlayers() match {
          case Left(err)       => ""
          case Right((p1, p2)) => p1.getDoubleName(p2)
        }
        case _         => ""    
      }
    }
  }



  def isBye() = SNO.isBye(value)
  def isNN()  = SNO.isNN(value)
}

object SNO {
  implicit def rw: RW[SNO] = macroRW
  
  val BYE = "99500" 
  val NN  = "99000" 

  def isBye(value: String) = {
    val intVal = value.toIntOption.getOrElse(0)
    (intVal >= 99500 & intVal <= 99999)
  }

  def isNN(value: String)  = (value == "" | value == SNO.NN )

  def bye(no: Int=0) = SNO(s"${99500 + no}")
  def nn()  = SNO(NN)

  def plId(inValue: String): Long = {
    val value = getMDLongArrDef(inValue)
    if (value(0) >= 99500) 0L else value(0)
  }
  
  def valid(inValue: String): Boolean = {
    val value = getMDLongArrDef(inValue)
    (value(0) > 0) && (value(0) < 99500)
  }

  def valid(value: Long): Boolean = {
    (value > 0) && (value < 99500)
  }  

}