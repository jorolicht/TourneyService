package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import shared.utils.{ Error, Return }
import shared.utils.Constants._
import shared.utils.Routines._


 /**
  *  Single/Double/Team to Competition mapping entry
  */
  case class Pant2Comp (
    val sno:       String,         // mapping of player identifier to start numbers = sno1 <MD> sno2 <MD> ...
                                   // player/double or team gets a participant of the competition
                                   // participants are identified by start numbers (sno) 
    val coId:      Long, 
    var ident:     String,
    var placement: String,         // format <position> or <fromPosition>.<toPosition>
    var status:    Int,            // -1 = pending signup acknowledge
                                   //  0 = register/signup
                                   //  1 = participate
                                   //  2 = currently playing
                                   //  3 = finished
     var options:   String = "_"                               
  
  ) {
    def encode = write[Pant2Comp](this)

    def getSignUpDate: Int   = getMDInt(options,0); def setSignUpDate(value: Int)   = { options = setMD(options,value,0) }
    def getSignUpTime: Int   = getMDInt(options,1); def setSignUpTime(value: Int)   = { options = setMD(options,value,1) }
    def getSignUpTyp: String = getMDStr(options,2); def setSignUpTyp(value: String) = { options = setMD(options,value,2) }
    def getTeamName: String = getMDStr(options,3); def setTeamName(value: String) = { options = setMD(options,value,3) }
    def getTeamNr: String = getMDStr(options,4); def setTeamNr(value: String) = { options = setMD(options,value,4) }

    def getPlaceDesc(fun:(String, Seq[String])=>String): String = {
      val placeArr = getMDIntArr(placement)
      placeArr.length match {
        case 1 => if (placeArr(0) > 0) fun("certificate.place.value", Seq(placeArr(0).toString)) else ""
        case 2 => {
          if (placeArr(0) > 0) {
            if (placeArr(0) == placeArr(1)) {
              fun("certificate.place.value", Seq(placeArr(0).toString)) 
            } else {
              fun("certificate.place.range", Seq(placeArr(0).toString, placeArr(1).toString)) 
            } 
          } else ""
        }  
        case _ => ""
      }
    }

    def getPlace(): (Int, Int) = {
      try {
        val placeArr = getMDIntArr(placement)
        placeArr.length match {
          case 1 => (placeArr(0).toInt, placeArr(0).toInt)
          case 2 => (placeArr(0).toInt, placeArr(1).toInt)
          case _ => (0, 0)
        }  
      } catch { case _: Throwable => (0, 0)}   
    }

    def getPlacement(): Either[shared.utils.Error, Placement] = Placement.decode(placement)
    def getPlayerId  = getMDLong(sno, 0)
    def getPlayerId1 = getMDLong(sno, 0)
    def getPlayerId2 = getMDLong(sno, 1)
    def getSingleId  = getMDLong(sno, 0)
    def getDoubleId  = (getMDLong(sno, 0), getMDLong(sno, 1))
  }

  
  object Pant2Comp {
    implicit def rw: RW[Pant2Comp] = macroRW
    def tupled = (this.apply _).tupled
    def dummy() = new Pant2Comp("", 0L, "", "", 0, "_")
    def single(plId: Long, coId: Long, status: Int) = new Pant2Comp(plId.toString, coId, "", "", status, "_") 
    def double(id1: Long, id2: Long, coId: Long, status :Int) = {
      val sno = (id1, id2) match {
        case (x,y) if (x<=0 | y<=0) => ""
        case (x,y) if (x==y)        => ""
        case (x,y) if (x>y)         => y.toString + "·" + x.toString
        case (x,y)                  => x.toString + "·" + y.toString 
      }
      new Pant2Comp(sno, coId, "", "", status, "_")
    }

    def decode(x: String): Either[Error, Pant2Comp] = {
      try Right(read[Pant2Comp](x)) 
      catch { case _: Throwable => Left(Error("err0054.decode.Pant2Comp", x.take(10), "", "Participant.decode")) }
    }

    def decSeq(p2cStr: String): Either[Error, Seq[Pant2Comp]] = {  
      try Right(read[Seq[Pant2Comp]](p2cStr))
      catch { case _: Throwable => Left(Error("err0055.decode.Pant2Comps", p2cStr.take(20),"","Participant.deqSeq")) }
    }
  }
  

// relevant information of an active player/participant within an competition
case class ParticipantEntry(
  var sno:       String,         // start number(s) concatenated string of player identifieres  
  val name:      String,                     
  val club:      String, 
  val rating:    Int,            // eg. ttr for table tennis
  var place:     (Int,Int),      // position after finishing the round (group or ko)
  var occu:      Int = 0,
  var effRating: Int = 0        
)  {
  def getRating = {
    if (rating == 0) "" else rating.toString
  }
}


object ParticipantEntry {
  implicit def rw: RW[ParticipantEntry] = macroRW

  def decode(x: String): Either[Error, ParticipantEntry] = {
    try Right(read[ParticipantEntry](x)) 
    catch { case _: Throwable => Left(Error("err0054.decode.Pant2Comp", x.take(10), "", "Participant.decode")) }
  }

  def bye(name: String="bye") =  ParticipantEntry(SNO.BYE, name, "", 0, (0, 0))

}

// PantSelect - info for participant selection for next round
case class PantSelect(val sno: SNO, val name: String, val info: String, var checked: Boolean, val winner: Boolean=true)