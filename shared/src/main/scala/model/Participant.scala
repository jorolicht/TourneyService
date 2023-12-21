package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }
import shared.utils.{ Error, Return, CSVConverter }
import shared.utils.Routines._

object PantStatus extends Enumeration {

// implicit val pStatusReadWrite: ReadWrite[PantStatus.Value] =
//   readWriter[Int].bimap[PantStatus](x => x.id, PantStatus(_))  

  // participant status
  val UNKN = Value(-99, "UNKN")
  val RJEC = Value(-3,  "RJEC")   // rejected
  val WAIT = Value(-2,  "WAIT")   // waiting list 
  val PEND = Value(-1,  "PEND")   // pending signup confirmation
  val REGI = Value( 0,  "REGI")   // registered  
  val REDY = Value( 1,  "REDY")   // participation confirmed
  val PLAY = Value( 2,  "PLAY")   // currently playing
  val FINI = Value( 3,  "FINI")   // competition finished

  implicit class PantStatusValue(ps: Value) {
    def code       = s"${ps.toString}"
    def msgCode    = s"PantStatus.${ps.toString}"
  }

  def apply(value: String): PantStatus.Value = {
    value match {
      case "RJEC" => PantStatus(-3)
      case "WAIT" => PantStatus(-2)
      case "PEND" => PantStatus(-1)
      case "REGI" => PantStatus(0)
      case "REDY" => PantStatus(1)
      case "PLAY" => PantStatus(2)
      case "FINI" => PantStatus(3)
      case _      => PantStatus(-99)
    }
  } 
}

/**
 *  Single/Double/Team to Competition mapping entry
 */
case class Pant2Comp (
  val sno:       String,         // mapping of player identifier to start numbers = sno1 <MD> sno2 <MD> ...
                                 // player/double or team gets a participant of the competition
                                 // participants are identified by start numbers (sno) 
  val coId:      Long, 
  var ident:     String,
  var placement: String,           // format <position> or <fromPosition>.<toPosition>
  var status:    PantStatus.Value, 
  var options:   String = "_"                               

) {
  def encode = write[Pant2Comp](this)

  def getSignUpDate: Int   = getMDInt(options,0); def setSignUpDate(value: Int)   = { options = setMD(options,value,0) }
  def getSignUpTime: Int   = getMDInt(options,1); def setSignUpTime(value: Int)   = { options = setMD(options,value,1) }
  def getSignUpTyp: String = getMDStr(options,2); def setSignUpTyp(value: String) = { options = setMD(options,value,2) }
  def getTeamName: String  = getMDStr(options,3); def setTeamName(value: String)  = { options = setMD(options,value,3) }
  def getTeamNr: String    = getMDStr(options,4); def setTeamNr(value: String)    = { options = setMD(options,value,4) }

  def getPlaceDesc(fun:(String, Seq[String])=>String): String = {
    val placeArr = getMDIntArr(placement)
    if  ( (placeArr.length==1 && placeArr(0)>0) || (placeArr.length==2 && placeArr(0)>0 && placeArr(0)==placeArr(1)) ) {
      fun("certificate.place.value", Seq(placeArr(0).toString)) 
    } else if (placeArr.length==2 && placeArr(0)>0 && placeArr(0)!=placeArr(1)) {
      fun("certificate.place.range", Seq(placeArr(0).toString, placeArr(1).toString)) 
    } else ""
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
  def getPlayerId3 = getMDLong(sno, 0)
  def getPlayerId2 = getMDLong(sno, 1)
  def getSingleId  = getMDLong(sno, 0)
  def getDoubleId  = (getMDLong(sno, 0), getMDLong(sno, 1))
}


object Pant2Comp {
  implicit val pStatusReadWrite: upickle.default.ReadWriter[PantStatus.Value] =
    upickle.default.readwriter[Int].bimap[PantStatus.Value](x => x.id, PantStatus(_))
  
  implicit def rw: RW[Pant2Comp] = macroRW
  def tupled = (this.apply _).tupled
  
  def dummy() = new Pant2Comp("", 0L, "", "", PantStatus.UNKN, "_")
  def single(id: Long, coId: Long, status: PantStatus.Value) = new Pant2Comp(Pant.genSNO(id, id), coId, "", "", status, "_") 
  def double(id1: Long, id2: Long, coId: Long, status: PantStatus.Value) = new Pant2Comp(Pant.genSNO(id1, id2), coId, "", "", status, "_")

  def decode(x: String): Either[Error, Pant2Comp] = {
    try Right(read[Pant2Comp](x)) 
    catch { case _: Throwable => Left(Error("err0054.decode.Pant2Comp", x.take(10), "", "Participant.decode")) }
  }

  def decSeq(p2cStr: String): Either[Error, Seq[Pant2Comp]] = {  
    try Right(read[Seq[Pant2Comp]](p2cStr))
    catch { case _: Throwable => Left(Error("err0055.decode.Pant2Comps", p2cStr.take(20),"","Participant.deqSeq")) }
  }
}


case class Pant(
  var sno:       String,         // start number(s) concatenated string of player identifieres  
  val name:      String,                     
  val club:      String, 
  val rating:    Int,            // eg. ttr for table tennis
  var qInfo:     String,     // position after finishing the round (group or ko) 
  var place:     (Int,Int)      // position after finishing the round (group or ko)
  
) {
  def setPlace(p: (Int,Int)) = place = p
  def getPlace  = place._1.toString
  def getRatingInfo = if (rating == 0) "" else rating.toString 
  def getEffRating(value: Int=0) = if (rating == 0) value else rating
  def toCSV = s"${sno}·${name}·${club}·${rating}·${qInfo}·${qInfo}·${place._1}·${place._2}"

  // getName returns name for all types of participants
  def getName(byeName: String="") = if (SNO.isBye(sno)) byeName else name
  def sortSno = getMDLongArr(sno)(0)

}


object Pant {
  implicit def rw: RW[Pant] = macroRW
  def bye(name: String="bye") =  Pant(SNO.BYE, name, "", 0, "", (0, 0))
  def genSNO(id1: Long, id2: Long): String = {
    (id1, id2) match {
      case (x,y) if (x<=0 | y<=0) => ""
      case (x,y) if (x==y)        => x.toString
      case (x,y) if (x>y)         => y.toString + "·" + x.toString
      case (x,y)                  => x.toString + "·" + y.toString 
    }
  }
  def fromCSV(value: String): Either[Error, Pant] = {
    try   { val x = value.split('·'); Right(Pant(x(0), x(1), x(2), x(3).toInt, x(4), (x(5).toInt, x(6).toInt) )) }
    catch {  case _: Throwable =>  Left(Error("err0238.decode.Pant", value))}
  } 
}


case class Pant1(
  var sno:       String,         // start number(s) concatenated string of player identifieres  
  val name:      String,                     
  val club:      String, 
  val rating:    Int,            // eg. ttr for table tennis
  var place:     (Int,Int)       // position after finishing the round (group or ko) 
) {
  def setPlace(p: (Int,Int)) = place = p
  def getPlace  = place._1.toString
  def getRatingInfo = if (rating == 0) "" else rating.toString 
  def getEffRating(value: Int=0) = if (rating == 0) value else rating
  def toCSV = s"${sno}·${name}·${club}·${rating}·${place._1}·${place._2}"
  def toPant(qInfo: String="") = Pant(sno, name, club, rating, qInfo, place)
}


object Pant1 {
  implicit def rw: RW[Pant1] = macroRW
  def bye(name: String="bye") =  Pant(SNO.BYE, name, "", 0, "", (0, 0))
  def genSNO(id1: Long, id2: Long): String = {
    (id1, id2) match {
      case (x,y) if (x<=0 | y<=0) => ""
      case (x,y) if (x==y)        => x.toString
      case (x,y) if (x>y)         => y.toString + "·" + x.toString
      case (x,y)                  => x.toString + "·" + y.toString 
    }
  }
  def fromCSV(value: String): Either[Error, Pant1] = {
    try   { val x = value.split('·'); Right(Pant1(x(0),x(1),x(2),x(3).toInt, (x(4).toInt,x(4).toInt) )) }
    catch {  case _: Throwable =>  Left(Error("err0238.decode.Pant", value))}
  } 
}