package shared.model

//import shared.model.gamesystem.Match
import scala.collection.mutable.{ ArrayBuffer, Map, HashMap }
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.{ Error, Return }
import shared.utils.Routines._
import shared.utils.Constants._

/*
**
**  TOURNBASE
**
**  representing a row in the Tourney Table with
**  unique id, name of Tourney, club, start and 
**  endDate (format yyyymmdd as Integer)
**  and clickTTid 
*/

case class TournBase(
  val name:      String, 
  var organizer: String,             // name of the organizer (club or key word) of registered program/tourney user
  val orgDir:    String,             // unified organizer name (used as directory)
  val startDate: Int, 
  var endDate:   Int, 
  var ident:     String,             // clickTTid 
  val typ:       Int,                // 0 = unknown, 1 = Tischtennis, ...
  var privat:    Boolean,            // privat tourneys are only seen by registered users
  var contact:   String = "",        // lastname·firstname·phone·email
  var address:   String = "",        // "description·country·zip·city·street"
  val id:        Long = 0L           // autoincrement
) extends {
   
  def encode(): String = write[TournBase](this)

  def check(): Either[Error, Boolean] = {
    val (y,m,d) = int2ymd(startDate)
    if (name.length() <= 3 ) {
      Left(Error("err0179.Tourney.name"))
    } else if (startDate > endDate) {
      Left(Error("err0180.Tourney.edate"))
    } else if (y < 2022) {
      Left(Error("err0181.Tourney.sdate"))
    } else Right(true)
  }  
  
  def getAddrDescription: String = getMDStr(address,0);    def setAddrDescription(value: String) = { address = setMD(address,value,0) }
  def getAddrCountry: String     = getMDStr(address,1);    def setAddrCountry(value: String)     = { address = setMD(address,value,1) }
  def getAddrZIP: String         = getMDStr(address,2);    def setAddrZIP(value: String)         = { address = setMD(address,value,2) }
  def getAddrCity: String        = getMDStr(address,3);    def setAddrCity(value: String)        = { address = setMD(address,value,3) }
  def getAddrStreet: String      = getMDStr(address,4);    def setAddrStreet(value: String)      = { address = setMD(address,value,4) }

  def getContactLastname: String  = getMDStr(contact,0);   def setContactLastname(value: String)  = { contact = setMD(contact,value,0) }
  def getContactFirstname: String = getMDStr(contact,1);   def setContactFirstname(value: String) = { contact = setMD(contact,value,1) }
  def getContactPhone: String     = getMDStr(contact,2);   def setContactPhone(value: String)     = { contact = setMD(contact,value,2) }
  def getContactEmail: String     = getMDStr(contact,3);   def setContactEmail(value: String)     = { contact = setMD(contact,value,3) }
  def getContactName: String      = {
    val lname = getContactLastname
    val fname = getContactFirstname
    if (lname != "" & fname != "") {
      lname + ", " + fname 
    } else {
      lname + fname 
    }
  }

  def getStartDate(lang: String, fmt:Int=0): String = int2date(startDate, lang, fmt)
  def getEndDate(lang: String, fmt:Int=0): String = int2date(endDate, lang, fmt)

  def setContact(name: String, email: String, phone: String) = {
    val nArr1 = name.split(",")
    val nArr2 = name.split(" ")      
    val (lastname,firstname) = if (nArr1.length>=2) {
      (nArr1(0).trim, nArr1(1).trim)
    } else if (nArr2.length>=2) {
      (nArr2(0).trim, nArr2(1).trim) 
    } else {
      (name.trim,"")
    }
    contact = s"${lastname}·${firstname}·${phone}·${email}"
  }

  def setAddress(desc: String, country: String, zip: String, city: String, street: String) = {
    address = s"${desc}·${country}·${zip}·${city}·${street}"
  }
}  
 
 
object TournBase {
  // necessary workaround for slick
  val tupled = (this.apply _).tupled

  implicit val tourneyTypReadWrite: upickle.default.ReadWriter[TourneyTyp.Value] =
    upickle.default.readwriter[Int].bimap[TourneyTyp.Value](x => x.id, TourneyTyp(_))

  implicit def rw: RW[TournBase] = macroRW

  def init(organizer: String, orgDir: String, date: Int, typ: TourneyTyp.Value) 
    = new TournBase("", organizer, orgDir, date, 0, "", typ.id, true, "", "", 0L)

  def decode(trnyStr: String): Either[Error, TournBase] = 
    if (trnyStr.length > 0 ){
      try Right(read[TournBase](trnyStr))
      catch { case _: Throwable => Left( Error("err0062.decode.TournBase", trnyStr.take(20), "", "TournBase.decode")) }
    } else {
      Left(Error("err0062.decode.TournBase", "<empty input>", "", "TournBase.decode"))
    }

  def encSeq(tBS: Seq[TournBase]): String = write[Seq[TournBase]](tBS)  
  def decSeq(tbStr: String): Either[Error, Seq[TournBase]] = {
    try Right(read[Seq[TournBase]](tbStr))
    catch { case _: Throwable => Left(Error("err0144.decode.TournBase", tbStr.take(20), "", "TournBase.decSeq")) }
  }

}


case class TourneyBaseData(id: Long, name: String, organizer: String, orgDir: String, 
                           startDate: Int, endDate: Int, ident: String, typ: TourneyTyp.Value, 
                           privat: Boolean, contact: Contact, address: Address)
                           
object TourneyBaseData {
  implicit val tourneyTypReadWrite: upickle.default.ReadWriter[TourneyTyp.Value] =
    upickle.default.readwriter[Int].bimap[TourneyTyp.Value](x => x.id, TourneyTyp(_))
  implicit def rw: RW[TourneyBaseData] = macroRW
}  
