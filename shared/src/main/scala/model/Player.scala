package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.utils.{ Error, Return }
import shared.utils.Routines._
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap

/* Player in                               
 *  Verbindung von firstname, lastname, birthyear, licence-nr ist eindeutig.
 * 	nationality: Text, zweistellg. Ländercode
 * 	foreigner-eq-state: Text, Ausländergleichstellungsstatus, nur relevant, 
 *  wenn Nationalität ungleich DE
 * 	region: Bezirk, des Vereins bei dem die Person Mitglied ist
 * 	sub-region: Kreis, des Vereins bei dem die Person Mitglied ist
 * Person can be in one or more Players (e.g. he plays Double or Team,
 * or hes is playing in serveral Competitions)    
 */
case class Player(                                             
  var id:          Long,                    // primary key
  var hashKey:     String,                  // hashKey to unique identify player cross environments
  var clubId:      Long,                    // plubId primary key of plub = Players Club
  var clubName:    String,                  // club-name CDATA #IMPLIED  
  val firstname:   String,             
  val lastname:    String,
  val birthyear:   Int,                     // 0 => not specified
  var email:       String,
  val sex:         Int,                     // 0 => not specified, 1 => female, 2 => male
  var options:     String = "_"             // separated List of optional parameters

/*
  Optional attributes encoded in options ":" as separator
  table tennis specific attributes (all clickTT definitions included)
                
  internalNr:  String,           // 0  internal-nr CDATA #REQUIRED                  
  licenceNr:   String,           // 1  licence-nr CDATA #REQUIRED                                                        
  clubNr:      String,           // 2  club-nr CDATA #IMPLIED                       
  clubFedNick: String,           // 3  club-federation-nickname CDATA #IMPLIED      
  ttr:         String,           // 4  ttr CDATA #IMPLIED                           
  ttrMatchCnt: String,           // 5  ttr-match-count CDATA #IMPLIED               
  nationality: String,           // 6  nationality CDATA #IMPLIED                   
  forEqState:  String,           // 7  foreigner-eq-state CDATA #IMPLIED            
  region:      String,           // 8  region CDATA #IMPLIED                        
  subRegion:   String            // 9  sub-region CDATA #IMPLIED                    
*/

) {
  def encode  = write[Player](this)
   
  def oneName: String = {
    if      (lastname.trim == "") firstname
    else if (firstname.trim == "") lastname
    else s"$lastname, $firstname"
  }

  def getName(fmt: Int=0) = fmt match {
    case 0 => oneName
    case 1 => if (id!=0) f"$lastname, $firstname [${id}%03d]" else oneName
    case _ => oneName
  }

  def getDoubleName(player2: Player): String = {
    lastname + "/" + player2.lastname 
  }

  def getDoubleClub(player2: Player): String = {
    clubName + "/" + player2.clubName 
  }  

  def getDoubleRating(player2: Player): Int = {
    val rating = getRating + player2.getRating
    if (getRating == 0 | player2.getRating == 0) rating else rating / 2
  }  


  def getOptStr(index: Int): String   = getMDStr(options, index) 
  def getOptInt(index: Int): Int      = getMDInt(options, index)
  def getOptLong(index: Int): Long    = getMDLong(options, index)
  def setOpt[X](value: X, index: Int) = options = setMD(options, value, index)

  def getInternalNr: String  = getMDStr(options,0); def setInternalNr(value: String) = { options = setMD(options, value, 0) }
  def getLicenceNr: String   = getMDStr(options,1); def setLicenceNr(value:String)   = { options = setMD(options, value, 1) }
  def getClubNr: String      = getMDStr(options,2); def setClubNr(value:String)      = { options = setMD(options, value, 2) }
  def getClubFedNick: String = getMDStr(options,3); def setClubFedNick(value:String) = { options = setMD(options, value, 3) }
  def getTTR: String         = getMDStr(options,4); def setTTR(value:String)         = { options = setMD(options, value, 4) }
  def getRating: Int         = getMDInt(options,4); def setRating(value: Int)        = { options = setMD(options, value, 4) } 
  def getTTRMatchCnt: String = getMDStr(options,5); def setTTRMatchCnt(value:String) = { options = setMD(options, value, 5) }
  def getNationality: String = getMDStr(options,6); def setNationality(value:String) = { options = setMD(options, value, 6) }
  def getForEqState: String  = getMDStr(options,7); def setForEqState(value:String)  = { options = setMD(options, value, 7) }
  def getRegion: String      = getMDStr(options,8); def setRegion(value:String)      = { options = setMD(options, value, 8) }
  def getSubRegion: String   = getMDStr(options,9); def setSubRegion(value:String)   = { options = setMD(options, value, 9) }

  def getClub(fmt: Int=0) = fmt match {
    case 0 => clubName
    case 1 => if (clubId!=0) f"$clubName [${clubId}%03d]" else  clubName
    case _ => clubName 
  }

  def getBirthyear()    = if (birthyear == 0) None else Some(birthyear)
  def getBYearStr() = if (birthyear == 0) "" else birthyear.toString


  def updLicenseNr(name2person: HashMap[String, ArrayBuffer[(String, Int, String)]]) = {
    if (getLicenceNr == "") {
      val name = s"${lastname}·${firstname}"
      if (name2person.isDefinedAt(name)) {
        val cylArray = (name2person(name)) 
        if (cylArray.length == 1) { 
          println("Update LicenseNr")
          setLicenceNr(cylArray(0)._3) 
        } else {
          cylArray.foreach { elem => if (elem._1 == clubName) setLicenceNr(elem._3) }
        } 
      }
    }
  }


  
}

object Player {
  implicit def rw: RW[Player] = macroRW
  def tupled = (this.apply _).tupled
  def init            = new Player(0L, "", 0L, "","","", 0, "", 0, "_")
  def get()           = new Player(0L, "", 0L, "","","", 0, "", 0, "_")
  def get(plId: Long) = new Player(plId, "", 0L, "", "", "", 0, "", 0, "_")
  def get(lastname: String, firstname: String, clubName: String, birthyear: Int, email: String, sex: Int) = {
    new Player(0L, "", 0L, clubName, firstname, lastname, birthyear, email, sex, "_")
  }
  def format(name: String, fmt: Int = 0) = {
    val na = name.split(",")
    fmt match {
      case 0 => if (na.length==2) s"${na(0)} ${na(1)}" else name
      case _ => name
    }
  }
  

  def parseName(name: String): Either[Error, (String, String, Long)] = {
    val mResult = "[^, ]+,[ ]*[^, ]+[ ]*\\[\\d\\d\\d\\]".r.findFirstIn(name).getOrElse(
      "[^, ]+,[ ]*[^, ]+[ ]*".r.findFirstIn(name).getOrElse("")
    )
    val res = mResult.split("[,\\[\\]]") 
    res.size match {
      case 3 if mResult == name.trim => Right((res(0).trim, res(1).trim, res(2).toLong))
      case 2 if mResult == name.trim => Right((res(0).trim, res(1).trim, 0L))
      case _ => Left(Error("err0159.Player.parseName"))
    }
  }  


  /** parseEmail - returns email or empty string or an error
   *  
   */ 
  def parseEmail(email: String): Either[Error, String] = {
    if (email.trim == "") Right("")
    else if (validEmail(email)) Right(email) 
    else Left(Error(""))
  }

  def decode(s: String) : Either[Error, Player] = {
    try Right(read[Player](s))
    catch { case _: Throwable => Left(Error("err0132.decode.Player", s, "", "Player.decode")) }
  }

 
  def decSeq(plStr: String): Either[Error, Seq[Player]] = {
    try Right(read[Seq[Player]](plStr))
    catch { case _: Throwable => Left(Error("err0038.decode.Players", plStr.take(20), "", "Player.decSeq")) }
  } 

}