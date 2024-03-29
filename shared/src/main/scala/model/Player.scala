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
  var clubId:      Long,                    // plubId primary key of plub = Players Club
  var clubName:    String,                  // club-name CDATA #IMPLIED  
  var firstname:   String,             
  var lastname:    String,
  var birthyear:   Int,                     // 0 => not specified
  var email:       String,
  var sex:         SexTyp.Value,            // 0 => not specified, 1 => female, 2 => male
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

  def hash =  s"${lastname}${firstname}${clubName}${getTTR}${getBYearStr()}".hashCode()

  def getInternalNr: String  = getMDStr(options,0); def setInternalNr(value: String) = { options = setMD(options, value, 0) }
  
  def getLicense: CttLicense     = CttLicense(getMDStr(options,1))
  def setLicense(lic:CttLicense) = { options = setMD(options, lic.value, 1) }
  def hasLicense = getMDStr(options,1) != ""
  def delLicense = { options = setMD(options, "", 1) }
  
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

  def setClub(club: Club) = {
    clubId   = club.id
    clubName = club.name
  }

  def getBirthyear() = if (birthyear == 0) None else Some(birthyear)
  def getBYearStr()  = if (birthyear == 0) "" else birthyear.toString

}

object Player {
  implicit val sextypReadWrite: upickle.default.ReadWriter[SexTyp.Value] =
    upickle.default.readwriter[Int].bimap[SexTyp.Value](x => x.id, SexTyp(_))

  implicit def rw: RW[Player] = macroRW
  def tupled = (this.apply _).tupled

  def get(lastname: String, firstname: String, clubName: String, birthyear: Int, ttr: Int, sex: SexTyp.Value) = {
    val p = new Player(0L, 0L, clubName, firstname, lastname, birthyear, "", sex, "_")
    p.setTTR(ttr.toString)
  }

  def dummy = new Player(0L, 0L, "", "", "", 1970, "", SexTyp.UNKN, "_")
 
  def fromCSV(value: String): Either[Error, Player] = 
    try {
      val p = value.split(',')
      val (lastname, firstname, club, ttr, birthyear, gender, email) = p.length match {
        case 7 => (p(0).trim, p(1).trim, p(2).trim, p(3).trim.toInt, p(4).trim.toInt, SexTyp(p(5).trim.toInt), p(6).trim) 
        case 6 => (p(0).trim, p(1).trim, p(2).trim, p(3).trim.toInt, p(4).trim.toInt, SexTyp(p(5).trim.toInt), "") 
        case 5 => (p(0).trim, p(1).trim, p(2).trim, p(3).trim.toInt, p(4).trim.toInt, SexTyp(0), "") 
        case 4 => (p(0).trim, p(1).trim, p(2).trim, p(3).trim.toInt, 0, SexTyp(0), "") 
        case 3 => (p(0).trim, p(1).trim, p(2).trim, 0, 0, SexTyp(0), "")  
        case _ => ("", "", "", 0, 0, SexTyp(0), "")  
      }
      lastname.toLowerCase() match {
        case "name"      => Left(Error("return001.csv.hdr.player"))
        case "lastname"  => Left(Error("return001.csv.hdr.player"))
        case ""          => Left(Error("err0239.decode.csv.player", value))
        case _           => {
          val player = Player(0L, 0L, club, firstname, lastname, birthyear, email, gender) 
          player.setTTR(ttr.toString)
          Right(player)
        }      
      } 
    } catch { case _: Throwable => Left(Error("err0239.decode.csv.player", value)) }




  def get(lastname: String, firstname: String, clubName: String, birthyear: Int, email: String, sex: SexTyp.Value) = {
    new Player(0L, 0L, clubName, firstname, lastname, birthyear, email, sex, "_")
  }
  def format(name: String, fmt: Int = 0) = {
    val na = name.split(",")
    fmt match {
      case 0 => if (na.length==2) s"${na(0)} ${na(1)}" else name
      case _ => name
    }
  }
  

  def validateName(name: String): Either[Error, (String, String, Long)] = {
    val mResult = "[^, ]+,[ ]*[^, ]+[ ]*\\[\\d\\d\\d\\]".r.findFirstIn(name).getOrElse(
      "[^, ]+,[ ]*[^, ]+[ ]*".r.findFirstIn(name).getOrElse("")
    )
    val res = mResult.split("[,\\[\\]]") 
    res.size match {
      case 3 if mResult == name.trim => Right((res(0).trim, res(1).trim, res(2).toLong))
      case 2 if mResult == name.trim => Right((res(0).trim, res(1).trim, 0L))
      case _ => Left(Error("err0159.Player.validateName"))
    }
  }

  def validateEmail(inText: String): Either[Error, String] = {
    if (inText.trim == "" || validEmail(inText.trim)) Right(inText.trim) else Left(Error("err0160.Player.validateEmail"))
  } 

  def validateTTR(inValue: String, range: (Int,Int)): Either[Error, String] = {
    val ttr = inValue.toIntOption.getOrElse(-1)
    if (inValue.trim == "" || (ttr >= range._1 && ttr <= range._2) ) Right(inValue.trim) else Left(Error("err0217.Player.validateTTR"))
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

object SexTyp extends Enumeration {
  val UNKN   = Value(0, "UNKN")
  val FEMALE = Value(1, "FEMALE")
  val MALE   = Value(2, "MALE")
}