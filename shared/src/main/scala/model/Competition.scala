package shared.model

import scala.util.matching
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }
import shared.utils.{ Error, Return }
import shared.utils.Routines._
import shared.utils.Constants._


/* Competition (based on Click TT Competition definition)                            
 * Tournament/Event can have 1..n competitions
 *             
 *     !ELEMENT competition (players, matches?)>                                    
 *     !ATTLIST competition                                      
 *       age-group CDATA #REQUIRED                            
 *       type (Einzel|Doppel|Mixed|Mannschaft) #REQUIRED                                                     
 *       start-date CDATA #REQUIRED                                                       
 *       ttr-from CDATA #IMPLIED      -> RatingLowerLevel                                 
 *       ttr-to CDATA #IMPLIED        -> RatingUpperLevel                          
 *       ttr-remarks CDATA #IMPLIED   -> RatingRemark      (e.g. C-Klasse)                          
 *       entry-fee CDATA #IMPLIED                                      
 *       age-from CDATA #IMPLIED                                       
 *       age-to CDATA #IMPLIED                                        
 *       sex CDATA #IMPLIED                 
 *       preliminary-round-playmode CDATA #IMPLIED                         
 *       final-round-playmode CDATA #IMPLIED                                   
 *       max-persons CDATA #IMPLIED                       
 *       manual-final-rankings (0|1) #IMPLIED
 */

case class Competition(      
  val id:           Long,        // auto increment primary key
  val hashKey:      String,      // hashKey to unique identify competition (name and typ to be unique)
  val name:         String,      // if empty initialised with ageGroup, ratingRemark, compType                     
  var typ:          Int,         // 0=UNDEFINED 1=EINZEL, 2=DOPPEL, 3=MIXED, 4=TEAM 
  val startDate:    String,      // Format: yyyymmdd#hhmm
                                 // Format: "dd.MM.yyyy HH:mm" or "yyyy-dd-MM HH:mm"
  var status:       Int,         // -1=WEB/SELF REGISTRATION, 0=READY, 1=RUNNING 
  var options:      String = "" 

 

  /** 
    values encoded within options for table tennis
    options(0)  => ageGroup:         String eg. Damen, Herren, Mädchen, Jungen, Schüler, ...
    options(1)  => ratingRemark:     String
    options(2)  => ratingLowLevel:   Int                           
    options(3)  => ratingUpperLevel: Int
    options(4)  => sex:              Int (0= undefined,  1=Männer, 2=Frauen, 3=egal)
    options(5)  => maxPerson:        Int  
    options(6)  => entryFee:         String                           
    options(7)  => ageFrom:          String                           
    options(8)  => ageTo:            String                                  
    options(9)  => preRndMod:        String                               
    options(10) => finRndMod:        String                                      
    options(11) => manFinRank:       String
  */
) {
  // mapping of licence to player identification accoridng to clickTT participant list
  // currently only for single player 
  var cttLic2player: Map[String, String] = Map().withDefaultValue("")

  def getTyp(value: String): Int = value.toLowerCase match {
    case "einzel" | "single" => 1
    case "doppel" | "double" => 2
    case "mixed"             => 3
    case "team"              => 4
    case _                   => 0
  } 

  def setTyp(value : String)= { typ = getTyp(value) }

  def getOptStr(index: Int): String   = getMDStr(options, index) 
  def getOptInt(index: Int): Int      = getMDInt(options, index)
  def getOptLong(index: Int): Long    = getMDLong(options, index)
  def setOpt[X](value: X, index: Int) = { options = setMD(options, value, index) }

  def getAgeGroup: String      = getMDStr(options,0);   def setAgeGroup(value: String)      = { options = setMD(options, value, 0) }
  def getRatingRemark: String  = getMDStr(options,1);   def setRatingRemark(value: String)  = { options = setMD(options, value, 1) }
  def getRatingLowLevel: Int   = getMDInt(options,2);   def setRatingLowLevel(value: Int)   = { options = setMD(options, value, 2) }
  def getRatingUpperLevel: Int = getMDInt(options,3);   def setRatingUpperLevel(value: Int) = { options = setMD(options, value, 3) }
  def getSex: Int              = getMDInt(options,4);   def setSex(value:Int)               = { options = setMD(options, value, 4) }  
  def getMaxPerson: Int        = getMDInt(options,5);   def setMaxPerson(value:Int)         = { options = setMD(options, value, 5) }
  def getEntryFee: String      = getMDStr(options,6);   def setEntryFee(value:String)       = { options = setMD(options, value, 6) }
  def getAgeFrom: String       = getMDStr(options,7);   def setAgeFrom(value:String)        = { options = setMD(options, value, 7) }
  def getAgeTo: String         = getMDStr(options,8);   def setAgeTo(value:String)          = { options = setMD(options, value, 8) }
  def getPreRndMod: String     = getMDStr(options,9);   def setPreRndMod(value:String)      = { options = setMD(options, value, 9) }
  def getFinRndMod: String     = getMDStr(options,10);  def setFinRndMod(value:String)      = { options = setMD(options, value, 10) }
  def getManFinRank: String    = getMDStr(options,11);  def setManFinRank(value:String)     = { options = setMD(options, value, 11) }
  def getWebRegister: Boolean  = getMDBool(options,12); def setWebRegister(value:Boolean)   = { options = setMD(options, value, 12) }
  def getCurCoPhId: Int        = getMDInt(options,13);  def setCurCoPhId(value:Int)         = { options = setMD(options, value, 13) }

  // formatTime - depending on local
  // 0 - date and time
  // 1 - date
  // 2 - time
  def formatTime(lang: String, fmt:Int=0): String = {
    val datetime = """(\d\d\d\d\d\d\d\d)#(\d\d\d\d)""".r
    startDate match { 
      case datetime(ymd, time) => fmt match {
        case 0 => int2date(ymd.toInt, lang, 0) + " " + int2time(time.toInt, lang) 
        case 1 => int2date(ymd.toInt, lang, 0) 
        case 2 => int2time(time.toInt, lang) 
        case _ => startDate  
      }
      case                  _  => startDate
    }
  }

  // validateDate - startDate is in range
  def validateDate(trnyStart: Int, trnyEnd: Int): Boolean = {
    val (year, month, day, hour, minute) = ymdHM(startDate)
    val sDate = year * 10000 + month * 100 + day
    sDate >= trnyStart & (trnyEnd==0 | sDate <= trnyEnd)
  }  

  // getStartDate - depending on local
  //def getStartDate() = Competition.getStartDate(startDate)


  // Name composed from type, agegroup, ....
  // Name containing middle dot
  def isNameComposed():Boolean = name.contains("·")

  def genRange(): String = {
    val lB = getRatingLowLevel
    val uB = getRatingUpperLevel
    if (lB > 0 & uB > 0) { 
      f"[$lB%04.0f-$uB%04.0f]" 
    } else if (lB > 0 & uB == 0) {
      f"[$lB%04.0f-XXXX]" 
    } else if (lB == 0 & uB > 0) {
      f"[0000-$uB%04.0f]"
    } else { "" }   
  }

  def getFromTTR: String = if (getRatingLowLevel>0) "%04d".format(getRatingLowLevel) else "0000"
  def getToTTR: String   = if (getRatingUpperLevel>0) "%04d".format(getRatingUpperLevel) else "XXXX"
  
  def genName(fun:(String, Seq[String])=>String): String = {
    if(name!="") { name } else { s"${getAgeGroup} ${getRatingRemark} ${getTypName(fun)}" }
  }

  def equal(co: Competition): Boolean = hash == co.hash
  def hash = s"${name}^${typ}^${startDate}^${getFromTTR}^${getToTTR}"
  def encode = write[Competition](this)

  def getStatusName(mfun:(String, Seq[String])=>String): String = mfun(s"competition.status.${status}",Seq())
  def getTypName(mfun:(String, Seq[String])=>String): String = mfun(s"competition.typ.${typ}",Seq())

  def matchClickTT(ageGroup: String, ttrFrom: String, ttrTo: String, ttrRemark: String, cttType: String): Boolean = {
    getAgeGroup.toLowerCase     == ageGroup.toLowerCase && 
    getRatingRemark.toLowerCase == ttrRemark.toLowerCase &&
    getRatingLowLevel           == ttrFrom.toIntOption.getOrElse(0) &&
    getRatingUpperLevel         == ttrTo.toIntOption.getOrElse(0) &&
    typ                         == getTyp(cttType)
  } 

}                                                               

object Competition {
  implicit def rw: RW[Competition] = macroRW

  // competition type
  val CT_UNKN   = 0
  val CT_SINGLE = 1
  val CT_DOUBLE = 2
  val CT_MIXED  = 3
  val CT_TEAM   = 4


  // Competition Status
  val CS_UNKN  =  -1 // unknown status
  val CS_REDY  =   0 // Ready / RESET
  val CS_RUN   = 100 // RUNNING  
  
  // Competition Status (old values)
  val CS_VRAUS = 1   // Auslosung der Vorrunde
  val CS_VREIN = 2   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CS_VRFIN = 3   // Vorrunde beendet, Auslosung ZR oder ER kann erfolgen

  val CS_ZRAUS = 4   // Auslosung der Zwischenrunde
  val CS_ZREIN = 5   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CS_ZRFIN = 6   // Zwischenrunde beendet, Auslosung ER kann erfolgen
  
  val CS_ERAUS = 7   // Auslosung der Endrunde
  val CS_EREIN = 8   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CS_ERFIN = 9   // Endrunde beendet ...

  def tupled = (this.apply _).tupled
  def init             = new Competition(0L, "", "",  CT_UNKN, "", CS_UNKN, "")
  def get(name: String)= new Competition(0L, "", name,  CT_UNKN, "", CS_UNKN, "")

  def ct2Name(x: Int): String = {
    x match {
      case CT_UNKN   => "unknown"
      case CT_SINGLE => "EINZEL"
      case CT_DOUBLE => "DOPPEL"
      case CT_MIXED  => "MIXED"
      case CT_TEAM   => "TEAM"      
      case _         => "unknown"
    }
  }

  def decode(s: String): Either[shared.utils.Error, Competition] = {
    try Right(read[Competition](s))
    catch { case _: Throwable => Left(Error("err0020.decode.Competition", s, "", "Competition.decode"))} 
  }

  def decSeq(comps: String): Either[shared.utils.Error, Seq[Competition]] = {
    try Right( read[Seq[Competition]](comps))
    catch { case _: Throwable => Left(Error("err0061.decode.Competitions", comps.take(20), "", "Competition.deqSeq")) }
  }

}