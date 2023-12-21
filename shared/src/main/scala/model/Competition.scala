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
  val id:           Long,             // auto increment primary key
  val name:         String,           // if empty initialised with ageGroup, ratingRemark, compType                     
  var typ:          CompTyp.Value,    // 0=UNDEFINED 1=EINZEL, 2=DOPPEL, 3=MIXED, 4=TEAM 
  val startDate:    String,           // Format: yyyymmdd#hhmm
                                      // Format: "dd.MM.yyyy HH:mm" or "yyyy-dd-MM HH:mm"
  var status:       CompStatus.Value, // 0=READY, 100=RUNNING 
  var options:      String = ""
) {

  def hash(): Int = s"${name}${typ.id.toString}${startDate}${getFromTTR}${getToTTR}".hashCode


  // mapping of licence to player identification accoridng to clickTT participant list
  // currently only for single player 
  var cttLic2player: Map[String, String] = Map().withDefaultValue("")

  def getTyp(value: String) = value.toLowerCase match {
    case "einzel" | "single" => CompTyp.SINGLE
    case "doppel" | "double" => CompTyp.DOUBLE
    case "mixed"             => CompTyp.MIXED
    case "team"              => CompTyp.TEAM
    case _                   => CompTyp.Typ
  } 

  def chkStatus(chkWith: CompStatus.Value*): Boolean = chkWith.contains(status)

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

  def getStartDate(mfun:(String, Seq[String])=>String, format:Int=0) = formatTime(mfun("app.lang",Seq()), format )

  // validateDate - startDate is in range
  def validateDate(trnyStart: Int, trnyEnd: Int): Boolean = {
    val (year, month, day, hour, minute) = ymdHM(startDate)
    val sDate = year * 10000 + month * 100 + day
    sDate >= trnyStart & (trnyEnd==0 | sDate <= trnyEnd)
  }

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
    } else { "0000-XXXX" }   
  }

  def getFromTTR: String = if (getRatingLowLevel>0) "%04d".format(getRatingLowLevel) else "0000"
  def getToTTR: String   = if (getRatingUpperLevel>0) "%04d".format(getRatingUpperLevel) else "XXXX"
  
  def getStatusName(mfun:(String, Seq[String])=>String): String = mfun(status.msgCode, Seq())
  def getTypName(mfun:(String, Seq[String])=>String): String = mfun(typ.msgCode,Seq())  
  def genName(fun:(String, Seq[String])=>String): String = {
    if(name!="") { name } else { s"${getAgeGroup} ${getRatingRemark} ${getTypName(fun)}" }
  }

  def equal(co: Competition): Boolean = hash == co.hash
  
  def encode = write[Competition](this)

  def matchClickTT(ageGroup: String, ttrFrom: String, ttrTo: String, ttrRemark: String, cttType: String): Boolean = {
    getAgeGroup.toLowerCase     == ageGroup.toLowerCase && 
    getRatingRemark.toLowerCase == ttrRemark.toLowerCase &&
    getRatingLowLevel           == ttrFrom.toIntOption.getOrElse(0) &&
    getRatingUpperLevel         == ttrTo.toIntOption.getOrElse(0) &&
    typ                         == getTyp(cttType)
  }
}                                                               


object Competition {
  implicit val compStatusReadWrite: upickle.default.ReadWriter[CompStatus.Value] =
    upickle.default.readwriter[Int].bimap[CompStatus.Value](x => x.id, CompStatus(_))

  implicit val compTypReadWrite: upickle.default.ReadWriter[CompTyp.Value] =
    upickle.default.readwriter[Int].bimap[CompTyp.Value](x => x.id, CompTyp(_))

  implicit def rw: RW[Competition] = macroRW

  def tupled = (this.apply _).tupled
  def init             = new Competition(0L, "",  CompTyp.Typ, "", CompStatus.CFG, "")
  def get(name: String)= new Competition(0L, name,  CompTyp.Typ, "", CompStatus.CFG, "")

  def decode(s: String): Either[shared.utils.Error, Competition] = {
    try Right(read[Competition](s))
    catch { case _: Throwable => Left(Error("err0020.decode.Competition", s, "", "Competition.decode"))} 
  }

  def decSeq(comps: String): Either[shared.utils.Error, Seq[Competition]] = {
    try Right( read[Seq[Competition]](comps))
    catch { case _: Throwable => Left(Error("err0061.decode.Competitions", comps.take(20), "", "Competition.deqSeq")) }
  }
}

object CompTyp extends Enumeration {
  val UNKN   = Value(0,  "UNKN")
  val SINGLE = Value(1,  "SINGLE") 
  val DOUBLE = Value(2,  "DOUBLE")    
  val MIXED  = Value(3,  "MIXED")         
  val TEAM   = Value(4,  "TEAM")
  val Typ    = Value(99, "Typ")
  
  implicit class CompTypValue(ct: Value) {
    def msgCode = s"CompTyp.${ct.toString}"
  }
}  

object CompStatus extends Enumeration {

  val UNKN   = Value(97,  "UNKN")  
  val Status = Value(98,  "Status")
  val CFG    = Value(99,  "CFG")
  val RUN    = Value(100, "RUN")
  val FIN    = Value(103, "FIN")

  val READY  = Value(  0, "READY")
  val VRAUS  = Value(  1, "VRAUS")   // Auslosung der Vorrunde       
  val VREIN  = Value(  2, "VREIN")   // Auslosung erfolgt, Eingabe der Ergebnisse
  val VRFIN  = Value(  3, "VRFIN")   // Vorrunde beendet, Auslosung ZR oder ER kann erfolgen

  val ZRAUS  = Value(  4, "ZRAUS")   // Auslosung der Zwischenrunde      
  val ZREIN  = Value(  5, "ZREIN")   // Auslosung erfolgt, Eingabe der Ergebnisse
  val ZRFIN  = Value(  6, "ZRFIN")   // Zwischenrunde beendet, Auslosung ER kann erfolgen

  val ERAUS  = Value(  7, "ERAUS")   // Auslosung der Endrunde   
  val EREIN  = Value(  8, "EREIN")   // Auslosung erfolgt, Eingabe der Ergebnisse
  val ERFIN  = Value(  9, "ERFIN")   // Endrunde beendet ... 

  implicit class CompStatusValue(cs: Value) {
    def msgCode = s"CompStatus.${cs.toString}"
  }

}