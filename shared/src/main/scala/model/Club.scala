package shared.model

import java.util.UUID
import scala.util.Try
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import shared.utils.{ Error, Return }
import shared.utils.Routines._


/* Club of Player contains all relevant infos about players club
 * with reference to external programs 
 */
case class Club (
  var id:      Long,        // generate (unique) id
  var rid:     Int,         // id/key of external program
  name:        String,      // the club name
  var options: String = "_" 
) 
{
  def this(name: String) = this(0L, 0, name, "_")
  def this(id: Long, name: String) = this(id, 0, name, "_")

  def stringify = s"${id}^${rid}^${name}^${options}^_"
  def encode() = s"${id}^${rid}^${name}^${options}^_"

  def getOptStr(index: Int): String   = getMDStr(options, index) 
  def getOptInt(index: Int): Int      = getMDInt(options, index)
  def getOptLong(index: Int): Long    = getMDLong(options, index)
  def setOpt[X](value: X, index: Int) = { options = setMD(options, value, index) }
  
  def getClubNr: String       = getMDStr(options, 0);    def setClubNr(value: String)      = { options = setMD(options, value, 0) }
  def getClubFedNick: String  = getMDStr(options, 1);    def setClubFedNick(value: String) = { options = setMD(options, value, 1) }

  def getName(fmt: Int=0) = fmt match {
    case 0 => name
    case 1 => if (id!=0) f"$name [${id}%03d]" else  name
    case _ => name 
  }

}

object Club {
  implicit def rw: RW[Club] = macroRW
  def tupled = (this.apply _).tupled

  def obify(s: String): Club = {
    val cl = s.split("\\^")
    try { 
      Club(cl(0).toLong, cl(1).toInt, cl(2), cl(3))
    } catch { case _: Throwable => Club(0L, 0, "", "_" ) }
  }
  
  def decode(s: String): Either[Error, Club] = {
    val cl = s.split("\\^")
    try Right(Club(cl(0).toLong, cl(1).toInt, cl(2), cl(3)))
    catch { case _: Throwable => Left(Error("err0050.decode.Club", s, "", "Club.decode"))} 
  }
  
  def encSeq(clubSeq: Seq[Club]): String = {
    write[Clubs](Clubs(clubSeq.map(_.stringify)))
  }

  def parseName(name: String): Either[Error, (String, Long)] = {
    val mResult = "[^,;:$=?+*\"]+[ ]+\\[\\d\\d\\d\\]".r.findFirstIn(name).getOrElse(
      "[^,;:$=?+*\"]+".r.findFirstIn(name).getOrElse("")
    )
    val res = mResult.split("[\\[\\]]") 
    res.size match {
      case 2 if (mResult == name.trim)                          => Right((res(0).trim, res(1).toLong))
      case 1 if ((mResult == name.trim) & (mResult.length > 2)) => Right((res(0).trim, 0L))
      case _ => Left(Error("err0161.Club.parseName"))
    }
  } 

  def decSeq(clStr: String): Either[Error, Seq[Club]] = {
    try decSeq(read[Clubs](clStr).list)  
    catch { case _: Throwable => Left(Error("err0056.decode.Clubs", clStr.take(20), "", "Club.decSeq")) }
  } 

  def decSeq(clubs: Seq[String]): Either[Error, Seq[Club]] = {
    try {
      (clubs.map{ cl => Club.decode(cl) }).partitionMap(identity) match {
        case (Nil, rights)      => Right(rights.toSeq)
        case (firstErr :: _, _) => Left(firstErr.add("Club.decSeq"))
      }
    } catch { case _: Throwable => Left(Error("err0060.decode.Clubs", clubs.toString().take(20), "", "Club.decSeq")) }
  } 

}

case class Clubs (list : Seq[String])
object Clubs { implicit def rw: RW[Clubs] = macroRW }