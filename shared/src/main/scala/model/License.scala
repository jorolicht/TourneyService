package shared.model

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.Error

/*
 * representing a License stored in Slick/Sqlite
 *
 * Database:
 *  CREATE TABLE "License" (
 *  	"id"	INTEGER PRIMARY KEY AUTOINCREMENT,
 *    "uuid"         TEXT NOT NULL,
 *  	"club"	       TEXT NOT NULL,
 *  	"orgDir"	     TEXT NOT NULL,
 *  	"licStr"	     TEXT NOT NULL,
 *  	"email"	       TEXT NOT NULL,
 *  	"name"	       TEXT NOT NULL,
 *  	"address"      TEXT,
 *  	"password"	   TEXT NOT NULL,
 *  	"reqTStamp"	   TEXT NOT NULL,
 *  	"allowContact" BOOLEAN,
 *    "fullVersion"  BOOLEAN)
 */ 
case class License(
  id:           Long = 0L,    // organizer Id / auto generated
  uuid:         String,
  club:         String,       // club/organizer
  orgDir:       String,       // organization path/directory name (unique)
  licStr:       String,       // <orgdir>/<licCode>   licCode = orgdir with licVal encoded/hashed
  email:        String,                   
  name:         String,
  address:      Option[String],
  password:     String,       // hash value of password
  reqTStamp:    String,
  allowContact: Boolean,
  fullVersion:  Boolean
) {
  def stringify() = write((id,uuid, club,orgDir,licStr,email,name,address,password,reqTStamp,allowContact,fullVersion))
  def encode()    = write((id,uuid,club,orgDir,licStr,email,name,address,password,reqTStamp,allowContact,fullVersion))
}

object License  {
  def tupled = (this.apply _).tupled
  def obify(x: String) = tupled(read[(Long, String, String, String,String,String,String,Option[String],String,String,Boolean,Boolean)](x))
 
  def decode(x: String): Either[Error, License] = {
    try Right(tupled(read[(Long, String, String, String,String,String,String,Option[String],String,String,Boolean,Boolean)](x)))
    catch { case _: Throwable => Left(Error("err0099.decode.License", x)) }
  }

  def fromRequest(licReq: LicRequest, uuid: String, orgDir: String, licStr: String, 
                  password: String, timeStamp: String) = {
    License(0L, uuid, licReq.club, orgDir, licStr, licReq.email, licReq.name, 
            licReq.getAddress, password, timeStamp, licReq.allowContact, licReq.fullVersion)
  }

}

case class Licenses (list : Seq[String])
object Licenses { implicit def rw: RW[Licenses] = macroRW }  


/*
 * License Request from a new user
 */
case class LicRequest(
  name:         String,
  email:        String,
  address:      String,
  password:     String,
  club:         String,        
  allowContact: Boolean,
  fullVersion:  Boolean
) {
  def stringify  = write((name, email, address, password, club, allowContact, fullVersion))
  def getAddress: Option[String] = if (address=="") None else Some(address)
}

object LicRequest {
  def tupled = (this.apply _).tupled
  def obify(x: String) = tupled(read[(String, String, String, String, String, Boolean, Boolean)](x))
}