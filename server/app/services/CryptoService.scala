package tourn.services

import java.util
import java.util.Date
import java.util.Base64
import java.util.UUID
import java.util.zip.CRC32
import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.nio.ByteBuffer
import java.security.MessageDigest
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import scala.util.control.Exception._
import scala.collection.mutable.HashMap

import play.api.i18n._
import play.api.Logger
import play.api.mvc.Cookie
import play.api.libs.json.Json

import models.User
import shared.utils._
import shared.model.{ Club, Competition, Player, License }

class UUIS(value : String) {

    def fromByteArray(buffer: Array[Byte], offset: Int=0): UUID =
    new UUID(
      (buffer(offset     )       ).toLong << 56
    | (buffer(offset +  1) & 0xff).toLong << 48
    | (buffer(offset +  2) & 0xff).toLong << 40
    | (buffer(offset +  3) & 0xff).toLong << 32
    | (buffer(offset +  4) & 0xff).toLong << 24
    | (buffer(offset +  5) & 0xff)        << 16
    | (buffer(offset +  6) & 0xff)        <<  8
    | (buffer(offset +  7) & 0xff)
    , (buffer(offset +  8)       ).toLong << 56
    | (buffer(offset +  9) & 0xff).toLong << 48
    | (buffer(offset + 10) & 0xff).toLong << 40
    | (buffer(offset + 11) & 0xff).toLong << 32
    | (buffer(offset + 12) & 0xff).toLong << 24
    | (buffer(offset + 13) & 0xff)        << 16
    | (buffer(offset + 14) & 0xff)        <<  8
    | (buffer(offset + 15) & 0xff)
    )
    
  override def toString = this.value
  def toUUID() = fromByteArray(Base64.getUrlDecoder.decode(this.value.getBytes(StandardCharsets.UTF_8)))
}


object UUIS {
  private def toByteArray(uuid: UUID): Array[Byte] = {
    val bb = ByteBuffer.allocate(16)
    bb.putLong(uuid.getMostSignificantBits())
    bb.putLong(uuid.getLeastSignificantBits())
    bb.array
  }
   def apply(x:String) = new UUIS(x)
   //def init = new UUIS(String(Base64.getUrlEncoder.withoutPadding.encode( toByteArray(UUID.fromString("00000000-0000-0000-0000-000000000000") )))
   //def random  = new UUIS(String(Base64.getUrlEncoder.withoutPadding.encode( toByteArray(UUID.randomUUID()) )))
   def randStr = new String(Base64.getUrlEncoder.withoutPadding.encode( toByteArray(UUID.randomUUID()) ))
   //def fromUUID(uStr: String) = UUIS(String(Base64.getUrlEncoder.withoutPadding.encode( toByteArray(UUID.fromString(uStr) )))
   //def fromUUID(uuid: UUID) =
}


object Crypto {
  var rml:        Boolean = true  //runModeLocal: Boolean = false,  local/privat server (no silhouette login) or cloud server (silhouette/google login)
  var accessCtrl: Boolean = true

  private val SALT:     String = "jMhKlOuJnM34G6NHkqo9V010GhLAqOpF0BePojHgh1HgNg8^72k" 
  private val UUIDNull: String = "00000000-0000-0000-0000-000000000000" 

  def hex2bytes(hex: String): Array[Byte] =
    //hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    hex.replaceAll("[^0-9A-Fa-f]", "").toSeq.sliding(2, 2).map(_.unwrap).toArray.map(Integer.parseInt(_, 16).toByte)


  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String =
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _    => bytes.map("%02x".format(_)).mkString(sep.get)
    }

  def xor(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
    require(a.length == b.length, "Byte arrays have to have the same length")
    (a.toList zip b.toList).map(elements => (elements._1 ^ elements._2).toByte).toArray
  }

  /**
    * Computes the SHA-1 digest for a byte array.
    *
    * @param bytes the data to hash
    * @return the SHA-1 digest, encoded as a hex string
    */
  def sha1(bytes: Array[Byte]): String = {
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("SHA-1")
    digest.reset()
    digest.update(bytes)
    digest.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }
  }

  def toSHA1(convertme: Array[Byte]): String = {
    import java.security.MessageDigest

    var md: MessageDigest = null
    try md = MessageDigest.getInstance("SHA-1")
    catch { case _: Throwable => ""} 
    new String(md.digest(convertme))
  }


  /**
    * Compute the SHA-1 digest for a `String`.
    *
    * @param text the text to hash
    * @return the SHA-1 digest, encoded as a hex string
    */
  //def sha1(text: String): String = sha1(xor(text.getBytes.slice(0,10),text.getBytes.slice(10,20)))
  def sha1(text: String): String = sha1(text.getBytes)

  /** generate Licensestring = 'ClubName / LicenseCode'
    *  append integer for license type
    */
  def genLicense(club: String, full: Boolean): String = {
    val ltyp = if (full) 2 else 1
    val lcode = hex2bytes(sha1(club.trim + "!" + f"${ltyp}%01d"))
    club.trim + "/" + (bytes2hex(xor(lcode.slice(0, 10), lcode.slice(10, 20)))).toUpperCase
  }
  
  /** get License Type
   *
   * return - 0 if license is invalid
   * return - 1,2,3 if valid license
   */
  def getLicenseType(licString: Option[String]): Int = {
    licString match {
      case None       => 0
      case Some(lSt)  => {
        val club = lSt.split("/")(0)        
        if (genLicense(club, false) == lSt) 1
        else if (genLicense(club, true) == lSt) 2
        else 0 
      }
    }
  }
  
  /** get License Type from basic Authentication
   *
   * return - 0 if authentication is invalid
   * return - 1,2,3 if authentication is valid
   */
  def getLicenseTypeFromBasicAuth(authorization: Option[String]): Int = {
    authorization match {
      case None        => 0
      case Some(auth)  => {
        allCatch.opt {
          val authStr = auth.split(" ").drop(1).headOption.getOrElse("x")
          val licStr = new String(Base64.getDecoder.decode(authStr.getBytes(StandardCharsets.UTF_8))).split(":").toList.mkString("/")
          getLicenseType(Some(licStr))
        } getOrElse 0
      }
    }
  } 
  

  def getPasswordFromBasicAuth(authorization: Option[String]): String = {
    authorization match {
      case None        => "#"
      case Some(auth)  => {
        allCatch.opt { 
          auth.split(" ").drop(1).headOption.getOrElse("#") 
        } getOrElse "#"
      }
    }
  } 

  /** crc32Hex - generates crc32 based hash value to unique identify 
   *             player, clubs and competitions
   *  return - hash value with prefix
   */  
  def crc32Hex(s: String, prefix: String =""): String = {
    val crc = new CRC32()
    val sb = new StringBuilder

    crc.update(s.getBytes())
    val bArray = BigInt(crc.getValue).toByteArray
    
    // take the last 4 Bytes
    for (b <- bArray.drop(bArray.length-4)) {
      sb.append(String.format("%02x", Byte.box(b)).toUpperCase)
    }
    prefix + sb.toString
  }

  /** get License Code from basic Authentication
   *
   * return - 0 if authentication is invalid
   * return - 1,2,3 if authentication is valid
   */
  def getLicenseCodeFromBasicAuth(authorization: Option[String]): String = {
    authorization match {
      case None        => ""
      case Some(auth)  => {
        allCatch.opt {
          val authStr = auth.split(" ").drop(1).headOption.getOrElse("")
          val licStr = new String(Base64.getDecoder.decode(authStr.getBytes(StandardCharsets.UTF_8))).split(":").toList
          licStr(1)
        } getOrElse ""
      }
    }
  }

  /** get UUID of default user/admin on local installations
   *
   * return - UUID
   */  
  def getLocalUserUUID(): UUID = UUID.fromString("66b04fc4-dca8-425d-b08c-11c33814c6d2")
  def getExternalUserUUID(): UUID = UUID.fromString("67c04fc4-dca8-415d-b09c-11c33815c6d2")
  def sessionKey(): String = "x656mnszut4xxkl"
 
  // encrypt AES
  def encrypt(key: String, value: String): String = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, keyToSpec(key))
    new String(Base64.getEncoder.encode(cipher.doFinal(value.getBytes((StandardCharsets.UTF_8)))))
  }

  // decrypt AES
  def decrypt(key: String, encryptedValue: String): String = {
    val cipher: Cipher = Cipher.getInstance("AES/ECB/PKCS5PADDING")
    cipher.init(Cipher.DECRYPT_MODE, keyToSpec(key))
    new String(cipher.doFinal(
        Base64.getDecoder.decode(encryptedValue.getBytes(StandardCharsets.UTF_8))   
    ))
  }

  def keyToSpec(key: String): SecretKeySpec = {
    var keyBytes: Array[Byte] = (SALT + key).getBytes("UTF-8")
    val sha: MessageDigest = MessageDigest.getInstance("SHA-1")
    keyBytes = sha.digest(keyBytes)
    keyBytes = util.Arrays.copyOf(keyBytes, 16)
    new SecretKeySpec(keyBytes, "AES")
  }

  /** genSessionBase64 - generate session base64 encoded
    * 
    * @return session base64 encoded
    */
  def genSessionBase64(): String = {
    implicit val sessionFmt  = Json.format[shared.utils.Session] 
    val ctxJs = Json.toJson(Session("", false, "", "", 0L,"", "", rml, new Date().getTime() / 1000)).toString
    new String(Base64.getEncoder.encode(ctxJs.getBytes(StandardCharsets.UTF_8)))
  }  

  def genSessionBase64(session: Session): String = {
    implicit val sessionFmt  = Json.format[shared.utils.Session] 
    val ctxJs = Json.toJson(session).toString
    new String(Base64.getEncoder.encode(ctxJs.getBytes(StandardCharsets.UTF_8)))
  }
  
  def genSessionBase64(lic: Option[License], admin: Boolean = false): String = {
    implicit val sessionFmt  = Json.format[shared.utils.Session] 
    lic match {
      case Some(l) => {
        val session = Session(l.uuid, admin, l.email, l.name, l.id, l.club, l.orgDir, rml, new Date().getTime() / 1000)
        val ctxJs = Json.toJson(session).toString
        new String(Base64.getEncoder.encode(ctxJs.getBytes(StandardCharsets.UTF_8)))
      }
      case None    => genSessionBase64()
    }
  }
  
  def genSessionCookie(lic: Option[License], admin: Boolean = false): String = {
    implicit val sessionFmt  = Json.format[shared.utils.Session]
    val cky = lic match {
      case Some(l) => Json.toJson(Session(l.uuid, admin, l.email, l.name, l.id, l.club, l.orgDir, rml, new Date().getTime() / 1000)).toString
      case None    => Json.toJson(Session()).toString
    }
    encrypt(sessionKey(), cky) 
  }  
  

  def genErrorCookie(cookieStr: String): String = {
    new String(Base64.getEncoder.encode(cookieStr.getBytes(StandardCharsets.UTF_8)))
  }  
  
  
  /** getSessionFromCookie - get session context from cookie
   *
   */  
  def getSessionFromCookie(cookie: Option[Cookie], msgs: Messages): Session = {
    implicit val sessionFmt  = Json.format[shared.utils.Session]
    
    cookie match {
      case None     => 
        Session.invalid("err0134.crypto.NoSessionCookie", rml)  
      case Some(c)  => {
        val res = allCatch.opt {
          val session = Json.parse(decrypt(sessionKey(), c.value)).as[Session]
          val curTime = new Date().getTime() / 1000
          if ( (curTime - session.loginTime ) < 86400L) {
            session
          } else {
            Session.invalid("err0135.crypto.SessionTimeout", rml)  
          }
        } 
        res match {
          case None    => Session.invalid("err0136.crypto.SessionDecoding", rml)           
          case Some(s) => s
        }
      }
    }
  }  
 
  
  def checkAccess(trnyOrgDir: String, callerOrgDir:String, writeAccess:Boolean): Boolean = {
    !accessCtrl | (trnyOrgDir == callerOrgDir) | !writeAccess
  }
  

  def string2UUID(uuid : String) : UUID = {
    allCatch.opt { UUID.fromString(uuid) }.getOrElse { UUID.fromString(UUIDNull) }
  }

  def parseXParam(url: String): HashMap[String,String] = {
    var hm: HashMap[String,String] = new HashMap()
    var cnt = 0

    try {
      val params = url.split("&")
      for (param <- params) {
        val m =  param.split("=", 2).map(s => URLDecoder.decode(s, "UTF-8"))
        if (m.length == 2) {
          hm += (m(0) -> m(1))
          cnt = cnt + 1
        } 
      }
    } catch { case _: Throwable => hm += ("__error__" -> "true") } 
    hm += ("__count__" -> cnt.toString)
  }

  def encParam(urlEnc: String): HashMap[String, String] = parseXParam(urlEnc.replace("_~2C~_",",").replace("_~3D~_","=").replace("_~4F~_","&"))


  def getParam(paramMap: HashMap[String,String], key: String, defValue: String=""): String = 
    try paramMap(key) catch { case _: Throwable =>  defValue }
  
  def getParam(paramMap: HashMap[String,String], key: String, defValue: Int): Int = 
    try paramMap(key).toInt catch { case _: Throwable =>  defValue }

  def getParam(paramMap: HashMap[String,String], key: String, defValue: Long): Long = 
    try paramMap(key).toLong catch { case _: Throwable =>  defValue }

  def getParam(paramMap: HashMap[String,String], key: String, defValue: Boolean): Boolean = 
    try paramMap(key).toBoolean catch { case _: Throwable =>  defValue }


  def encEParam(urlEnc: String): Either[Error, HashMap[String,String]] = {
    val hm = parseXParam(urlEnc.replace("_~2C~_",",").replace("_~3D~_","=").replace("_~4F~_","&"))
    if (hm.contains("__error__")) Left(Error("err0138.crypto.paramlist")) else Right(hm)
  }

  def getEParam(paramMap: HashMap[String,String], key: String): Either[Error, String] = 
    try Right(paramMap(key)) catch { case _: Throwable =>  Left(Error("err0139.crypto.StringParam", key)) } 
  
  def getEParam(paramMap: HashMap[String,String], key: String, default: Long): Either[Error, Long] = 
    try Right(paramMap(key).toLong) catch { case _: Throwable =>  Left(Error("err0140.crypto.LongParam", key)) } 

  def getEParam(paramMap: HashMap[String,String], key: String, default: Int): Either[Error, Int] = 
    try Right(paramMap(key).toInt) catch { case _: Throwable =>  Left(Error("rr0141.crypto.IntParam", key)) } 

  def getEParam(paramMap: HashMap[String,String], key: String, default: Boolean): Either[Error, Boolean] = 
    try Right(paramMap(key).toBoolean) catch { case _: Throwable =>  Left(Error("err0142.crypto.Boolean", key)) } 

  def genUUID() : String = UUID.randomUUID().toString()

}