package shared.utils
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

/*
 * Session Environment loaded after successful authentication
 */
case class Session(
  userId:       String  = "",
  admin:        Boolean = false,
  email:        String  = "",
  fullName:     String  = "",
  orgId:        Long    = 0L,      // id of organizer 
  organizer:    String  = "",      // name of organizer
  orgDir:       String  = "",      // unique name/dir of organizer
  runModeLocal: Boolean = false,   // local/privat server (no silhouette login) or cloud server (silhouette/google login)
  loginTime:    Long    = 0L,      // time in seconds 1970
  code:         Long    = 0L,      // >= 0 -> session is valid
  text:         String  = ""       // contains e.g. error msgs
)

object Session {
  implicit def rw: RW[Session] = macroRW
  def get(rml: Boolean) = new Session("", false, "", "", 0L, "", "", rml, 0L, 0L, "")
  def invalid(msgCode: String, rml: Boolean = false) = new Session("", false, "", "", 0L, "", "", rml, 0L, -1L, msgCode)
  def getDemo() = new Session("",false,"info@turnier-service.org", "Lichtenegger, Robert", 1L, "TTC Demo e.V.", "ttcdemo", false, 0L, 1L, "")
}
