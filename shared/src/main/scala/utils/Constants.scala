package shared.utils

package object Constants {

  val MAX_RATING = 5000

  val UCP0 = UseCaseParam("","","","",(x:String,y:Seq[String])=>"")
  
  // configuration file types / upload type
  val ULD_CLICKTT = "UploadClickTT"     // ClickTT Participant File
  val ULD_INVIT   = "UploadInvitation"  // Description of invitation in markdown format
  val ULD_LOGO    = "UploadLogo"        // Logo picture file of club 
  val ULD_CERT    = "UploadCert"        // File for certificate print
  val ULD_RESULT  = "UploadResult"      // Result file (TT format)
  val ULD_BANNER  = "UploadBanner"      // Club Banner

  val UploadModeUnknown = 0
  val UploadModeUpdate  = 1
  val UploadModeNew     = 2

  // checkUploadExt - check whether required upload type
  //                  matches with its extension
  def checkUploadExt(ext: String, uloadTyp: String): Boolean = {
    uloadTyp match {
      case ULD_CLICKTT => (ext == "xml") 
      case ULD_INVIT   => (ext == "md") 
      case ULD_LOGO    => (ext == "png") | (ext == "jpg") | (ext == "gif") 
      case ULD_CERT    => (ext == "png")
      case ULD_RESULT  => (ext == "csv")
      case ULD_BANNER  => (ext == "png")
      case _ => false
    }
  }

}