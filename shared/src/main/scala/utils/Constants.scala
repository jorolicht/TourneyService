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


  val PANT_SELECT_ALL = 3
  val PANT_SELECT_WINNER = 2
  val PANT_SELECT_LOOSER = 1

}