package shared.utils

package object Constants {

  val MAX_RATING = 5000
 
  // competition (section) status types
  val CSS_AUS   = 101 // Auslosung (Section)
  val CSS_EIN   = 102 // Eingabe   (Section)
  val CSS_FIN   = 103 // Finished  (Section) 



  // player status
  val PLS_UNKN = -99
  val PLS_RJEC = -3  // rejected
  val PLS_WAIT = -2  // waiting list 
  val PLS_SICO = -1  // pending signup confirmation
  val PLS_SIGN =  0  // signup confirmed
  val PLS_REDY =  1  // participation confirmed
  val PLS_PLAY =  2  // currently playing
  val PLS_FINE =  3  // competition finished

  // tourney type
  val TT_UNKN  = -1  // unknown
  val TT_ANY   =  0  // any
  val TT_TT    =  1  // table tennis 
  val TT_SK    =  2  // schafkopf

  // configuration file types / upload type
  val ULD_CLICKTT = "UploadClickTT"     // ClickTT Participant File
  val ULD_INVIT   = "UploadInvitation"  // Description of invitation in markdown format
  val ULD_LOGO    = "UploadLogo"        // Logo picture file of club 
  val ULD_CERT    = "UploadCert"        // File for certificate print
  val ULD_RESULT  = "UploadResult"      // Result file (TT format)
  val ULD_BANNER  = "UploadBanner"      // Club Banner

}