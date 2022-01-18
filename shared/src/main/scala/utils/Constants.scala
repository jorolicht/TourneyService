package shared.utils

package object Constants {

  // special player identifier
  val PLID_BYE = 99999
  val PLID_NN  = 99998
  val SNO_BYE  = "99999"

  // competition type
  val CT_UNKN   = 0
  val CT_SINGLE = 1
  val CT_DOUBLE = 2
  val CT_MIXED  = 3
  val CT_TEAM   = 4

  // competition phases type
  val CP_UNKN = -99
  val CP_INIT =   0
  val CP_VRGR =   1
  val CP_ZRGR =   2
  val CP_ERGR =   3
  val CP_TRGR =   4
  val CP_ERBO =   3     // Endrunde: KO oder Gruppe
  val CP_TRBO =   4     // Trostrunde: KO oder Gruppe
  val CP_LEER =   5     // LEER
  val CP_VRKO =   6     // not yet available
  val CP_ZRKO =   7     // not yet available
  val CP_ERKO =   8     // only final KO round
  val CP_TRKO =   9     // nur Trostrunde KO

  // competition section type 
  val CST_UNKN     = -99
  val CST_GR3to9   = 100   // beliebige Gruppen-Spielphase 3-9er Gruppen
  val CST_GRPS3    = 101   // Gruppensystem mit 3er
  val CST_GRPS34   = 102   // Gruppensystem mit 3er und 4er
  val CST_GRPS4    = 103   // Gruppensystem mit 4er
  val CST_GRPS45   = 104   // Gruppensystem mit 4er und 5er
  val CST_GRPS5    = 105   // Gruppensystem mit 5er
  val CST_GRPS56   = 106   // Gruppensystem mit 5er und 6er
  val CST_GRPS6    = 107   // Gruppensystem mit 6er
  val CST_JGJ      = 108   // Gruppe Jeder-gegen-Jeden
  val CST_KO       = 109   // KO-Spielphase 
  val CST_SW       = 110   // Switzsystem

  // competition system
  val CSY_UNKN = -99
  val CSY_KO   =   1
  val CSY_GR   =   2
  val CSY_SW   =   3


  // competition (section) status types
  val CSS_AUS   = 101 // Auslosung (Section)
  val CSS_EIN   = 102 // Eingabe   (Section)
  val CSS_FIN   = 103 // Finished  (Section) 

  val CS_UNKN  = -99 // unknown status
  val CS_WEBRE =  -2 // Registrierung / Anmeldung via WEB
  val CS_REGIS =  -1 // Registrierung / Anmeldung
  val CS_RESET =   0 // RESET
  val CS_RUN   =   1 // RUNNING  
  
  val CS_VRAUS = 1   // Auslosung der Vorrunde
  val CS_VREIN = 2   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CS_VRFIN = 3   // Vorrunde beendet, Auslosung ZR oder ER kann erfolgen

  val CS_ZRAUS = 4   // Auslosung der Zwischenrunde
  val CS_ZREIN = 5   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CS_ZRFIN = 6   // Zwischenrunde beendet, Auslosung ER kann erfolgen
  
  val CS_ERAUS = 7   // Auslosung der Endrunde
  val CS_EREIN = 8   // Auslosung erfolgt, Eingabe der Ergebnisse
  val CS_ERFIN = 9   // Endrunde beendet ...

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


  def csy2Name(x: Int): String = {
    x match {
      case CST_UNKN     => "unknown"
      case CST_GR3to9   => "Gruppenspiele"
      case CST_KO       => "KO-Runde"
      case CST_JGJ      => "Jeder-gegen-Jeden"
      case CST_GRPS3    => "3er Gruppen"
      case CST_GRPS34   => "3er und 4er Gruppen"
      case CST_GRPS4    => "4er Gruppen"
      case CST_GRPS45   => "4er und 5er Gruppen"
      case CST_GRPS5    => "5er Gruppen"
      case CST_GRPS56   => "5er und 6er Gruppen"
      case CST_GRPS6    => "6er Gruppen"
      case CST_SW       => "Schweizer System"
      case _            => "unknown"
    }
  }

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

}