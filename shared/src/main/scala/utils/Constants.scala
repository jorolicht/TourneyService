package shared.utils

object TourneyAction extends Enumeration { 
  val UNKNOWN   = Value("UNKNOWN")
  val New       = Value("New")
  val Edit      = Value("Edit")
  val Delete    = Value("Delete")
  val View      = Value("View")
  val Select    = Value("Select")
}

object DownloadType extends Enumeration { 
  val UNKNOWN   = Value("UNKNOWN")
  val Player    = Value("Player")
  val ClickTT   = Value("ClickTT")
}

object UploadType extends Enumeration { 
  val UNKNOWN   = Value("UNKNOWN")   // 0
  val ClickTT   = Value("ClickTT")   // 1 - ClickTT Participant File
  val Invite    = Value("Invite")    // 2 - Description of invitation in markdown format
  val Logo      = Value("Logo")      // 3 - Logo picture file of club 
  val Cert      = Value("Cert")      // 4 - File for certificate print
  val Banner    = Value("Banner")    // 5 - Club Banner


  // checkUploadExt - check whether required upload type
  //                  matches with its extension
  def checkExtension(ext: String, uplType: UploadType.Value): Boolean = {
    uplType match {
      case UploadType.ClickTT => (ext == "xml") 
      case UploadType.Invite  => (ext == "md") 
      case UploadType.Logo    => (ext == "png") | (ext == "jpg") | (ext == "gif") 
      case UploadType.Cert    => (ext == "png")
      case UploadType.Banner  => (ext == "png")
      case UploadType.UNKNOWN => false
    }
  }
}

object UploadMode extends Enumeration { 
  val UNKNOWN   = Value  // 0
  val Update    = Value  // 1 
  val New       = Value  // 2 
}

object PlayerSortTyp extends Enumeration { 
  val UNKNOWN     = Value(-0, "UNKNOWN")    // 0
  val SNO         = Value(1, "SNO")         // 1 
  val Name        = Value(2, "Name")        // 2
  val Competition = Value(3, "Competition") // 3 
  val Status      = Value(4, "Status")      // 4 
}


package object Constants {

  val MAX_RATING = 5000

  val UCP0 = UseCaseParam("","","","",(x:String,y:Seq[String])=>"")

}