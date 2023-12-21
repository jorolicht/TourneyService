package shared.model

import shared.utils.CSVConverter

case class CttPerson(
  firstname: String,                        // 0
  lastname: String,                         // 1      
  birthyear: String,                        // 2
  internalNr: String,                       // 3
  licenceNr: String,                        // 4
  sex:Int,                                  // 5
  var clubName:String="",                   // 6
  var clubNr: String="",                    // 7
  var clubFederationNickname: String="",    // 8
  var ttr: String="",                       // 9
  var ttrMatchCount:String="",              // 10
  var nationality:String="",                // 11
  var foreignerEqState: String="",          // 12
  var region: String="",                    // 13
  var subRegion: String=""                  // 14
) {


  // toPlayer convert ctt person to player
  def toPlayer(plId: Long = 0L, clubId: Long = 0L, email: String ="") : Player = {
    val pl = new Player(plId, clubId, clubName, firstname, lastname, getBY(), email, SexTyp(sex), "_")
   
    pl.setInternalNr(internalNr)
    pl.setLicense(CttLicense(licenceNr))
    pl.setClubNr(clubNr)
    pl.setClubFedNick(clubFederationNickname)
    pl.setTTR(ttr)
    pl.setTTRMatchCnt(ttrMatchCount)
    pl.setNationality(nationality)
    pl.setForEqState(foreignerEqState)
    pl.setRegion(region)
    pl.setSubRegion(subRegion)
    pl
  }

  def getBY() : Int = {
    val year = birthyear.toIntOption.getOrElse(0) 
    if (year >= 1900 & year < 2100) year else 0
  } 

  def toCsv(): String = CSVConverter[CttPerson].to(this)

}

case class CttPersonCsv(value: String) {
  def get: Either[shared.utils.Error, CttPerson] =  {
    import scala.util.{Try, Success, Failure}
    import shared.utils.Error
    CSVConverter[CttPerson].from(value) match {
      case Success(ctp) => Right(ctp)
      case Failure(e)   => Left(Error("err0214.decode.CttPerson", value)) 
    }
  }
  
  def valid = if (value == "") false else {
    import scala.util.{Try, Success, Failure}
    import shared.utils.Error
    CSVConverter[CttPerson].from(value) match {
      case Success(ctp) => true
      case Failure(e)   => false
    }  
  }

}

case class CttLicense(value: String)


// object CttPerson {

//   def fromCsv(csv: String): Either[shared.utils.Error, CttPerson] = {
//     import scala.util.{Try, Success, Failure}
//     import shared.utils.Error
//     CSVConverter[CttPerson].from(csv) match {
//       case Success(ctp) => Right(ctp)
//       case Failure(e)   => Left(Error("err0214.decode.CttPerson", csv)) 
//     }
//   }

// }