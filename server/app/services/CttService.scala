
package tourn.services

import scala.xml._
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
import play.api.i18n.Messages
import shared.model. { Competition, Player, Club }


// Define a new enumeration with a type alias and work with the 
// full set of enumerated values
object CttCompType extends Enumeration {
  type CompType = Value
  val Einzel, Doppel, Mixed, Mannschaft = Value
}

object CttPlayerType extends Enumeration {
  type PlayerType = Value
  val single, double = Value
}

case class CttTournamentLocation(name: String, street:String, zipCode: String, city: String)

class CttTournament(
                val name:       String, 
                val startDate:  String, 
                val endDate:    String, 
                val ident:      String,
                var competitions: List[CttCompetition],
                var tournamentLocation: Option[CttTournamentLocation] = None,
                var winningSets: String="", 
                var winningSetsText: String="",
                var multipleParticipationsSameDay: String="",
                var multipleParticipationsSameTime: String="",
                var tableCount: String ="",
                var teamFormation: String = "",   
               ) {
  
  override def toString: String = {
    var buf = new StringBuilder(
      s"Tournament: $name [$ident] ($startDate to $endDate)\n" 
    )
    competitions.zipWithIndex.foreach {
      case(co, count) => {
        buf.append(s"  [$count]: ${co}\n")       
      } 
    }
    buf.toString  
  }

  // getPersons remove duplicates
  def getPersons: List[CttPerson] = {
    val result = for {
      co   <- competitions
      pl   <- co.players
      ps   <- pl.persons
    } yield ps
    result.distinct
  } 
    
  //getClubs remove duplicates  
  def getClubs: List[Club] = {
    val result = for {
      co   <- competitions
      pl   <- co.players
      ps   <- pl.persons
    } yield { 
      val cl = new Club(ps.clubName) 
      cl.setClubNr(ps.clubNr)
      cl.setClubFedNick(ps.clubFederationNickname)
      cl
    }
    result.distinct
  }
}

class CttCompetition(
                  val ageGroup:                 String, 
                  val typVal:                   String, 
                  val startDate:                String,
                  var players:                  List[CttPlayer],
                  val ttrFrom:                  String="",
                  val ttrTo:                    String="",
                  val ttrRemarks:               String="",
                  var entryFee:                 String="",
                  var ageFrom:                  String="",
                  var ageTo  :                  String="",
                  var sex    :                  String="",
                  var preliminaryRoundPlaymode: String="",
                  var finalRoundPlaymode:       String="",
                  var maxPersons:               String="",
                  var manualFinalRankings:      String=""
                 ) {
   def count = players.length
   
   override def toString: String = {
     var buf = new StringBuilder(s"Competition: $ageGroup $typVal $startDate $ttrFrom $ttrTo\n")
     players.zipWithIndex.foreach {
       case(pl, count) => {
         buf.append(s"      [$count]: ${pl}\n")       
       } 
     }
     buf.toString  
   }
}

class CttPlayer(
             val typVal:    String,
             val id:        String,
             val persons:   Array[CttPerson],
             val teamName:  String = "",
             val teamNr  :  String = "",
             val placement: String = ""
            ) {
  
   override def toString: String = {
     var buf = new StringBuilder(s"Player: $typVal $id\n")
     persons.zipWithIndex.foreach {
       case(prson, count) => {
         buf.append(s"          [$count]: ${prson}\n")       
       } 
     }
     buf.toString  
   }
}

case class CttPerson(
  firstname: String, lastname: String, birthyear: String,
  internalNr: String, licenceNr: String, sex:Int,
  var clubName:String="",
  var clubNr: String="",
  var clubFederationNickname: String="",
  var ttr: String="", 
  var ttrMatchCount:String="", 
  var nationality:String="",
  var foreignerEqState: String="",
  var region: String="", 
  var subRegion: String=""
) 


object CttService  { 
  import scala.util.Try

  def tryToInt(s: String): Option[Int] = Try(s.toInt).toOption
  def toInt(s: String): Int = { try { s.toInt } catch { case e: Exception => 0 } }

  def tryToString(s: String): Option[String] = if (s.trim == "") None else Some(s.trim) 
  def getBY(byear: String) : Int = {
    val year = tryToInt(byear).getOrElse(0)
    if (year >= 1900 & year < 2100) year else 0
  } 

  
  // cttPers2Player convert ctt person to player
  //                ctt player is implemented by playerComp mapping table
  def cttPers2Player(cttp: CttPerson) : Player = {
    val pl = new Player(0L, "", 0L, cttp.clubName, cttp.firstname, cttp.lastname, getBY(cttp.birthyear), "", cttp.sex, "_")

    pl.setOpt(cttp.internalNr, 0)
    pl.setOpt(cttp.licenceNr, 1)
    pl.setOpt(cttp.clubNr, 2)
    pl.setOpt(cttp.clubFederationNickname, 3)
    pl.setOpt(cttp.ttr, 4)
    pl.setOpt(cttp.ttrMatchCount, 5)
    pl.setOpt(cttp.nationality, 6)
    pl.setOpt(cttp.foreignerEqState, 7)
    pl.setOpt(cttp.region, 8)
    pl.setOpt(cttp.subRegion, 9)
    pl
  }
  
  // cttComp2Comp convert ctt specific competition to competition
  def cttComp2Comp(cttComp: CttCompetition)(implicit msg: Messages) : Competition = {
    
    def parseTyp(value : String): Int = value.toLowerCase match {
      case "einzel" | "single" => 1
      case "doppel" | "double" => 2
      case "mixed"             => 3
      case "team"              => 4
      case _                   => 5
    } 
    
    val tVal = parseTyp(cttComp.typVal)

    val name = 
      if (cttComp.ttrRemarks=="") {
        s"${cttComp.ageGroup} ${msg("competition.typ."+tVal)}"
      } else {
        s"${cttComp.ageGroup} ${cttComp.ttrRemarks} ${msg("competition.typ."+tVal)}"
      }

    val co = new Competition(0L, "", name, tVal, Competition.parseStartTime(cttComp.startDate), 0, "_")

    co.setOpt(cttComp.ageGroup, 0)
    co.setOpt(cttComp.ttrRemarks, 1)
    co.setOpt(toInt(cttComp.ttrFrom), 2)
    co.setOpt(toInt(cttComp.ttrTo), 3)
    co.setOpt(toInt(cttComp.maxPersons), 4)
    co.setOpt(cttComp.entryFee, 5)
    co.setOpt(cttComp.ageFrom, 6)
    co.setOpt(cttComp.ageTo, 7)
    co.setOpt(cttComp.sex, 8)
    co.setOpt(cttComp.preliminaryRoundPlaymode, 9)
    co.setOpt(cttComp.finalRoundPlaymode, 10)
    co.setOpt(cttComp.manualFinalRankings, 11)
    co
  } 
  
  def load(fName: String, xmlString: String="") : CttTournament = {
    
    def initXML(fName: String, xmlStr: String) = if (xmlStr =="") { MyXML.loadFile(fName) } else { MyXML.loadString(xmlStr) }
    val xmlObject = initXML(fName, xmlString)
    val listOfTournaments = new ListBuffer[CttTournament]
    
     
    val tourney = xmlObject \\ "tournament" 
    tourney.map { tourney =>
      val listOfCompetitions = new ListBuffer[CttCompetition]
/*
      tournament.winningSets                    = tourney \@ "winning-sets"
      tournament.winningSetsText                = tourney \@ "winning-sets-text"
      tournament.multipleParticipationsSameDay  = tourney \@ "multiple-participations-same-day"
      tournament.multipleParticipationsSameTime = tourney \@ "multiple-participations-same-time"
      tournament.tableCount                     = tourney \@ "table-count"
      tournament.teamFormation                  = tourney \@ "team-formation"
      */
      
      val comp = tourney \ "competition"
      comp.map { comp =>
        val listOfPlayers = new ListBuffer[CttPlayer]
        
        val players = comp \ "players"
        players.map { players =>
          val player = players \ "player"
          val id = player \@ "id"
       
          player.map { player =>
            val arrOfPerson = new ArrayBuffer[CttPerson]
            
            val person = player \ "person"            
            person.zipWithIndex.foreach {
              case(person, count) => {
                arrOfPerson += new CttPerson( 
                  person  \@ "firstname",  
                  person  \@ "lastname",  
                  person  \@ "birthyear",  
                  person  \@ "internal-nr",  
                  person  \@ "licence-nr",  
                  (person  \@ "sex").toInt,  
                  person  \@ "club-name",  
                  person  \@ "club-nr",  
                  person  \@ "club-federation-nickname",  
                  person  \@ "ttr",  
                  person  \@ "ttr-match-count",
                  person  \@ "nationality", 
                  person  \@ "foreigner-eq-state", 
                  person  \@ "region",
                  person  \@ "sub-region"
                )  
              }
            }
            
            listOfPlayers += new CttPlayer(
              player \@ "type",
              player \@ "id", 
              arrOfPerson.toArray,
              player \@ "team-name", 
              player \@ "tema-nr", 
              player \@ "placement"
            )
          } 
        }
        
        listOfCompetitions += new CttCompetition(
          comp \@ "age-group",
          comp \@ "type", 
          comp \@ "start-date", 
          listOfPlayers.toList,
          comp \@ "ttr-from", 
          comp \@ "ttr-to", 
          comp \@ "ttr-remarks",
          comp \@ "entry-fee",
          comp \@ "age-from",
          comp \@ "age-to",
          comp \@ "sex",
          comp \@ "preliminary-round-playmode",
          comp \@ "final-round-playmode",
          comp \@ "max-persons",
          comp \@ "manual-final-rankings"
        )
      }
      
      listOfTournaments += new CttTournament(
        tourney \@ "name",
        tourney \@ "start-date",
        tourney \@ "end-date",
        tourney \@ "tournament-id",
        listOfCompetitions.toList,
        None,
        tourney \@ "winning-sets",
        tourney \@ "winning-sets-text",
        tourney \@ "multiple-participations-same-day",
        tourney \@ "multiple-participations-same-time",
        tourney \@ "team-formation",
        
      )      
    }
    // there should be only on ...
    listOfTournaments.head
   }
 }

object MyXML extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    //f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    f.newSAXParser()
  }
}
