
package tourn.services

import scala.xml._
import scala.xml.XML
import scala.xml.Elem
import scala.xml.Node
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer

import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
import play.api.i18n.Messages
import shared.model. { Competition, CompTyp, CompStatus, Player, Club, SexTyp }
import shared.utils.Routines._


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

class AddMatches(mtchs: Node, ageGroup: String, coTyp: CompTyp.Value, ttrF: Int, ttrT: Int, ttrR: String, startTime: String) extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case e: Elem if (e.label == "competition") => {
      val players = e \ "players"
      //val matches = <matches><match nr={mtchs}/></matches>  
      if (
          ageGroup.toLowerCase == e.attributes("age-group").toString.toLowerCase()             &&
          coTyp                == CttService.convCttTyp2CompTyp(e.attributes("type").toString) &&
          ttrF                 == e.attributes("ttr-from").toString.toIntOption.getOrElse(0)   &&
          ttrT                 == e.attributes("ttr-to").toString.toIntOption.getOrElse(0)     &&
          ttrR.toLowerCase     == e.attributes("ttr-remarks").toString.toLowerCase()           &&
          startTime            == parseStartTime(e.attributes("start-date").toString)
          ) {
        println(s"Attribs: ${e.attributes("age-group")}")
        Elem(e.prefix, e.label, e.attributes, e.scope, true, (players ++ mtchs).toSeq:_* )
      } else { e }
    }    
    case x => x  
  }  
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
    var buf = new StringBuilder(s"Tournament: $name [$ident] ($startDate to $endDate)\n")
    competitions.zipWithIndex.foreach {
      case(co, count) => { buf.append(s"  [$count]: ${co}\n") } 
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

  def getTyp = CttService.convCttTyp2CompTyp(typVal)

  def matchWith(ageGrp: String, coTyp: CompTyp.Value, ttrF: Int, ttrT: Int, ttrR: String, startTime: String): Boolean = {
    (coTyp  == getTyp)                             && 
    (ttrF   == ttrFrom.toIntOption.getOrElse(0))   &&
    (ttrT   == ttrTo.toIntOption.getOrElse(0))     &&
    (ttrR.toLowerCase   == ttrRemarks.toLowerCase) &&
    (ageGrp.toLowerCase == ageGroup.toLowerCase)   &&
    (startTime ==  parseStartTime(startDate))
  }

}

class CttPlayer(val typVal: String, val id: String, val persons: Array[CttPerson], val teamName: String = "", val teamNr: String = "", val placement: String = "")
{  
  override def toString: String = {
    var buf = new StringBuilder(s"Player: $typVal $id\n")
    persons.zipWithIndex.foreach { case(prson, count) => buf.append(s"          [$count]: ${prson}\n") }
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

  def getBY(byear: String) : Int = {
    val year = byear.toIntOption.getOrElse(0) 
    if (year >= 1900 & year < 2100) year else 0
  } 

  def convCttTyp2CompTyp(compTyp: String) = compTyp.toLowerCase match {
    case "einzel" | "single" => CompTyp.SINGLE
    case "doppel" | "double" => CompTyp.DOUBLE
    case "mixed"             => CompTyp.MIXED
    case "team"              => CompTyp.TEAM
    case _                   => CompTyp.UNKN
  } 
  
  // cttPers2Player convert ctt person to player
  //                ctt player is implemented by playerComp mapping table
  def cttPers2Player(cttp: CttPerson) : Player = {
    val pl = new Player(0L, "", 0L, cttp.clubName, cttp.firstname, cttp.lastname, getBY(cttp.birthyear), "", SexTyp(cttp.sex), "_")

    pl.setInternalNr(cttp.internalNr)
    pl.setLicenceNr(cttp.licenceNr)
    pl.setClubNr(cttp.clubNr)
    pl.setClubFedNick(cttp.clubFederationNickname)
    pl.setTTR(cttp.ttr)
    pl.setTTRMatchCnt(cttp.ttrMatchCount)
    pl.setNationality(cttp.nationality)
    pl.setForEqState(cttp.foreignerEqState)
    pl.setRegion(cttp.region)
    pl.setSubRegion(cttp.subRegion)
    pl
  }

  
  // cttComp2Comp convert ctt specific competition to competition
  def cttComp2Comp(cttComp: CttCompetition)(implicit msg: Messages) : Competition = {
    val tVal = cttComp.getTyp
    val name = 
      if (cttComp.ttrRemarks=="") {
        s"${cttComp.ageGroup} ${msg("competition.typ."+tVal)}"
      } else {
        s"${cttComp.ageGroup} ${cttComp.ttrRemarks} ${msg("competition.typ."+tVal)}"
      }

    val co = new Competition(0L, "", name, tVal, parseStartTime(cttComp.startDate), CompStatus.READY, "_")

    co.setAgeGroup(cttComp.ageGroup)
    co.setRatingRemark(cttComp.ttrRemarks)
    co.setRatingLowLevel(cttComp.ttrFrom.toIntOption.getOrElse(0))
    co.setRatingUpperLevel(cttComp.ttrTo.toIntOption.getOrElse(0)) 
    co.setSex(cttComp.sex.toIntOption.getOrElse(0))
    co.setMaxPerson(cttComp.maxPersons.toIntOption.getOrElse(0))
    co.setEntryFee(cttComp.entryFee)
    co.setPreRndMod(cttComp.preliminaryRoundPlaymode)
    co.setFinRndMod(cttComp.finalRoundPlaymode)
    co.setManFinRank(cttComp.manualFinalRankings)
    co
  } 
  
  // generate CttTournament from xmlObject
  def readCttTourney(xmlObject: scala.xml.Elem): CttTournament = {
    val listOfTournaments = new ListBuffer[CttTournament]
      
    val tourney = xmlObject \\ "tournament" 
    tourney.map { tourney =>
        val listOfCompetitions = new ListBuffer[CttCompetition]

        // tournament.winningSets                    = tourney \@ "winning-sets"
        // tournament.winningSetsText                = tourney \@ "winning-sets-text"
        // tournament.multipleParticipationsSameDay  = tourney \@ "multiple-participations-same-day"
        // tournament.multipleParticipationsSameTime = tourney \@ "multiple-participations-same-time"
        // tournament.tableCount                     = tourney \@ "table-count"
        // tournament.teamFormation                  = tourney \@ "team-formation"
        
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
          tourney \@ "team-formation"
        )      
      }
      // there should be only on ...
      listOfTournaments.head
    }
  

  def load(fName: String, xmlStr: String="") : Either[shared.utils.Error, CttTournament] = {
    try {
      val xmlObject = if (xmlStr =="") MyXML.loadFile(fName) else MyXML.loadString(xmlStr)
      Right(readCttTourney(xmlObject))
    } catch {
      case e: java.io.FileNotFoundException => Left(shared.utils.Error("xxx"))  // println("Couldn't find that file.")
      case e: java.io.IOException           => Left(shared.utils.Error("xxx"))  // println("Had an IOException trying to read that file")
      case _: Throwable                     => Left(shared.utils.Error("xxx"))  // println("Had an IOException trying to read that file")
    }
  }  
   
    // addResults adds result to competiton
    // usage: 
    //   val res = addResults(clickTTdoc, <matches><match nr="3"/></matches>, "Herren")
    //   saveClickTT(res, "CTTResult.xml")
    def addResults(node: Node, matches: Node, 
                   ageGroup: String, coTyp: CompTyp.Value, ttrF: Int, ttrT: Int, ttrR: String, startTime: String): scala.xml.Node = {
      val rule = new RuleTransformer(new AddMatches(matches, ageGroup, coTyp, ttrF, ttrT, ttrR, startTime))
      rule.transform(node).head
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

  def saveClickTT(node: Node, fileName: String) = {

    import java.nio.file.{Paths, Files}
    import java.nio.charset.StandardCharsets

    val Encoding = "UTF-8"
    val pp       = new scala.xml.PrettyPrinter(300, 2)
    val result   = new StringBuilder("<?xml version='1.0' encoding='" + Encoding + "'?>\n")
    result.append("<!DOCTYPE tournament SYSTEM 'http://www.datenautomaten.nu/dtd/nuLiga/TournamentPortal.dtd'>")
    result.append(pp.format(node))
    
    Files.write(Paths.get(fileName), result.toString.getBytes(StandardCharsets.UTF_8))
  }

}