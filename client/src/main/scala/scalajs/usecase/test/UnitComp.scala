package scalajs.usecase.home

// Start TestCases
// --------------------------------------------------------------------------------------------------
// DlgCardPlayer:       http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardPlayer&ucInfo=show
// OrganizeCompetition: http://localhost:9000/start?ucName=TestMain&ucParam=OrganizeCompetition

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}


//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service imports
import shared.utils._
import shared.utils.Routines._

import shared.model._
import shared.utils.Constants._
import shared.model.tabletennis._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object UnitComp extends UseCase("UnitComp") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{

  def render(testCase: String = "", testOption: String = "", reload: Boolean=false) = {} 

  def testCompDraw(testCase: String, testOption: String = "161") = {
    val toId = testOption.toLong
    authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasic", s"${err.encode}")
      case Right(sessionCtx) => {
        info("authBasic", s"successfull ctx: ${sessionCtx}")
        AppEnv.initContext(sessionCtx)
        App.loadRemoteTourney(toId) map {
          case Left(err)     => error("loadRemoteTourney", s"${err.encode}")
          case Right(result) => {
            val coId = 1L
            info("loadRemoteTourney", App.tourney.toString)
            App.tourney.addSect(0, coId, "Vorrunde", CST_GRPS45) match {
              case Left(err)    => error("addSect", s"${err.encode}")
              case Right(secId) => {
                //(coId: Long, secId: Int, pEntries: ArrayBuffer[ParticipantEntry]): Either[Error, Int] = { 
                val pants = App.tourney.pl2co.keys
                  .filter(x => (x._2 == coId))
                  .filter(x => App.tourney.pl2co(x).status == 1)
                  .map( x => App.tourney.pl2co(x).getEntry(App.tourney.players, CT_SINGLE) )

                App.tourney.setSectPlayer(coId, secId, pants.toArray) match {
                  case Left(err)    => error("setSectPlayer", s"${err.toString}")
                  case Right(cnt) => {
                    info("addSectPlayer", s"noPlayer: ${cnt}") 
                    App.execUseCase("OrganizeCompetitionDraw", "", "")   
                  }
                }              
              }
            } 
          } 
        }
      }  
    }
  }  


  /** CompSection - encode and decode CompSection
   *  call from browser console: Test.start("CompSection", "")
   * 
   *  class CompSection(var name: String, val coId: Long, val coSystem: Int, val coTyp: Int, val noWinSets: Int)
   * 
   *  competition system (competition phases types)
   *     val CSY_UNKN     =  0  
   *     val CSY_GR       =  1   // beliebige Gruppen-Spielphase
   *     val CSY_KO       =  2   // KO-Spielphase
   *     val CSY_JGJ      =  3   // Gruppe Jeder-gegen-Jeden
   *     val CSY_GRPS3    =  4   // Gruppensystem mit 3er
   *     val CSY_GRPS34   =  6   // Gruppensystem mit 3er und 4er
   *     val CSY_GRPS4    =  7   // Gruppensystem
   *     val CSY_GRPS45   =  8   // Gruppensystem
   *     val CSY_GRPS5    =  9   // Gruppensystem
   *     val CSY_GRPS56   = 10   // Gruppensystem
   *     val CSY_GRPS6    = 11   // Gruppensystem
   *     val CSY_SW       = 12   // Switzsystem
   * 
   *  competition typ
   *     val CT_UNKN   = 0
   *     val CT_SINGLE = 1
   *     val CT_DOUBLE = 2
   *     val CT_MIXED  = 3
   *     val CT_TEAM   = 4 
   */

  //class CompSection(var name: String, val coId: Long, val coSystem: Int, val coTyp: Int, val noWinSets: Int) {

  def testSection(testCase: String, testOption: String) = {
    import scala.collection.mutable.ArrayBuffer

    val id      = 1
    val preId   = 0
    val winId   = 0
    val looId   = 0
    val name    = "VORRUNDE"
    val coId    = 13L
    val winSets = 4

    //class CompSection(id: Int, preId: Int, coId: Long, name: String, secTyp: Int)
    val coSect1 = new CompSection(id, 0, coId, name, CST_GRPS5)
    coSect1.pants = Array(
      ParticipantEntry("131", "Lichtenegger1, Robert1", "TTC Freising1", 1201, (0,0)),
      ParticipantEntry("132", "Lichtenegger2, Robert2", "TTC Freising2", 1202, (0,0)),
      ParticipantEntry("133", "Lichtenegger3, Robert3", "TTC Freising3", 1203, (0,0)),
      ParticipantEntry("134", "Lichtenegger4, Robert4", "TTC Freising4", 1204, (0,0)),
      ParticipantEntry("135", "Lichtenegger5, Robert5", "TTC Freising5", 1205, (0,0)),
      ParticipantEntry("136", "Lichtenegger6, Robert6", "TTC Freising6", 1206, (0,0)),
      ParticipantEntry("137", "Lichtenegger7, Robert7", "TTC Freising7", 1207, (0,0)),
      ParticipantEntry("138", "Lichtenegger8, Robert8", "TTC Freising8", 1208, (0,0)),
      ParticipantEntry("139", "Lichtenegger9, Robert9", "TTC Freising9", 1209, (0,0)),
      ParticipantEntry(PLID_BYE.toString, "BYE", "-", 0, (0,0))
    )
    coSect1.noPlayer = 10

    // CompSection(id, 0, coId, name, CST_GRPS5)
    val coSect2 = new CompSection(2, id, coId, "KO-ENDRUNDE", CSY_KO)

    val coSect3 = new CompSection(3, id, coId, "TROSTRUNDE", CSY_KO)

    val coSect4 = new CompSection(4, 0, coId, "SCHWEIZER SYSTEM", CSY_KO)


    coSect4.setMatches(1, 
      ArrayBuffer(
        //         stNoA,  stNoB,   coId, coPh,  gameNo, round, maNo, grId, wgw,   playfield, info, startTime, endTime, status, result 
        MatchEntry("00131","00132", 13L,  CST_SW, 1,      1,     1,    0,    (0,0), "",        "",   "",        "",      0,      "3·2·3·4"),
        MatchEntry("00133","00134", 13L,  CST_SW, 2,      1,     2,    0,    (0,0), "",        "",   "",        "",      0,      "4·2·-2·3·4"),
        MatchEntry("00135","00136", 13L,  CST_SW, 3,      1,     3,    0,    (0,0), "",        "",   "",        "",      0,      "3·9·3·4"),
        MatchEntry("00137","00138", 13L,  CST_SW, 4,      1,     4,    0,    (0,0), "",        "",   "",        "",      0,      "5·2·-8·-12·3·4")
      ))
    coSect4.setMatches(2, 
      ArrayBuffer(
        //         stNoA,  stNoB,   coId, coPh,  gameNo, round, maNo, grId, wgw,   playfield, info, startTime, endTime, status, result 
        MatchEntry("00132","00133", 13L,  CST_SW, 5,      2,     1,    0,    (0,0), "",        "",   "",        "",      0,      "3·1·5·4"),
        MatchEntry("00134","00135", 13L,  CST_SW, 6,      2,     2,    0,    (0,0), "",        "",   "",        "",      0,      "4·2·-7·3·4"),
        MatchEntry("00136","00137", 13L,  CST_SW, 7,      2,     3,    0,    (0,0), "",        "",   "",        "",      0,      "3·7·3·4"),
        MatchEntry("00131","00138", 13L,  CST_SW, 8,      2,     4,    0,    (0,0), "",        "",   "",        "",      0,      "5·2·-4·-11·3·8")
      ))

    println(coSect4.toString) 
  }


  // testPhase - CompPhase
  def testPhase(testCase: String, testOption: String) = {

    // val coph = new CompPhase("Test Runde", 13, CP_SW, CSY_SW, true, 10, 9, 4)
    // val result = coph.init(Array(
    //       PlayerEntry("131", "Lichtenegger, Robert1", "TTC Freising1", (0,0)),
    //       PlayerEntry("132", "Lichtenegger, Robert2", "TTC Freising2", (0,0)),
    //       PlayerEntry("133", "Lichtenegger, Robert3", "TTC Freising3", (0,0)),
    //       PlayerEntry("134", "Lichtenegger, Robert4", "TTC Freising4", (0,0)),
    //       PlayerEntry("135", "Lichtenegger, Robert5", "TTC Freising5", (0,0)),
    //       PlayerEntry("136", "Lichtenegger, Robert6", "TTC Freising6", (0,0)),
    //       PlayerEntry("137", "Lichtenegger, Robert7", "TTC Freising7", (0,0)),
    //       PlayerEntry("138", "Lichtenegger, Robert8", "TTC Freising8", (0,0)),
    //       PlayerEntry("139", "Lichtenegger, Robert9", "TTC Freising9", (0,0))
    // ))
  }

}