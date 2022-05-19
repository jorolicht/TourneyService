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
import shared.model.CompPhase._
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
                // val pants = App.tourney.pl2co.keys
                //   .filter(x => (x._2 == coId))
                //   .filter(x => App.tourney.pl2co(x).status == 1)
                //   .map( x => App.tourney.pl2co(x).getEntry(App.tourney.players, CT_SINGLE) )



                // App.tourney.setSectPlayer(coId, secId, pants.toArray) match {
                //   case Left(err)    => error("setSectPlayer", s"${err.toString}")
                //   case Right(cnt) => {
                //     info("addSectPlayer", s"noPlayer: ${cnt}") 
                //     App.execUseCase("OrganizeCompetitionDraw", "", "")   
                //   }
                // }              
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
   *     val CPT_UNKN     =  0  
   *     val CPT_GR       =  1   // beliebige Gruppen-Spielphase
   *     val CPT_KO       =  2   // KO-Spielphase
   *     val CPT_JGJ      =  3   // Gruppe Jeder-gegen-Jeden
   *     val CPT_GRPS3    =  4   // Gruppensystem mit 3er
   *     val CPT_GRPS34   =  6   // Gruppensystem mit 3er und 4er
   *     val CPT_GRPS4    =  7   // Gruppensystem
   *     val CPT_GRPS45   =  8   // Gruppensystem
   *     val CPT_GRPS5    =  9   // Gruppensystem
   *     val CPT_GRPS56   = 10   // Gruppensystem
   *     val CPT_GRPS6    = 11   // Gruppensystem
   *     val CPT_SW       = 12   // Switzsystem
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
    coSect1.noPlayer = 10

    // CompSection(id, 0, coId, name, CST_GRPS5)
    val coSect2 = new CompSection(2, id, coId, "KO-ENDRUNDE", CPT_KO)
    val coSect3 = new CompSection(3, id, coId, "TROSTRUNDE", CPT_KO)
    val coSect4 = new CompSection(4, 0, coId, "SCHWEIZER SYSTEM", CPT_KO)

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

  }




}
