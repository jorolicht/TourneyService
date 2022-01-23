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


object UIDlg extends UseCase("UIDlg") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  def render(testCase: String = "", testOption: String = "", reload: Boolean=false) = {} 

  // Start TestCases
  // DlgCardCfgSection:   http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardCfgSection&ucInfo=161#
  // DlgCardRegSingle:    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardRegSingle&ucInfo=#
  // DlgCardRegDouble:    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardRegDouble&ucInfo=#
  // DlgCardPlayer:       http://localhost:9000/start?ucName=TestMain&ucParam=Player&ucInfo=#


  // testDlgCardCfgSection
  def testDlgCardCfgSection(testCase: String, testOption: String) = {
    val size = testOption.toInt
    val toId = 161L  //testOption.toLong
    App.loadRemoteTourney(toId) map {
      case Left(err)     => error("testDlgCardCfgSection", s"Can't load tourney")
      case Right(result) => {
        debug("DlgCardCfgSection", App.tourney.toString) 

        val pants = Array(
          ParticipantEntry("131", "Lichtenegger1, Robert1", "TTC Freising1", 1201, (0,0), true),
          ParticipantEntry("132", "Lichtenegger2, Robert2", "TTC Freising2", 1202, (0,0), true),
          ParticipantEntry("133", "Lichtenegger3, Robert3", "TTC Freising3", 1203, (0,0), true),
          ParticipantEntry("134", "Lichtenegger4, Robert4", "TTC Freising4", 1204, (0,0), true),
          ParticipantEntry("135", "Lichtenegger5, Robert5", "TTC Freising5", 1205, (0,0), true),
          ParticipantEntry("136", "Lichtenegger6, Robert6", "TTC Freising6", 1206, (0,0), true),
          ParticipantEntry("137", "Lichtenegger7, Robert7", "TTC Freising7", 1207, (0,0), true),
          ParticipantEntry("138", "Lichtenegger8, Robert8", "TTC Freising8", 1208, (0,0), true),
          ParticipantEntry("139", "Lichtenegger9, Robert9", "TTC Freising9", 1209, (0,0), true),
          ParticipantEntry(PLID_BYE.toString, "BYE", "-", 0, (0,0), true)
        )

        //def show(coId: Long, secId: Int, size:Int, trny: Tourney,  lang: String): Future[Either[Error, Int]] = {
        DlgCardCfgSection.show(1L, 1, size, "DE", pants)(App.tourney) map {
          case Left(err)  => println(s"testCase: ${testCase} error  -> ${err}") 
          case Right(res) => println(s"testCase: ${testCase} result: ${res}") 
        }
      } 
    }
  }  


  // def show(coId: Long, secId: Int, size:Int, lang: String, pants: Array[ParticipantEntry])
  //         (implicit trny: Tourney): Future[Either[Error, Int]] = {  

// case class ParticipantEntry(
//   var sno:    String,         // start number(s) concatenated string of player identifieres  
//   val name:   String,                     
//   val club:   String, 
//   val rating: Int,            // eg. ttr for table tennis
//   var place:  (Int,Int),      // position after finishing the round (group or ko)
// ) {




  // startCardTourney - DlgCardTourney
  def testDlgCardTourney(testCase: String, testOption: String) =
    testOption.toLowerCase() match {
      case "new" => DlgCardTourney.show("new", TournBase("TestTourneyName", "TestClub 007"," testclub007",
                                         20210801, 20210808, "Ident007", TT_TT, false, "Doe·John·089-4566689·test.user@email.com",
                                        "Turnhalle an der Ampler·Germany·85456·Freising·Riegerauer Weg 10", 0L))            
         .map     { retValue  => println(s"testCase: ${testCase} return: ${retValue}") }
         .recover { case e: Exception =>  println(s"testCase: ${testCase} error: ${e.getMessage}") }  
      case "edit"  => DlgCardTourney.show("edit", TournBase("TestTourneyName", "TestClub 007"," testclub007",
                                          20210801, 20210808, "Ident007", TT_TT, false, "Doe·John·089-4566689·test.user@email.com",
                                          "Turnhalle an der Ampler·Germany·85456·Freising·Riegerauer Weg 10", 0L))               
         .map     { retValue  => println(s"testCase: ${testCase} return: ${retValue}") }
         .recover { case e: Exception =>  println(s"testCase: ${testCase} error: ${e.getMessage}") }  
      case "view"  => DlgCardTourney.show("view", TournBase("TestTourneyName", "TestClub 007"," testclub007",
                                           20210801, 20210808, "Ident007", TT_TT, false, "Doe·John·089-4566689·test.user@email.com",
                                          "Turnhalle an der Ampler·Germany·85456·Freising·Riegerauer Weg 10", 0L))                
         .map     { retValue  => println(s"testCase: ${testCase} return: ${retValue}") }
         .recover { case e: Exception =>  println(s"testCase: ${testCase} error: ${e.getMessage}") }  
    } 



  // testDlgCardComp
  def testDlgCardComp(testCase: String, testOption: String) =
    testOption.toLowerCase() match {
      case "view" => DlgCardComp.show(Competition(11l, 12, "Competition Name SHOW", 1, "20210731#1215", 0, ""), App.tourney, AppEnv.getLang, DlgOption.View )               
         .map     { retValue  => println(s"testCase: ${testCase} return: ${retValue}") }
         .recover { case e: Exception =>  println(s"testCase: ${testCase} error: ${e.getMessage}") }  
      case "edit"  => DlgCardComp.show(Competition(11l, 12, "Competition Name EDIT", 1, "20210731#1215", 0, ""), App.tourney, AppEnv.getLang, DlgOption.Edit )               
         .map     { retValue  => println(s"testCase: ${testCase} return: ${retValue}") }
         .recover { case e: Exception =>  println(s"testCase: ${testCase} error: ${e.getMessage}") }  
      case "new"  => DlgCardComp.show(Competition(0L, 0, "", 0, "", -99, ""), App.tourney, AppEnv.getLang, DlgOption.New )               
         .map     { retValue  => println(s"testCase: ${testCase} return: ${retValue}") }
         .recover { case e: Exception =>  println(s"testCase: ${testCase} error: ${e.getMessage}") }  
    }


  // testDlgCardRegSingle
  def testDlgCardRegSingle(testCase: String, testOption: String) = {
    App.loadRemoteTourney(159) map {
      case Left(err)     => error("test_DlgCardRegSingle", s"Can't load tourney")
      case Right(result) => {
        debug("DlgCardRegSingle", App.tourney.toString) 
        App.tourney.players(2).setTTR("3000")
        DlgCardRegSingle.show(11, App.tourney, "DE") map {
          case Left(err)  => println(s"testCase: ${testCase} error  -> ${err}") 
          case Right(res) => println(s"testCase: ${testCase} result -> player: ${res._1} status: ${res._2}") 
        }
      } 
    }
  }  

  // testDlgCardRegDouble - DlgCardRegDouble
  def testDlgCardRegDouble(testCase: String, testOption: String) = App.loadRemoteTourney(159) map {
    case Left(err)     => error("test_DlgCardRegDouble", s"Can't load tourney")
    case Right(result) => {
      debug("DlgCardRegDouble", App.tourney.toString) 
      DlgCardRegDouble.show(11, App.tourney, "DE") map {
        case Left(err)  => println(s"testCase: ${testCase} error  -> ${err}") 
        case Right(res) => println(s"testCase: ${testCase} result -> player: ${res._1} status: ${res._2}") 
      }
    } 
  }


  // testDlg - Dialog
  def testDlg(testCase: String, testOption: String) = {
    val testInfo = s"Test ${testCase}(Option: ${testOption})"
    setHtml_("mainContent", s"Message: ${testOption}") 
    testOption match {
      case "Info" => DlgInfo.show("TEST Title", "TEST BODY", "danger")
      case _      => error("test_Dialog", testInfo)
    }
  }  
  
  
}