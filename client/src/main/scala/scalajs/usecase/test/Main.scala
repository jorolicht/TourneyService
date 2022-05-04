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


@JSExportTopLevel("Test")
object TestMain extends UseCase("TestMain") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  // Start TestCases
  // DlgCardRegSingle:    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardRegSingle&ucInfo=#
  // DlgCardRegDouble:    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardRegDouble&ucInfo=#
  // DlgCardPlayer:       http://localhost:9000/start?ucName=TestMain&ucParam=Player&ucInfo=#
  // OrgComp:             http://localhost:9000/start?ucName=TestMain&ucParam=OrgComp
  // OrgCompDraw:         http://localhost:9000/start?ucName=TestMain&ucParam=OrgCompDraw
  // AddTournCTT:         http://localhost:9000/start?ucName=TestMain&ucParam=AddTournCTT


  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = start(testCase, testOption)

  @JSExport
  def scalaTest() = "Hello World!"

  @JSExport
  def exec(testCase:String = "") = start(testCase)

  @JSExport
  def start(testCase:String = "", testOption:String = "") = {
    info(testCase, s"--> START")
    val result = testCase match {
      // UnitBasic
      case "Dump" | "dump"       => UnitBasic.testDump(testCase, testOption); true
      case "Spinner" | "spinner" => UnitBasic.testSpinner(testCase, testOption); true
      case "Sidebar" | "sidebar" => UnitBasic.testSidebar(testCase, testOption); true
      case "Now"  | "now"        => println(s"Now: ${getNow()}"); true
      case "MainContent"         => UnitBasic.testMainContent(testCase, testOption); true
      case "EitherT"             => UnitBasic.testEitherT(testCase, testOption); true
      case "HLists"              => UnitBasic.testHLists(testCase, testOption); true

      //UIDlg
      case "DlgCardCfgSection"   => UIDlg.testDlgCardCfgSection(testCase, testOption); true
      case "DlgCardRegSingle"    => UIDlg.testDlgCardRegSingle(testCase, testOption); true
      case "DlgCardRegDouble"    => UIDlg.testDlgCardRegDouble(testCase, testOption); true
      case "DlgCardComp"         => UIDlg.testDlgCardComp(testCase, testOption); true
      case "DlgCardTourney"      => UIDlg.testDlgCardTourney(testCase, testOption); true
      case "Dlg"                 => UIDlg.testDlg(testCase, testOption); true

      //UnitComp
      case "CompPhase"           => UnitComp.testPhase(testCase, testOption); true
      case "CompSection"         => UnitComp.testSection(testCase, testOption); true

      //UIOrg
      case "OrgComp"             => UIOrg.testComp(testCase, testOption); true
      case "OrgCompDraw"         => UIOrg.testCompDraw(testCase, testOption); true
      case "OrgCompView"         => UIOrg.testCompView(testCase, testOption); true
      case "OrgCompInput"        => UIOrg.testCompInput(testCase, testOption); true
      case "OrgTrny"             => UIOrg.testTrny(testCase, testOption); true

      //UnitManager
      case "authBasic"  | "AuthBasic"              => UnitManager.testAuthBasic(testCase, testOption); true
      case "authReset"                             => UnitManager.testAuthReset(testCase, testOption); true
      case "getEmail"                              => UnitManager.testGetEmail(testCase, testOption); true
      case "authBasicContext" | "AuthBasicContext" => UnitManager.testAuthBasicContext(testCase, testOption); true  
      case "Google"                                => UnitManager.testGoogleLogin(testCase, testOption); true
      case "License"                               => UnitManager.testLicense(testCase, testOption); true      
      case "sendFullLicense"                       => UnitManager.testSendFullLicense(testCase, testOption); true
      case "Invoice"                               => UnitManager.testInvoice(testCase, testOption); true
      case "Register"                              => UnitManager.testRegister(testCase, testOption); true


      //UnitTourney
      case "AddTournCTT"  | "addTournCTT"          => UnitTourney.testAddTournCTT(testCase, testOption); true

      //UnitPlayer
      case "addPlayer"           => UnitPlayer.testAddPlayer(testCase, testOption); true

      //various tests
      case "Card"                => test_Card(testCase, testOption); true
      case "UserAware"           => test_UserAware(testCase, testOption); true

      case _                     => info("render", s"testCase: ${testCase} testOption: ${testOption} - unknown testcase"); false
    }
    if (result) info(testCase, s"--> TEST SUCCEEDED") else error(testCase, s"--> TEST FAILED") 
    result
  }


  /** test_Card
   * 
   * @param testCase
   * @param testOption
   */
  def test_Card(testCase: String, testOption: String) = {
    // import clientviews.component.helper.html
    import clientviews.home.test.html.Info

    val testInfo = s"Test ${testCase}(Option: ${testOption})"
    setMainContent( clientviews.component.helper.html.card(
                    "TeSt", "TestHeadline", Info(testInfo, "Card Test Message"), "ReqHallo", "req.hallo").toString )
  } 
  

  /** test_UserAware - check silhouette user aware handling
    * 
    * @param testCase
    * @param testOption
    */
  def test_UserAware(testCase: String, testOption: String) = {
    val testInfo = s"Test ${testCase}(Option: ${testOption})"
    getJson("/authenticate/userAware","user=User&test=true").map { 
      case Left(err)     => info("test_Register", s"${testInfo} -> request result: ${err}")
      case Right(result) => info("test_Register", s"${testInfo} -> request result: ${result}")
    }
  }


}

// Calling nativ javascript code  
@js.native
@JSGlobal("AppJs")
object AppJs extends js.Object {
  def testAdd(url: String, id: String): js.Any = js.native
}