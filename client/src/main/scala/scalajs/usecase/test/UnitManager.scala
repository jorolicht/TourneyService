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


object UnitManager extends UseCase("UnitManager") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{

  def render(testCase: String = "", testOption: String = "", reload: Boolean=false) = {} 


  // testRegister
  def testRegister(testCase: String, testOption: String) = {
    implicit val ucp=UseCaseParam("HomeRegister", "home.register", "HomeRegister", "home-register", BasicHtml.getMsg_ _ ) 

    val data = ujson.read(""" { "name":"otto" } """) 

    val testInfo = s"Test ${testCase}(Option: ${testOption})"

    // val url = "https://raw.githubusercontent.com/mozilla/pdf.js/ba2edeae/examples/learning/helloworld.pdf"
    // val canvasId = "the-canvas"
    // AppJs.testAdd(url, canvasId) 

    debug("test_Register", s"exec pdf") 

    setHtml_("mainContent", clientviews.home.test.html.Register(testInfo, "Simulate FullRequest finished", "20210029_Invoice_0047.pdf", "licstring/kjlfklökjlkj").toString)
    setVisibleByAttr("view", "full-finish", true)
    setHtml("FullLicenseCode", "licstring/test")
  }  

  // testInvoice
  def testInvoice(testCase: String, testOption: String) = {
    implicit val ucp=UseCaseParam("HomeRegister", "home.register", "HomeRegister", "home-register", BasicHtml.getMsg_ _ ) 

    val contact = Contact("Lichtenegger", "Robert", "01514125", "robert.lichtenegger@icloud.com")
    val address = Address("home","Germany","85356","Freising","Finkenstr. 36a")
    val orgId   = 99998
    val orgDir  = "demo"
    genInvoice(contact: Contact, address: Address, orgDir: String, orgId: Long).map {
      case Left(err)    => setHtml_("mainContent", s"""<samp><b>${testCase}(${testOption}) </b>Error:  ${getError(err)}</samp>""")
      case Right(value) => setHtml_("mainContent", s"""<samp><b>${testCase}(${testOption}) </b>Result: ${value}</samp>""")
    }
  }

  // testSendFullLicense
  def testSendFullLicense(testCase: String, testOption: String) = {
    import cats.data.EitherT
    import cats.implicits._    

    //(generated license, orgId, orgDir)
    val contact = Contact("Doe", "John", "001-777654", "robert.lichtenegger@gmail.com")
    //val contact = Contact("Doe", "John", "001-777654", "John.Doe@example.info")
    val address = Address("home","Germany","85356","Freising","Riegerauer Weg 29")
    val licReq = LicRequest(contact.getName(1), contact.email, address.encode, "Abcd0815", testOption, false, true)

    (for {
      licInfo <- EitherT(requestLicense(licReq))
      invoice <- EitherT(genInvoice(contact, address, licInfo._3, licInfo._2))
      result  <- EitherT(sendLicense(licInfo._2, invoice))
    } yield { result }).value.map {
      case Left(err) => setHtml_("mainContent", s"""<samp><b>${testCase}(${testOption}) </b>Error:  ${getError(err)}</samp>""")
      case Right(value) => setHtml_("mainContent", s"""<samp><b>${testCase}(${testOption}) </b>Result: ${value}</samp>""")
    }
  }

  // authBasicContext
  def testAuthBasicContext(testCase: String, testOption: String) = {    
    authBasicContext("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasicContext", s"Error: ${err}") 
      case Right(res) => info("authBasicContext", s"Success: ${res}")
    } 
  }  

  // authBasicContext
  def testAuthBasic(testCase: String, testOption: String) = {    
    authBasic("robert.lichtengger@icloud.com", "", "Si5d4H").map {
      case Left(err)  => error("authBasic", s"Error: ${err}") 
      case Right(res) => info("authBasic", s"Success: ${res}")
    } 
  }  

  /** testLicense
   * case class LicRequest(
   *   name:         String,
   *   email:        String,
   *   address:      String,
   *   password:     String,
   *   club:         String, 
   *   allowContact: Boolean,
   *   fullVersion:  Boolean
   * )
   */ 
  def testLicense(testCase: String, testOption: String) = {
    val testInfo = s"Test ${testCase}(Option: ${testOption})"

    val contact = Contact("Doe", "John", "001-777654", "John.Doe@example.info")
    val address = Address("home","Germany","85356","Freising","Riegerauer Weg 29")

    val licReq = LicRequest(contact.getName(1), contact.email, address.encode, "Abcd0815", testOption, false, true)
    requestLicense(licReq).map {
      case Left(error)   => setHtml_("mainContent", s"""<samp><b>${testCase}(${testOption}) </b>Error:  ${getError(error)}</samp>""")
      case Right(value) => setHtml_("mainContent", s"""<samp><b>${testCase}(${testOption}) </b>Result: ${value}</samp>""")
    }
  }

  // testGoogleLogin
  def testGoogleLogin(testCase: String, testOption: String) = {
    val testInfo = s"Test ${testCase}(Option: ${testOption})"
    dom.window.location.replace("/authenticate/google")
  }    



}
