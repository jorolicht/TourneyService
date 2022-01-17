package scalajs.usecase.home

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
import shared.model.tabletennis._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


@JSExportTopLevel("TestConsole")
object TestConsole extends UseCase("TestConsole") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{

  def start(testCase: String = "", testOption: String = "") = {
    testCase.toLowerCase match {
      
      case "authbasic"       => test_authBasic(testCase, testOption)
      case "addtournbase"    => test_addTournBase(testCase, testOption)
      case "requestlicense"  => test_requestLicense(testCase, testOption)
      case "dlgtournbase"    => test_DlgTournBase(testCase, testOption)
      case "dlgshowerror"    => test_DlgShowError(testCase, testOption)
      //case "dlgcardplayer"   => test_DlgCardPlayer(testCase, testOption) 
      case _                 => HomeConsole.add(s"testCase: ${testCase} testOption: ${testOption} unknown")
    }  
  }

  def render(ucParam: String = "", testOption: String = "", reload: Boolean=false) = {}


  /** test_dlgShowError: ${testCase} 
   * 
   * @param testCase
   * @param testOption
   */
  def test_DlgShowError(testCase: String, testOption: String) = {
    testOption match {
      case _        => 
        DlgShowError.show(List(Error("err.one"), Error("err.two")))               
          .map     { retValue  => HomeConsole.add(s"testCase: ${testCase} returns: ${retValue}") }
          .recover { case e: Exception =>  HomeConsole.add(s"testCase: ${testCase} error: ${e.getMessage}") }   
    }
  }


  /** test_requestLicense
   * 
   * @param testCase
   * @param testOption
   */
  def test_requestLicense(testCase: String, testOption: String) = {
    import org.encoding.Base64._
    import shared.utils.Constants._

    val addr = Address("", "country", "zip", "city", "street")
    val pw = "Abc123"
    val licReq = LicRequest("Test, User", "test.user@email.de", addr.stringify, pw.getBytes.toBase64, "Test Club 007", false, false)

    requestLicense(licReq).map {
      case Left(err)  => HomeConsole.add(s"requestLicense error: ${err.msgCode}")
      case Right(res) => HomeConsole.add(s"requestLicense successful: license=${res._1} orgId=${res._2} orgDir=${res._3}")
    }
  }  

  /** test_dlgTournBase
   * 
   * @param testCase
   * @param testOption
   */
  def test_DlgTournBase(testCase: String, testOption: String) = {
    import shared.utils.Constants._

    // DlgTournBase.show("Hallo", TournBase.dummy("", "", 20220101, TT_TT))
    //   .map(tB => HomeConsole.add(s"testCase: ${testCase} result: ${tB}"))
    //   .recover {case e: Exception =>  HomeConsole.add(s"testCase: ${testCase} error: ${e.getMessage}") }   


    testOption match {
      case "edit"  =>
        DlgCardTourney.show("edit", TournBase("TestTourneyName", "TestClub 007"," testclub007",
          20210801, 20210808, "Ident007", TT_TT, false, "Doe·John·089-4566689·test.user@email.com",
                                            "Turnhalle an der Ampler·Germany·85456·Freising·Riegerauer Weg 10", 0L))                    
          .map {
            case Left(err)  => HomeConsole.add(s"testCase: ${testCase} error: ${getError(err)}")
            case Right(tB)  => HomeConsole.add(s"testCase: ${testCase} result: ${tB}")
          }

      case ""        => 
        DlgCardTourney.show("new", TournBase("TestTourneyName", "TestClub 007"," testclub007",
          20210801, 20210808, "Ident007", -1, false, "Doe·John·089-4566689·test.user@email.com",
                                            "Turnhalle an der Ampler·Germany·85456·Freising·Riegerauer Weg 10", 0L))                    
          .map {
            case Left(err)  => HomeConsole.add(s"testCase: ${testCase} error: ${getError(err)}")
            case Right(tB)  => HomeConsole.add(s"testCase: ${testCase} result: ${tB}")
          }
    }
  }

  /** test_authBasic
   * 
   * @param testCase
   * @param testOption
   */
  def test_authBasic(testCase: String, testOption: String) = {
    authBasic("test.user@email.de", "", "Abc123").map {
      case Left(err)         => HomeConsole.add(s"authBasic error: ${err.msgCode}")
      case Right(sessionCtx) => {
        AppEnv.initContext(sessionCtx)
        HomeConsole.add("authBasic successful")
      }  
    }
  }


  /** test_addTournBase
   * 
   * @param testCase
   * @param testOption
   */
  def test_addTournBase(testCase: String, testOption: String) = {
    import shared.utils.Constants._

    val tb0 = TournBase("Test Turnier - Variante 0001", "Test Club 007", "testclub007", 
                        20210607, 20210608, "ClickId_1", TT_TT, false,
                        "Lichtenegger·Robert·0049-1577-6434434·info@turnier-service.com·",
                        "Turnhalle Grundschule St. Landbert·Germany·85356·Freising·St. Landbert Str. 4·")
    val tb1 = TournBase("Test Turnier - Variante 0002", "Test Club 007", "testclub007", 
                        20210707, 20210708, "ClickId_2", TT_TT, false,
                        "Lichtenegger·Robert·0049-1577-6434434·info@turnier-service.com·",
                        "Turnhalle Grundschule St. Landbert·Germany·85356·Freising·St. Landbert Str. 4·")

    authBasic("test.user@email.de", "", "Abc123").map {
      case Left(err)         => HomeConsole.add(s"authBasic error: ${err.msgCode}")
      case Right(sessionCtx) => {
        AppEnv.initContext(sessionCtx)
        HomeConsole.add("authBasic successful")

        addTournBase(tb0).map { 
          case Left(err)   => HomeConsole.add(s"addTournBase error: ${err.msgCode}")
          case Right(trny) => HomeConsole.add(s"addTournBase successfull: [${trny.id}] ${trny.name} ")
        }
        addTournBase(tb1).map { 
          case Left(err)   => HomeConsole.add(s"addTournBase error: ${err.msgCode}")
          case Right(trny) => HomeConsole.add(s"addTournBase successfull: [${trny.id}] ${trny.name} ")
        } 
      }  
    }
  }



  /** test_sendFullLicense
   * 
   * @param testCase
   * @param testOption
   */
  def test_sendFullLicense(testCase: String, testOption: String) = {
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
  
}
