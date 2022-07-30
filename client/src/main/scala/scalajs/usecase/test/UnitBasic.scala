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

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object UnitBasic extends UseCase("UnitBasic") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{

  def render(testCase: String = "", testOption: String = "", reload: Boolean=false) = {} 

  // testSpinner
  def testSpinner(testCase: String, testOption: String) = {
    setHtml_("mainContent", clientviews.component.html.Spinner("Bitte warten ...").toString)
  }

  // testSidebar
  def testSidebar(testCase: String, testOption: String) = {
    try { ctrlSidebar(AppEnv.status) }
    catch { case _: Throwable => debug("test_Sidebar", s"exception") }
    setHtml_("mainContent", s"""<pre>setSidebar(${testOption})</pre>""")
  }


  // testDump
  def testDump(testCase: String, testOption: String) = {
    testOption.toLowerCase match {
      case "table"       => println(s"""${App.tourney.playfields.toString}""")
      case "competition" => println(s"""Test \n Competition""")
      case _             => println(s"""${App.tourney.toString}""")
    }
  }



  // testMainContent
  def testMainContent(testCase: String, testOption: String) = {
    val testInfo = s"Test ${testCase}(Option: ${testOption})"
    setHtml_("mainContent", s"Message: ${testOption}") 
  }  


  // testHLists - check shapeless HLists
  def testHLists(testCase: String, testOption: String) = {
    import upickle.default._
    import shapeless._
    val testInfo = s"Test ${testCase}(Option: ${testOption})" 

    case class IceCream(name: String, numCherries: Int, inCone: Boolean) {
      def stringify  = write((name,numCherries,inCone)) 
    }

    object IceCream {
      //type IceCreamTyp = (String, Int, Boolean)
      def tupled = (IceCream.apply _).tupled
      //def obify(x: String) = Generic[IceCream].from(Generic[IceCreamTyp].to(read[IceCreamTyp](x)))
      def obify(x: String) = tupled(read[(String, Int, Boolean)](x))
    }

    type IceCreamTyp = (String, Int, Boolean)
    val iceCreamCGen = Generic[IceCream]
    val iceCreamTGen = Generic[IceCreamTyp]

    val iceCream = IceCream("Vanille", 5, false)

    val res6 = IceCream.tupled(("ddd",4,false))

    // iceCream: IceCream = IceCream(Sundae,1,false)
    val repr = iceCreamCGen.to(iceCream)
    // repr: iceCreamGen.Repr = Sundae :: 1 :: false :: HNil
    val iceCream2 = iceCreamCGen.from(repr)

    val iceCreamFromTuppel = iceCreamCGen.from(iceCreamTGen.to(("Erdbeere",9,true)) )
    val tupleGen = Generic[(String, Int, Boolean)]

    val res1 = tupleGen.to(("Hello", 123, true))
    // res4: tupleGen.Repr = Hello :: 123 :: true :: HNil
    val res2 = tupleGen.from("Hello" :: 123 :: true :: HNil)
    // res5: (String, Int, Boolean) = (Hello,123,true)

    val res3  = IceCream("Schoko",12, false)
    val res3x = res3.stringify
    val res4  = IceCream.obify(res3x)
    
    info("testHLists", s"${testInfo} -> res0: ${iceCreamFromTuppel}")
    info("testHLists", s"${testInfo} -> res1: ${res1}")
    info("testHLists", s"${testInfo} -> res2: ${res2}")
    info("testHLists", s"${testInfo} -> res3: ${res3} / ${res3.stringify}")
    info("testHLists", s"${testInfo} -> res4: ${res4}")
  }  

  // testEitherT - check cats monad transformer
  def testEitherT(testCase: String, testOption: String) = {
    import cats.data.EitherT
    import cats.implicits._

    val testInfo = s"Test ${testCase}(Option: ${testOption})" 
    
    def getFutureLeft(x: Int): Future[Either[String,Int]] = {
      info("testShapeless", s"evaluate getFutureLeft(${x})")
      Future.successful(Left(x.toString))
    }

    def getFutureRight(x: Int): Future[Either[String,Int]] = {
      info("testShapeless", s"evaluate getFutureRight(${x})")
      Future.successful(Right(x))
    }

    val futureRes = for {
      //a <- EitherT(getFutureLeft(9))
      b <- EitherT(getFutureRight(10))
      c <- EitherT(getFutureRight(42))
    } yield c

    futureRes.value.map { _ match {
      case Left(err)     => {
        info("testShapeless", s"${testInfo} -> succeeded")
        //setHtml_("mainContent", clientviews.home.test.html.Info(testInfo, s"succeeded with ${err}").toString)
      }
      case Right(result) => {
        info("testShapeless", s"${testInfo} -> succeeded")
        //setHtml_("mainContent", clientviews.home.test.html.Info(testInfo, s"succeeded with ${result}").toString)        
      }
    }}
  }




}
