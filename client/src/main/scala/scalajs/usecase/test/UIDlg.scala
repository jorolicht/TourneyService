package scalajs.usecase.home

// Start TestCases
// --------------------------------------------------------------------------------------------------
// DlgCardPlayer:       http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardPlayer&ucInfo=show
// OrganizeCompetition: http://localhost:9000/start?ucName=TestMain&ucParam=OrganizeCompetition
// DlgCardCfgSection:   http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardCfgSection&ucInfo=1#
// DlgCardRegSingle:    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardRegSingle&ucInfo=#
// DlgCardRegDouble:    http://localhost:9000/start?ucName=TestMain&ucParam=DlgCardRegDouble&ucInfo=#
// DlgCardPlayer:       http://localhost:9000/start?ucName=TestMain&ucParam=Player&ucInfo=#


import scala.collection.mutable.{ ArrayBuffer }
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } 
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

  // testDlgCardCfgSection
  def testDlgCardCfgSection(testCase: String, testOption: String) = {
    val coId = testOption.toLong
    val toId = 161L 
    App.loadRemoteTourney(toId) map {
      case Left(err)     => error("testDlgCardCfgSection", s"Can't load tourney")
      case Right(result) => {
      
        // case class PantSelect(val sno: SNO, val name: String, val info: String, var checked: Boolean, val winner: Boolean=true)
        // getInfo returns tuple (SNO.value, Name, Club, TTR)

        val pants = (Trny.pl2co.filterKeys(_._2 == coId).map { x =>
          val sno = SNO(x._2.sno) 
          val (snoValue, name, club, ttr) = sno.getInfo(Trny.comps(coId).typ)(Trny)
          PantSelect(sno, s"${name} [${club}]", s"TTR: ${ttr}", x._2.status == PLS_REDY) 
        }).to(ArrayBuffer).sortBy(x => (!x.checked, x.name))
        
        // val pants = ArrayBuffer[PantSelect]()
        // for (i <-1 to size) { 
        //   val sno = SNO(i.toString)
        //   val info = sno.getInfo(1)(Trny)
        //   debug("DlgCardCfgSection", s"${sno}  ${info}") 
        //   pants += PantSelect(sno, info._2, info._4.toString, true)            
        // }

        DlgCardCfgSection.show(1L, 0, pants)(Trny) map {
          case Left(err)  => println(s"testCase: ${testCase} error -> ${err}") 
          case Right(res) => println(s"testCase: ${testCase} result: ${res}") 
        }
      } 
    }
  }  

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