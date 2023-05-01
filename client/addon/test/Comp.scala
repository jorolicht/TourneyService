package addon.test

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

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object AddonComp extends UseCase("AddonComp") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def testEncode(testOption: String) = {
    import scala.collection.mutable.{ ArrayBuffer }
    import cats.data.EitherT
    import cats.implicits._ 
   
    val toId = testOption.toLongOption.getOrElse(182L)
    println(s"START AddonComp.testEncode => toId: ${toId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => BasicHtml.setResult(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        
    // case class CompPhase(val name: String, val coId: Long, val coPh: Int, 
    //                      val coPhId: Int, val coPhTyp: Int, 
    //                      val status: Int, var enabled: Boolean, 
    //                      var size: Int, var noPlayers: Int, noWinSets: Int = 3)

        val coPhGr = new CompPhase("TestGroup", 3L, 2, 99, CompPhaseTyp.GR, CompPhaseStatus.FIN, true, 7, 7, 3)
        // val initGrRes  = coPhGr.init( ArrayBuffer(
        //   ParticipantEntry("XXX131", "Lichtenegger, Robert1", "TTC Freising1", 1207, (0,0)),
        //   ParticipantEntry("XXX132", "Lichtenegger, Robert2", "TTC Freising2", 1301, (0,0)),
        //   ParticipantEntry("XXX133", "Lichtenegger, Robert3", "TTC Freising3", 1299, (0,0)),
        //   ParticipantEntry("XXX134", "Lichtenegger, Robert4", "TTC Freising4", 1400, (0,0)),
        //   ParticipantEntry("XXX135", "Lichtenegger, Robert5", "TTC Freising5", 1309, (0,0)),
        //   ParticipantEntry("XXX136", "Lichtenegger, Robert6", "TTC Freising6", 2100, (0,0)),
        //   ParticipantEntry("XXX137", "Lichtenegger, Robert7", "TTC Freising7", 1123, (0,0))),
        //   List( GroupConfig(1, "A", 3, 2, 1), GroupConfig(2, "B", 4, 2, 4))
        //)

        //case class GroupConfig(id: Int, name: String, size: Int, quali: Int, pos: Int)

        val coPhKO = new CompPhase("TestKO", 3L, 4, 99, CompPhaseTyp.KO, CompPhaseStatus.DEP, true, 8, 7, 3)
        // val initKORes  = coPhKO.init( ArrayBuffer(
        //   ParticipantEntry("XXX131", "Lichtenegger, Robert1", "TTC Freising1", 1207, (0,0)),
        //   ParticipantEntry(SNO.BYE, "bye", "", 0, (0, 0)),
        //   ParticipantEntry("XXX132", "Lichtenegger, Robert2", "TTC Freising2", 1301, (0,0)),
        //   ParticipantEntry("XXX133", "Lichtenegger, Robert3", "TTC Freising3", 1299, (0,0)),
        //   ParticipantEntry("XXX134", "Lichtenegger, Robert4", "TTC Freising4", 1400, (0,0)),
        //   ParticipantEntry("XXX135", "Lichtenegger, Robert5", "TTC Freising5", 1309, (0,0)),
        //   ParticipantEntry("XXX136", "Lichtenegger, Robert6", "TTC Freising6", 2100, (0,0)),
        //   ParticipantEntry("XXX137", "Lichtenegger, Robert7", "TTC Freising7", 1123, (0,0))))
  
        App.tourney.cophs((3l,4))  = coPhKO
        App.tourney.cophs((3l,2))  = coPhGr


        val exa =s"""  
        """
        
        // CompPhase.decode(exa) match {
        //   case Left(err)   => println(s"testEncode => ERROR: ${err}")
        //   case Right(res)  => println(s"testEncode => RESULT: ${res}")
        // }

        println(s"testEncode => GroupPhase: ${coPhGr.encode}")
        println(s"testEncode => KOPhase: ${coPhKO.encode}")

        val  encResult = App.tourney.encode()
      
        info("testEncode", s"Tourney encoded: ${encResult}")

        val testTrny = Tourney.decode(encResult)
        //val testTrny = Tourney.decode(encResult)
        info("testEncode", s"Tourney decoded: ${testTrny.toString()}")

        // saveTourney(toId) map {
        //   case Left(err)  => BasicHtml.setResult(s"ERROR: saving tourney ${toId} failed with: ${err.msgCode}")
        //   case Right(res) => BasicHtml.setResult(s"SUCCESS: tourney toId: ${toId} saved")
        // }
      }
    }
  }

}