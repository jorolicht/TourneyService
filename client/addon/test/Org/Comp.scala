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
import scala.collection.mutable.ArrayBuffer

// tourney service imports
import shared.utils._
import shared.utils.Routines._

import shared.model._
import shared.model.CompPhase._
import shared.utils.Constants._
import shared.model.tabletennis._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object AddonOrgComp extends TestUseCase("AddonOrgComp") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{

  def testDraw(tnp: TNP) = {
    import cats.data.EitherT
    import cats.implicits._ 

    val toId = tnp.param.toLongOption.getOrElse(182L)
    START(tnp)
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => BasicHtml.setResult(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        AppEnv.setCoId(1)
        App.execUseCase("OrganizeCompetitionDraw", "", "")

        //val testTrny = Tourney.decode(encResult)
        //info("testEncode", s"Tourney decoded: ${testTrny.toString()}")
        //println(s"FAILED AddonOrgComp.testDraw")
        SUCCESS(tnp)

      }
    }
  }

  def testDrawKo(tnp: TNP) = {
    import cats.data.EitherT
    import cats.implicits._ 

    val toId = tnp.param.toLongOption.getOrElse(182L)
    START(tnp)
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => BasicHtml.setResult(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        AppEnv.setCoId(1)
        App.execUseCase("OrganizeCompetitionDraw", "", "")
        SUCCESS(tnp)

      }
    }
  }

  def testInputKo(tnp: TNP) = {
    import cats.data.EitherT
    import cats.implicits._ 

    val toId = tnp.param.toLongOption.getOrElse(182L)
    START(tnp)
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => BasicHtml.setResult(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        AppEnv.setCoId(1)
        AppEnv.coPhIdMap(1) = 3
        App.execUseCase("OrganizeCompetitionInput", "", "")
        SUCCESS(tnp)
      }
    }
  }



  def testMatchEncode(tnp: TNP) = {
    START(tnp)
    
    val rnd1M = ArrayBuffer(
      //         stNoA,  stNoB,   coId, coPh,  coPhId,  gameNo, round, maNo, grId, wgw,   playfield, info, startTime, endTime, status, sets,result 
      MatchEntry("", "", 0L, 0, 0, 0, 0, 0, 0, (0,0), "", "", "", "", 0, (0,0), ""),
      MatchEntry("00131","00132", 13L,  CST_SW, 1,      1,      1,     1,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "3·2·3·4"),
      MatchEntry("00133","00134", 13L,  CST_SW, 1,      2,      1,     2,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "4·2·-2·3·4"),
      MatchEntry("00135","00136", 13L,  CST_SW, 1,      3,      1,     3,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "3·9·3·4"),
      MatchEntry("00137","00138", 13L,  CST_SW, 1,      4,      1,     4,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "5·2·-8·-12·3·4"),
      MatchEntry("00132","00133", 13L,  CST_SW, 1,      5,      2,     1,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "3·1·5·4"),
      MatchEntry("00134","00135", 13L,  CST_SW, 1,      6,      2,     2,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "4·2·-7·3·4"),
      MatchEntry("00136","00137", 13L,  CST_SW, 1,      7,      2,     3,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "3·7·3·4"),
      MatchEntry("00131","00138", 13L,  CST_SW, 1,      8,      2,     4,    0,    (0,0), "",        "",   "",        "",      0,   (0,0),   "5·2·-4·-11·3·8")
    )

    val m = MatchTest(rnd1M) 
    val mEnc = write[MatchTest](m)
    println(s"Encode Match: ${mEnc}")

    val testMatch = MatchEntry.objectify("00025^99505^1^8^3^1^4^1^0^0^0^1·8·1^^^20220207170505^2^3^0^^_")
    println(s"Test MatchEntry.objectify ${testMatch}")
    SUCCESS(tnp)
  }





}



case class MatchTest(
  val matches:   ArrayBuffer[MatchEntry]
)

object MatchTest {
  implicit def rw: RW[MatchTest] = macroRW 
}
