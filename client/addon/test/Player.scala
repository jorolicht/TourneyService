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

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }


object AddonPlayer extends UseCase("AddonPlayer") 
  with TourneySvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, toId: Long, plId: Long, param: String): Future[Boolean] = {
    number match {
      case 0 => test_0(toId, plId)
      case 1 => test_1(toId, plId)
      case 2 => test_2(toId, plId).flatMap(identity)
      case 3 => test_3(toId, plId).flatMap(identity)
      case 4 => test_4(toId, plId, param)
      case 5 => test_5(toId, plId, param)
    }
  }

  // Test Player 0 show dialog: test -s player -n 0 --toId 185 --plId 41
  def test_0(toId: Long, plId: Long):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    AddonMain.setOutput(s"START Test Player 0: show player page")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => AddonMain.addOutput(s"ERROR Test Player 0: load tourney ${toId} failed with: ${err.msgCode}"); false
      case Right(res)   => {
        App.tourney.setCurCoId(1)
        App.execUseCase("OrganizePlayer", "", "")
        AddonMain.addOutput(s"SUCCESS Test Player 0")
        true
      }
    }
  }


  // Test Player 1 show: test -s player -n 1 --toId 185 --plId 41
  def test_1(toId: Long, plId: Long):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    AddonMain.setOutput(s"START Test Player 1: show player plId=${plId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => AddonMain.addOutput(s"ERROR Test Player 1: load tourney ${toId} failed with: ${err.msgCode}"); false
      case Right(res)   => AddonMain.addOutput(s"SUCCESS Test Player 1: ${App.tourney.players(plId)}"); true
    }
  }


  // {
  //     "id": 41,
  //     "hashKey": "",
  //     "clubId": 30,
  //     "clubName": "TV Quahog",
  //     "firstname": "Ross",
  //     "lastname": "Haar",
  //     "birthyear": 1999,
  //     "email": "",
  //     "sex": 1,
  //     "options": "NU1281105·202004168·202004·ByTTV·1904·82····"
  // }

  // Test Player 2 delete license: test -s player -n 2 --toId 185 --plId 41
  def test_2(toId: Long, plId: Long):Future[Future[Boolean]] = {
    import cats.data.EitherT
    import cats.implicits._ 

    AddonMain.setOutput(s"START Test Player 2: delete license ${plId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => AddonMain.setOutput(s"ERROR Test Player 2: load tourney ${toId} failed with: ${err.msgCode}"); Future(false)
      case Right(res)   => setPlayer(plId, CttLicense("")).map {
        case Left(err)     => AddonMain.setOutput(s"ERROR Test Player 2: ${getError(err)}"); false
        case Right(player) => AddonMain.setOutput(s"SUCCESS Test Player 2: ${player.getLicense}"); true
      }
    }
  }

  // Test Player 3 set license: test -s player -n 3 --toId 185 --plId 41
  def test_3(toId: Long, plId: Long):Future[Future[Boolean]] = {
    import cats.data.EitherT
    import cats.implicits._ 

    AddonMain.setOutput(s"START Test Player 3 set license: plId=${plId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => AddonMain.addOutput(s"ERROR Test Player 3 set license: load failed with: ${err.msgCode}"); Future(false)
      case Right(res)   => setPlayer(plId, CttLicense("202004168")).map {
        case Left(err)     => AddonMain.addOutput(s"ERROR Test Player 3 set license: failed with: ${err.msgCode}"); false
        case Right(player) => AddonMain.addOutput(s"SUCCESS Test Player 3: ${player.getLicense}"); true
      }
    }
  }

  // Test Player 4 set email: test -s player -n 4 --toId 185 --plId 41 --param robert.lichtenegger@icloud.com
  def test_4(toId: Long, plId: Long, email: String):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    AddonMain.setOutput(s"START Test Player 4 set email: ${email}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT( App.loadRemoteTourney(toId) )
      player    <- EitherT( addPlayer(App.tourney.players(plId).copy(email=email))  )
    } yield { (pw, result, player ) }).value.map {
      case Left(err)  => AddonMain.addOutput(s"ERROR Test Player 4 set email failed with: ${getError(err)}"); false
      case Right(res) => AddonMain.addOutput(s"SUCCESS Test Player 4 set email: ${res._3.email}"); true
    }
  }

  // Test Player 5 set name: test -s player -n 5 --toId 185 --plId 41 --param <Name>
  def test_5(toId: Long, plId: Long, name: String):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    AddonMain.setOutput(s"START Test Player 5 set lastname: ${name}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT( App.loadRemoteTourney(toId) )
      player    <- EitherT( setPlayer(App.tourney.players(plId).copy(lastname=name))  )
    } yield { (pw, result, player ) }).value.map {
      case Left(err)  => AddonMain.addOutput(s"ERROR Test Player 5 set lastname failed with: ${getError(err)}"); false
      case Right(res) => AddonMain.addOutput(s"SUCCESS Test Player 5 set lastname: ${res._3.lastname}"); true
    }
  }

  // Test Player 6 set name: test -s player -n 6 --toId 185 --plId 41 --param <Name>
  def test_6(toId: Long, plId: Long):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    AddonMain.setOutput(s"START Test Player 6 dialog: plId->${plId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT( App.loadRemoteTourney(toId) )
      player    <- EitherT( setPlayer(App.tourney.players(plId).copy(lastname=name))  )
    } yield { (pw, result, player ) }).value.map {
      case Left(err)  => AddonMain.addOutput(s"ERROR Test Player 5 set lastname failed with: ${getError(err)}"); false
      case Right(res) => AddonMain.addOutput(s"SUCCESS Test Player 5 set lastname: ${res._3.lastname}"); true
    }
  }



}