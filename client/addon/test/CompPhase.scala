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


object AddonCompPhase extends UseCase("AddonCompPhase") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, param: String)= {
    number match {
      case 0 => test_0(param)
      case 1 => test_1(param)
      case 2 => test_2(param)
      case 3 => test_3(param)
    }
  }

  def test_0(text: String) = {
    import cats.data.EitherT
    import cats.implicits._ 

    val toId = text.toLongOption.getOrElse(182L)
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        App.setCurCoId(1)
        App.execUseCase("OrganizeCompetitionDraw", "", "")
        println(s"SUCCESS: test_0")
      }
    }
  }


  def test_1(text: String) = {
    import cats.data.EitherT
    import cats.implicits._ 

    val toId = text.toLongOption.getOrElse(182L)
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        App.setCurCoId(2)
        
        App.execUseCase("OrganizeCompetitionDraw", "", "")
        println(s"SUCCESS: test_0")
      }
    }
  }

  def test_2(text: String) = {
    import scala.collection.mutable.ArrayBuffer
    import scalajs.usecase.organize.OrganizeCompetition
    import scalajs.usecase.dialog.DlgCardCfgCompPhase
    import scalajs.usecase.dialog.DlgCardCfgCompPhase.PantSelect
    import scalajs.usecase.dialog.DlgCardCfgCompPhase.QualifyTyp

    import cats.data.EitherT
    import cats.implicits._ 

    val toId = text.toLongOption.getOrElse(182L)
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        App.setCurCoId(2)
        val coPhIdPrev = 0 
        var coId = 2

        // initialize participants to be shown 
        // only participants with status signed or ready
        val pants = (App.tourney.pl2co.filterKeys(_._2 == coId).filter { case (x1,x2) => x2.status == Pant.SIGN || x2.status == Pant.REDY } map { x =>
          val sno = SNO(x._2.sno) 
          val (snoValue, name, club, ttr) = sno.getInfo(App.tourney.comps(coId).typ)(App.tourney)
          val enabled = (x._2.status == Pant.REDY)
          // show name, club name and ttr value
          PantSelect(sno, s"${name} [${club}]", s"TTR: ${ttr}", enabled, if(enabled) QualifyTyp.Winner else QualifyTyp.Looser ) 
        }).to(ArrayBuffer).sortBy(x => (!x.checked, x.name))
        
        OrganizeCompetition.startCompPhaseDlg(coId, coPhIdPrev, QualifyTyp.All, pants)(App.tourney).map {
          case Left(err)   => error("startCompPhaseDlg", s"error message: ${err}")
          case Right((coph, pantResult)) => {
            //set pant status in participant 2 competition mapping
            println(s"${coph.toString}")
            pantResult.foreach { x => println(s"${x.name} [${x.club}] ${x.rating}") }
          }
        }
      }
    }
  }

  
  // test_3 start following ko-round
  def test_3(text: String) = {
    import scala.collection.mutable.ArrayBuffer
    import scalajs.usecase.organize.OrganizeCompetition
    import scalajs.usecase.dialog.DlgCardCfgCompPhase
    import scalajs.usecase.dialog.DlgCardCfgCompPhase.PantSelect
    import scalajs.usecase.dialog.DlgCardCfgCompPhase.QualifyTyp

    import cats.data.EitherT
    import cats.implicits._ 

    val toId = text.toLongOption.getOrElse(182L)
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        App.setCurCoId(2)
        val coId   = 2
        val coPhId = 1

        // initialize participants to be shown 
        // only participants with status signed or ready
        val coph = App.tourney.cophs((coId, coPhId))

        // generate tuple (PantSelect, Clubname, Group.grId, Position)
        val pantsInfo = (for(i <- 0 to coph.groups.size-1; j <- 0 to coph.groups(i).size-1) yield {
          val pEntry = coph.groups(i).pants(j)
          val pos  = pEntry.place._1
          val size = coph.groups(i).size
          val enabled = pos <= (size/2 + size%2)
          val sets   = coph.groups(i).sets(j)
          val points = coph.groups(i).points(j)
          val rank:Int = (50 + sets._1 - sets._2)*1000 + (50 + points._1 - points._2)
          println(s"${pEntry.name} sets: ${sets._1}/${sets._2}   points: ${points._1}/${points._2} ")
          (PantSelect(SNO(pEntry.sno), s"${pEntry.name} [${pEntry.club}]", s"Group: ${coph.groups(i).name} Position: ${pos}", enabled, if(enabled) QualifyTyp.Winner else QualifyTyp.Looser), coph.groups(i).name, coph.groups(i).grId, pos, rank)
        }).to(ArrayBuffer).sortBy(x => (x._3, x._4))
        val pantMap = pantsInfo.map(x => (x._1.sno -> (x._2, x._3, x._4, x._5))).toMap

        OrganizeCompetition.startCompPhaseDlg(coId, coPhId, QualifyTyp.All, pantsInfo.map(x => x._1))(App.tourney).map {
          case Left(err)   => error("startCompPhaseDlg", s"error message: ${err}")
          case Right((coph, pantResult)) => {
            //set pant status in participant 2 competition mapping
            pantResult.foreach { x => 
              println(s"SNO: ${x.sno}  [${x.club}] ${x.rating}  ${pantMap(SNO(x.sno))._1}  grId: ${pantMap(SNO(x.sno))._2} pos: ${pantMap(SNO(x.sno))._3}    ") 
            }
            val pantsWithGroupInfo = pantResult.map(x => (x, (pantMap(SNO(x.sno))._1, pantMap(SNO(x.sno))._2, pantMap(SNO(x.sno))._3, pantMap(SNO(x.sno))._4))).sortBy(x=>(x._2._3,-x._2._4))
            val (pants, drawInfo) = pantsWithGroupInfo.unzip
            coph.drawWithGroupInfo(pants, drawInfo, App.tourney.comps(coId).typ)

            App.setCurCoPhId(coId, coph.coPhId)
            App.execUseCase("OrganizeCompetitionDraw", "", "")


            println(s"${coph.toString}")
          }
        }
      }
    }
  }



}