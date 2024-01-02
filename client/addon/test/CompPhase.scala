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


object AddonCompPhase extends UseCase("AddonCompPhase") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, toId: Long=0L, coId: Long=0L, phase: Int=0,  param: String=""): Future[Boolean]= {
    number match {
      case 0 => test_0(param); Future(true)
      case 1 => test_1(param); Future(true)
      case 2 => test_2(toId, coId, phase, param)
      case 3 => test_3(toId, coId, phase, param)
      case 4 => test_4(param)
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
        App.tourney.setCurCoId(1)
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
        App.tourney.setCurCoId(2)
        
        App.execUseCase("OrganizeCompetitionDraw", "", "")
        println(s"SUCCESS: test_0")
      }
    }
  }


  // Test 2 - Swiss Tournamnt
  // %2D => -
  // %20 => <space>
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20coph%20%2Dn%202%20%2D%2DtoId%20186%20%2D%2DcoId%203%20%2D%2Dphase%203%20%2D%2Dparam%20gen 
  def test_2(toId: Long, coId: Long, phase: Int, param: String): Future[Boolean]  = {
    AddonMain.setOutput(s"START Test Swiss Tournament: toId->${toId} coId->${coId} phase->${phase} param->${param}")
    AddonMain.setLoginLoad(toId).map {
      case Left(err)  => AddonMain.addOutput(s"ERROR setLoginLoad - Test Swiss Tournament"); false
      case Right(res) => {
        val pantList = scala.collection.mutable.ListBuffer[Player]()
        val content = """
          Name, Vorname, Club, TTR, Jahrgang, Geschlecht, EMail
          A, , TTC Freising, 1610, 1963, 2, ro.li@icloud.com
          B, , TTC Kranzberg, 1200, 1962, 2
          C, , TTC Finkenstr, 1111, 1969, 1
          D, , DSK Fernsehen, 1234, 1960, 2
          E, , FSK Grünanlage, 1236, 1945, 2
          F, , TTC Egal, 1423, 1935, 2
          G, , FC Zigaretten, 1534, 1996, 2
          H, , TTC Tennisschläger, 1611, 1998, 2
          I, , ASV Terroreinheit, 1534, 1966, 2
          J, , LK Kühlschrank, 1333, 1978, 2
          K, , HJ Nichtnormal, 1442, 1999, 1
          L, , TJK Südafrika, 1327, 1958, 1     
        """

        val lines = content.split("\n")
        lines.zipWithIndex.foreach { case (line, index) => Player.fromCSV(line) match { 
          case Left(err)     => if (!err.is("return001.csv.hdr.player")) println("ERROR reading lines")
          case Right(player) => if (!List("name","lastname").contains(player.lastname.toLowerCase()) ) pantList += player
        }}
        
        regSingle(coId, pantList.toList, PantStatus.REDY).map {
          case Left(err)   => println("ERROR register single")
          case Right(res)  => {
            println("SUCCESS loading single")

          }  
        }
        true
      }
    }
  }  


  // Test 3 - Test Swiss Tournament Draw
  // %2D => -
  // %20 => <space>
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20coph%20%2Dn%203%20%2D%2DtoId%20186%20%2D%2DcoId%203%20%2D%2Dphase%203%20%2D%2Dparam%20gen 
  def test_3(toId: Long, coId: Long, coPhId: Int, param: String): Future[Boolean]  = {
    AddonMain.setOutput(s"START Test Swiss Tournament: toId->${toId} coId->${coId} coPhId->${coPhId} param->${param}")
    AddonMain.setLoginLoad(toId).map {
      case Left(err)  => AddonMain.addOutput(s"ERROR setLoginLoad - Test 3 Swiss Tournament Draw"); false
      case Right(res) => {
        if (!App.tourney.cophs.contains((coId, coPhId))) error("App.tourney.cophs.contains", s"Test 3 Swiss Tournament Draw can't find competition phase")
        val coph = App.tourney.cophs((coId, coPhId)) 
        coph.draw(App.tourney.comps(coph.coId).typ, App.tourney.getCoPhList(coId, coPhId)) match {
          case Left(err)  => error("coph.draw", s"${err}"); false
          case Right(res) => {
            App.execUseCase("OrganizeCompetitionDraw", "", "")
            true
          }
        }
      }
    }
  } 



  //  def showRrResult(coId: Long, coPhId: Int, group: Group) = {
  //   if (group.size <= 12) showGrResult(coId, coPhId, group) else {
  //     (for(i<-0 until group.size) yield {
  //       (group.pants(i).place._1, group.pants(i).name, group.pants(i).club, 
  //        group.balls(i)._1.toString + ":" + group.balls(i)._2.toString, 
  //        group.sets(i)._1.toString + ":" + group.sets(i)._2.toString,
  //        group.points(i)._1.toString + ":" + group.points(i)._2.toString)
  //     }).sortBy(_._1).zipWithIndex.map { e => {


  // def test_2(text: String) = {
  //   import scala.collection.mutable.ArrayBuffer
  //   import scalajs.usecase.organize.OrganizeCompetition
  //   import scalajs.usecase.dialog.DlgCardCfgCompPhase
  //   import scalajs.usecase.dialog.DlgCardCfgCompPhase.PantSelect
  //   import scalajs.usecase.dialog.DlgCardCfgCompPhase.QualifyTyp

  //   import cats.data.EitherT
  //   import cats.implicits._ 

  //   val toId = text.toLongOption.getOrElse(182L)
  //   (for {
  //     pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
  //     coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
  //     result    <- EitherT(App.loadRemoteTourney(toId))
  //   } yield { (result, pw) }).value.map {
  //     case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
  //     case Right(res)   => {
  //       App.tourney.setCurCoId(2)
  //       val coPhIdPrev = 0 
  //       var coId = 2

  //       // initialize participants to be shown 
  //       // only participants with status signed or ready
  //       val pants = (App.tourney.pl2co.filterKeys(_._2 == coId).filter { case (x1,x2) => x2.status == PantStatus.PEND.id || x2.status == PantStatus.REDY.id } map { x =>
  //         val sno = SNO(x._2.sno) 
  //         val (snoValue, name, club, ttr) = sno.getInfo(App.tourney.comps(coId).typ)(App.tourney)
  //         val enabled = (x._2.status == PantStatus.REDY.id)
  //         // show name, club name and ttr value
  //         PantSelect(sno, s"${name} [${club}]", s"TTR: ${ttr}", enabled, if(enabled) QualifyTyp.Winner else QualifyTyp.Looser ) 
  //       }).to(ArrayBuffer).sortBy(x => (!x.checked, x.name))
        
  //       // OrganizeCompetition.startCompPhaseDlg(coId, coPhIdPrev, QualifyTyp.All, pants)(App.tourney).map {
  //       //   case Left(err)   => error("startCompPhaseDlg", s"error message: ${err}")
  //       //   case Right((coph, pantResult)) => {
  //       //     //set pant status in participant 2 competition mapping
  //       //     println(s"${coph.toString}")
  //       //     pantResult.foreach { x => println(s"${x.name} [${x.club}] ${x.rating}") }
  //       //   }
  //       // }
  //     }
  //   }
  // }

  
  // // test_3 start following ko-round
  // def test_3(text: String) = {
  //   import scala.collection.mutable.ArrayBuffer
  //   import scalajs.usecase.organize.OrganizeCompetition
  //   import scalajs.usecase.dialog.DlgCardCfgCompPhase
  //   import scalajs.usecase.dialog.DlgCardCfgCompPhase.PantSelect
  //   import scalajs.usecase.dialog.DlgCardCfgCompPhase.QualifyTyp

  //   import cats.data.EitherT
  //   import cats.implicits._ 

  //   val toId = text.toLongOption.getOrElse(182L)
  //   (for {
  //     pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
  //     coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
  //     result    <- EitherT(App.loadRemoteTourney(toId))
  //   } yield { (result, pw) }).value.map {
  //     case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
  //     case Right(res)   => {
  //       App.tourney.setCurCoId(2)
  //       val coId   = 2
  //       val coPhId = 1

  //       // initialize participants to be shown 
  //       // only participants with status signed or ready
  //       val coph = App.tourney.cophs((coId, coPhId))

  //       // generate tuple (PantSelect, Clubname, Group.grId, Position)
  //       val pantsInfo = (for(i <- 0 to coph.groups.size-1; j <- 0 to coph.groups(i).size-1) yield {
  //         val pEntry = coph.groups(i).pants(j)
  //         val pos  = pEntry.place._1
  //         val size = coph.groups(i).size
  //         val enabled = pos <= (size/2 + size%2)
  //         val sets   = coph.groups(i).sets(j)
  //         val points = coph.groups(i).points(j)
  //         val rank:Int = (50 + sets._1 - sets._2)*1000 + (50 + points._1 - points._2)
  //         println(s"${pEntry.name} sets: ${sets._1}/${sets._2}   points: ${points._1}/${points._2} ")
  //  1       (PantSelect(SNO(pEntry.sno), 
  //  2          s"${pEntry.name} [${pEntry.club}]", 
  //  3          s"Group: ${coph.groups(i).name} Position: ${pos}", 
  //  4          enabled, 
  //  5          if(enabled) QualifyTyp.Winner else QualifyTyp.Looser), 
  //  6          coph.groups(i).name, 
  //  7          coph.groups(i).grId, 
  //  8          pos, 
  //            rank)
  //       }).to(ArrayBuffer).sortBy(x => (x._3, x._4))
  //       val pantMap = pantsInfo.map(x => (x._1.sno -> (x._2, x._3, x._4, x._5))).toMap

  //      coph.groups(i).name, coph.groups(i).grId, pos, rank

  //       // OrganizeCompetition.startCompPhaseDlg(coId, coPhId, QualifyTyp.All, pantsInfo.map(x => x._1))(App.tourney).map {
  //       //   case Left(err)   => error("startCompPhaseDlg", s"error message: ${err}")
  //       //   case Right((coph, pantResult)) => {
  //       //     //set pant status in participant 2 competition mapping
  //       //     pantResult.foreach { x => 
  //       //       println(s"SNO: ${x.sno}  [${x.club}] ${x.rating}  ${pantMap(SNO(x.sno))._1}  grId: ${pantMap(SNO(x.sno))._2} pos: ${pantMap(SNO(x.sno))._3}    ") 
  //       //     }
  //       //     val pantsWithGroupInfo = pantResult.map(x => (x, (pantMap(SNO(x.sno))._1, pantMap(SNO(x.sno))._2, pantMap(SNO(x.sno))._3, pantMap(SNO(x.sno))._4))).sortBy(x=>(x._2._3,-x._2._4))
  //       //     val (pants, drawInfo) = pantsWithGroupInfo.unzip
  //       //     coph.drawWithGroupInfo(pants, drawInfo, App.tourney.comps(coId).typ)

  //       //     App.tourney.comps(coId).setCurCoPhId(coph.coPhId)
  //       //     App.execUseCase("OrganizeCompetitionDraw", "", "")


  //       //     println(s"${coph.toString}")
  //       //   }
  //       // }
  //     }
  //   }
  // }

  // Test 4 - File Input Dialog Test
  // %2D => -
  // %20 => <space>
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20coph%20%2Dn%204%20%2D%2Dparam%20gen 


  def test_4(param: String):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    val test = s"START Test Competition Phase 4 text->${param}"
    AddonMain.setOutput(test)
    val toId = 186L
    (for {
      valid   <- EitherT(authBasicContext("maria30.lichtenegger@gmail.com", "", "MLich@4530"))
      result  <- EitherT(App.loadRemoteTourney(toId))
    } yield { (valid, result) }).value.map {
      case Left(err)    => AddonMain.setOutput(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}"); true
      case Right(res)   => {
        App.tourney.setCurCoId(1)
        App.execUseCase("OrganizeCompetition", "", "")
        AddonMain.setOutput(s"SUCCESS: ${test}")
        true
      }
    }
  }



}