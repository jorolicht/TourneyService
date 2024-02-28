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
      case 0 => test_0(param)
      case 1 => test_1(param); Future(true)
      case 2 => test_2(toId, coId, phase, param)
      case 3 => test_3(toId, coId, phase, param)
      case 4 => test_4(param)
      case 5 => test_5(toId, coId, phase, param)
    }
  }



    

  // Test 0 - competition phase default test/debug function 
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test;scope=coph;param=swiss
  def test_0(param: String):Future[Boolean] = {

    def genEdgeInfo(group: Group, pos: Int): String = {
      val str = new StringBuilder("")
      for (i <- pos+1 until group.size) {
        if (group.results(pos)(i).valid) {
          str.append(s"(${pos+1},${i+1},X) ")
        } else {
          val pointDiff = (group.points(pos)._1 - group.points(i)._1).abs
          str.append(s"(${pos+1},${i+1},${pointDiff}) ")
        }
      }
      str.toString()
    }

    val toId = App.tourney.id
    val coId = App.tourney.curCoId
    val coPhId = App.tourney.getCurCoPhId

    val test = s"START: Test Competition Phase toId:${toId} coId:${coId} coPhId:${coPhId} param:${param}"
    AddonMain.setOutput(test)

    param.toLowerCase() match {
      case "swiss" => App.tourney.getCoPh(coId, coPhId) match {
        case Left(err)   => AddonMain.addOutput(s"ERROR: getCoPh failed ${err.toString()}")
        case Right(coph) => {
          val g = coph.groups(0)
          for(i <- 0 until g.size) {
            AddonMain.addOutput(s"(${i+1}) ${g.pants(i).name.substring(0,2)} -> ${g.points(i)._1} ${genEdgeInfo(g,i)}")
          }
        }
      }

      case "mwpm" => App.tourney.getCoPh(coId, coPhId) match {
        case Left(err)   => AddonMain.addOutput(s"ERROR: getCoPh failed ${err.toString()}")
        case Right(coph) => {
          val g = coph.groups(0)
          for(i <- 0 until g.size) {
            for(j <- i+1 until g.size) {
              if (!g.results(i)(j).valid) {
                val pDiff = (2*g.size) - (g.points(i)._1 - g.points(i)._2 - (g.points(j)._1 - g.points(j)._2)).abs
                AddonMain.addOutput(s"${i+1} ${j+1} ${pDiff}")
              }  
            }
          }
        }
      }



      case _ => AddonMain.addOutput(s"HINT: valid params 'swiss' 'mwpm' ")
    }

    AddonMain.addOutput(s"SUCCESS: Test Competition Phase")
    Future(true)
  }


  // def test_0(text: String) = {
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
  //       App.tourney.setCurCoId(1)
  //       App.execUseCase("OrganizeCompetitionDraw", "", "")
  //       println(s"SUCCESS: test_0")
  //     }
  //   }
  // }


  def test_1(text: String) = {
    import cats.data.EitherT
    import cats.implicits._ 

    val toId = text.toLongOption.getOrElse(182L)
    AddonMain.setLoginLoad(toId).map {
      case Left(err)  => AddonMain.addOutput(s"ERROR setLoginLoad - Test Swiss Tournament"); false
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

  // Test 4 - File Input Dialog Test
  // %2D => -
  // %20 => <space>
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20coph%20%2Dn%204%20%2D%2Dparam%20gen 
  def test_4(param: String):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    val test = s"START: Test Competition Phase 4 param:${param}"
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
        AddonMain.setOutput(s"SUCCESS: Test Competition Phase 4")
        true
      }
    }
  }


  // Test 5 - Publish final resultsFile Input Dialog Test 
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test;scope=coph;number=5;toId=186;coId=1;phase=2;param=xx 
  def test_5(toId: Long, coId: Long, phase: Int, param: String):Future[Boolean] = {
    import cats.data.EitherT
    import cats.implicits._ 

    val test = s"START: Test Competition Phase 5 toId:${toId} coId:${coId} phase:${phase} param:${param}"
    AddonMain.setOutput(test)
    (for {
      valid   <- EitherT(authBasicContext("maria30.lichtenegger@gmail.com", "", "MLich@4530"))
      result  <- EitherT(App.loadRemoteTourney(toId))
    } yield { (valid, result) }).value.map {
      case Left(err)    => AddonMain.setOutput(s"ERROR: load tourney toId:${toId} failed with: ${err.msgCode}"); true
      case Right(res)   => {
        App.tourney.setCurCoId(coId)
        val plm = App.tourney.cophs((coId, phase)).getPlacements()
        AddonMain.addOutput(s"Placements: ${plm.mkString(" : ")}")

        for ((key, pantElem) <- App.tourney.pl2co) if (key._2 == coId) { 
          if (plm.isDefinedAt(key._1)) pantElem.setPlace(plm(key._1)) else pantElem.setPlace((0,0)) 
          AddonMain.addOutput(s"Check pant:${key._1} placement:${pantElem.placement}")
        }
        App.tourney.pl2co foreach {case (key, value) => AddonMain.addOutput(s"${value.sno} -> ${value.placement}") }

        App.execUseCase("OrganizeCompetition", "", "")
        AddonMain.addOutput(s"SUCCESS: Test Competition Phase 5")
        true
      }
    }
  }


}