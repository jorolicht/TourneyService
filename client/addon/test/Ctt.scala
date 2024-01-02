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


object AddonCtt extends UseCase("AddonCtt") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, param: String)= {
    number match {
      case 0 => test_0(param)
      case 1 => test_1(param)
      case 2 => test_2(param)
      case 3 => test_3(param)
      case 4 => test_4(param)
      case 5 => test_5(param)
      // case 6 => test_6(param)
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

  // test -s ctt -n 1 -p 185
  def test_1(text: String) = {
    import scalajs.usecase.dialog.DlgSpinner
    import js.JSConverters._
    import cats.data.EitherT
    import cats.implicits._ 

    val sDate = text.toIntOption.getOrElse(20230401)
    val toId  = 185L
    
    println(s"---> Start Test: CTT update / toId ${toId} sDate: ${sDate}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => {
        val date = text.toIntOption.getOrElse(19000101)

        val formData = new dom.FormData()  
        val data = new dom.Blob(CttTest.genTourneyXML(sDate).toJSArray.asInstanceOf[scala.scalajs.js.Array[scala.scalajs.js.Any]], 
                                dom.raw.BlobPropertyBag("text/plain"))
        formData.append("file", data, "participant.xml") 

        DlgSpinner.start( "Start Transfer") 
        updCttFile(App.tourney.getToId, App.tourney.startDate, formData).map {
          case Left(err)  => println(s"${err}"); DlgSpinner.error( getError(err)) 
          case Right(res) => DlgSpinner.result( s"TourneyId: ${res}") 
        }
      }
    }
  }


  def test_2(text: String) = {
    import scalajs.usecase.dialog.DlgSpinner
    import js.JSConverters._
    import cats.data.EitherT
    import cats.implicits._ 
    
    val sDate = text.toIntOption.getOrElse(19000101)
    
    println(s"---> Start Test: CTT new / sDate: ${sDate}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
    } yield { (pw) }).value.map {
      case Left(err)  => dom.window.alert(s"ERROR: authentication failed with: ${getError(err)}")
      case Right(res) => {
        val date   = sDate

        val formData = new dom.FormData()  
        val data = new dom.Blob(CttTest.genTourneyXML(sDate).toJSArray.asInstanceOf[scala.scalajs.js.Array[scala.scalajs.js.Any]], 
                                dom.raw.BlobPropertyBag("text/plain"))
        formData.append("file", data, "participant.xml") 

        DlgSpinner.start("Start Transfer") 
        newCttFile(0L, sDate, formData).map {
          case Left(err)   => println(s"${err}"); DlgSpinner.error( getError(err)) 
          case Right(res) => {
            DlgSpinner.result(s"TourneyId: ${res._1} Name: ${res._2}") 
            App.loadRemoteTourney(res._1).map {
              case Left(err)  => dom.window.alert(s"ERROR: load tourney ${res._1} failed with: ${getError(err)}")
              case Right(res) => {         
               App.tourney.setCurCoId(1)
               App.execUseCase("OrganizeCompetition", "", "")
              }
            }
          }  
        }
      }
    }
  }


  def test_3(text: String) = {
    import scalajs.usecase.dialog.DlgSpinner
    import js.JSConverters._
    import cats.data.EitherT
    import cats.implicits._ 
    
    val toId = text.toLongOption.getOrElse(185L)

    
    println(s"---> Start Test: generate CTT result file for toId: ${toId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (pw) }).value.map {
      case Left(err)  => dom.window.alert(s"ERROR: authentication failed with: ${getError(err)}")
      case Right(res) => {
        genCttResult.map {
          case Left(err)     => println(s"ERROR: genCttResult with: ${getError(err)}")
          case Right(resArr) => {
            resArr.foreach { entry => entry._2 match {
              case Left(err)      => println(s"CoId: ${entry._1} Error: ${err}")
              case Right(snoLists) => 
                println("----------------------------------------------------------")
                println(s"CoId: ${entry._1}   okList: ${snoLists._1}")  
                println(s"CoId: ${entry._1} missList: ${snoLists._2}") 
            }}

          }  
        }
      }
    }
  }


  def test_4(text: String) = {
    import scalajs.usecase.dialog.DlgSpinner
    import js.JSConverters._
    import cats.data.EitherT
    import cats.implicits._ 
    
    val sDate = text.toIntOption.getOrElse(19000101)
    
    println(s"---> Start Test: generate CTT result file")
    genCttResult.map {
      case Left(err)     => println(s"ERROR: genCttResult with: ${getError(err)}")
      case Right(result) => println(s"RESULT: ${result}")
    }
  }


  def test_5(text: String) = {
    import cats.data.EitherT
    import cats.implicits._ 

    val toId = text.toLongOption.getOrElse(185L)
    println(s"---> Start Test: generate CTT result file for: ${toId}")
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (result, pw) }).value.map {
      case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
      case Right(res)   => genCttResult.map {
        case Left(err)     => println(s"ERROR: genCttResult with: ${getError(err)}")
        case Right(result) => {
          result.foreach(elem => { println(s"coId: ${elem._1}");  println(s"mapping: ${elem._2}") })
        }  
      }
    }
  }

  // def test_6(text: String) = {
  //   import cats.data.EitherT
  //   import cats.implicits._ 

  //   val toId = text.toLongOption.getOrElse(185L)
  //   println(s"---> Start Test: update CTT mapping: ${toId}")

  //   (for {
  //     pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
  //     coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
  //     result    <- EitherT(App.loadRemoteTourney(toId))
  //   } yield { (result, pw) }).value.map {
  //     case Left(err)    => dom.window.alert(s"ERROR: load tourney ${toId} failed with: ${err.msgCode}")
  //     case Right(res)   => {
        


  //     }
  //   }
  // }  




}

object CttTest {

  def genTourneyXML(date: Int): String = {
    val day   = "%02d".format(date%100)
    val month = "%02d".format((date/100)%100)
    val year  = date/10000
    val dateStr = s"${year}-${month}-${day}"

    val rand = new scala.util.Random
    val x = 100 + rand.nextInt(50)

    s"""<?xml version="1.0" encoding="utf-8"?>
     |<!DOCTYPE tournament SYSTEM "http://www.datenautomaten.nu/dtd/nuLiga/TournamentPortal.dtd">
     |<tournament start-date="${dateStr}" end-date="${dateStr}"  name="${x}. Internationale Freisinger Meisterschaften Freisinger" tournament-id="i36eIvYBUqxYW1OUCKu2pT5v50i4mxM0">
     |  
     |    <competition start-date="${dateStr} 12:00" ttr-from="1650" ttr-to="3000" ttr-remarks="S-Klasse" age-group="Herren" type="Einzel">
     |      <players>
     |        
     |          <player type="single" id="PLAYER1">
     |            <person licence-nr="111009155" club-federation-nickname="ByTTV" club-name="TV Traktor Leipzig" sex="1" ttr-match-count="351" lastname="Kahn" ttr="1792" internal-nr="NU1000999" club-nr="111009" firstname="Ada" birthyear="1965"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER2">
     |            <person licence-nr="105018106" club-federation-nickname="ByTTV" club-name="TTC Torpedo Moskau" sex="1" ttr-match-count="118" lastname="Tion" ttr="1846" internal-nr="NU210649" club-nr="105018" firstname="Addi" birthyear="1947"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER3">
     |            <person licence-nr="113015005" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" sex="1" ttr-match-count="250" lastname="Lette" ttr="1922" internal-nr="NU999395" club-nr="113015" firstname="Adi" birthyear="1960"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER4">
     |            <person licence-nr="112020111" club-federation-nickname="ByTTV" club-name="TSV Schatzinsel" sex="1" ttr-match-count="315" lastname="ter Native" ttr="1680" internal-nr="NU1004283" club-nr="112020" firstname="Al" birthyear="1957"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER5">
     |            <person licence-nr="110011337" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" sex="1" ttr-match-count="77" lastname="Herum" ttr="2190" internal-nr="NU1009099" club-nr="110011" firstname="Albert" birthyear="1981"/>				
     |          </player>
     |        
     |          <player type="single" id="PLAYER6">
     |            <person licence-nr="103013138" club-federation-nickname="ByTTV" club-name="SpVgg Röhrmoos-Großinzemoos" sex="1" ttr-match-count="123" lastname="Platz" ttr="1761" internal-nr="NU1314710" club-nr="103013" firstname="Alexander" birthyear="1962"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER7">
     |            <person licence-nr="112002146" club-federation-nickname="ByTTV" club-name="FC Kiffen 08" sex="1" ttr-match-count="392" lastname="Bert" ttr="1836" internal-nr="NU1006294" club-nr="112002" firstname="Ali" birthyear="1965"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER8">
     |            <person licence-nr="112024034" club-federation-nickname="ByTTV" club-name="VfR Netzroller" sex="1" ttr-match-count="251" lastname="Mente" ttr="1927" internal-nr="NU1006207" club-nr="112024" firstname="Ali" birthyear="1972"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER9">
     |            <person licence-nr="110007242" club-federation-nickname="ByTTV" club-name="SV-DJK Kantenballer" sex="1" ttr-match-count="112" lastname="Mater" ttr="1669" internal-nr="NU1005826" club-nr="110007" firstname="Alma" birthyear="1970"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER10">
     |            <person licence-nr="106009129" club-federation-nickname="ByTTV" club-name="SC Donnerfels" sex="1" ttr-match-count="153" lastname="Gehzauch" ttr="1313" internal-nr="NU1011848" club-nr="106009" firstname="Anders" birthyear="1950"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER11">
     |            <person licence-nr="112005028" club-federation-nickname="ByTTV" club-name="TTC Scheibenwelt" sex="1" ttr-match-count="295" lastname="Völcker-Dieserweldt" ttr="1865" internal-nr="NU1011157" club-nr="112005" firstname="Andi" birthyear="1962"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER12">
     |            <person licence-nr="106012060" club-federation-nickname="ByTTV" club-name="SV Nokturmgasse 7" sex="1" ttr-match-count="118" lastname="Schweiß" ttr="1733" internal-nr="NU1011856" club-nr="106012" firstname="Axel" birthyear="1980"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER13">
     |            <person licence-nr="112010153" club-federation-nickname="ByTTV" club-name="SV Springfield" sex="1" ttr-match-count="133" lastname="Achse" ttr="1996" internal-nr="NU1010868" club-nr="112010" firstname="Kurt" birthyear="1961"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER14">
     |            <person licence-nr="112027001" club-federation-nickname="ByTTV" club-name="SV Monkey Island e.V." sex="1" ttr-match-count="67" lastname="Container" ttr="2201" internal-nr="NU1260286" club-nr="112027" firstname="Klaas" birthyear="1970"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER15">
     |            <person licence-nr="110011483" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" sex="1" ttr-match-count="12" lastname="Trophobie" ttr="2020" internal-nr="NU1376904" club-nr="110011" firstname="Klaus" birthyear="1978"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER16">
     |            <person licence-nr="113015094" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" sex="1" ttr-match-count="37" lastname="Schenn" ttr="1892" internal-nr="NU1228839" club-nr="113015" firstname="Knuth" birthyear="1965"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER17">
     |            <person licence-nr="113009215" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" sex="1" ttr-match-count="151" lastname="Brandenburg" ttr="1877" internal-nr="NU1014471" club-nr="113009" firstname="Mark" birthyear="1995"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER18">
     |            <person licence-nr="402010400" club-federation-nickname="ByTTV" club-name="DJK SB Pandora 64" sex="1" ttr-match-count="52" lastname="Zorn" ttr="1876" internal-nr="NU402453" club-nr="402010" firstname="Martin" birthyear="1988"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER19">
     |            <person licence-nr="112011158" club-federation-nickname="ByTTV" club-name="SG Hogsmeade" sex="1" ttr-match-count="450" lastname="Hose" ttr="1729" internal-nr="NU1019633" club-nr="112011" firstname="Lutz" birthyear="1961"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER20">
     |            <person licence-nr="110006163" club-federation-nickname="ByTTV" club-name="SV Hundert-Morgen-Wald" sex="1" ttr-match-count="91" lastname="Ehfer" ttr="1727" internal-nr="NU1268250" club-nr="110006" firstname="Maik" birthyear="1969"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER21">
     |            <person licence-nr="205015165" club-federation-nickname="ByTTV" club-name="SV Ihrlerstein 1958" sex="1" ttr-match-count="271" lastname="Hauden" ttr="1731" internal-nr="NU1017849" club-nr="205015" firstname="Lukas" birthyear="1995"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER22">
     |            <person licence-nr="112005044" club-federation-nickname="ByTTV" club-name="TTC Scheibenwelt" sex="1" ttr-match-count="368" lastname="Schaltung" ttr="1863" internal-nr="NU1017876" club-nr="112005" firstname="Leif" birthyear="1946"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER23">
     |            <person licence-nr="110020317" club-federation-nickname="ByTTV" club-name="SV Isengart" sex="1" ttr-match-count="214" lastname="Rung" ttr="1756" internal-nr="NU1016452" club-nr="110020" firstname="Lucky" birthyear="1984"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER24">
     |            <person licence-nr="105002220" club-federation-nickname="ByTTV" club-name="TSV Lummerland e.V." sex="1" ttr-match-count="221" lastname="Thien" ttr="1957" internal-nr="NU1128537" club-nr="105002" firstname="Niko" birthyear="1958"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER25">
     |            <person licence-nr="105002221" club-federation-nickname="ByTTV" club-name="TSV Lummerland e.V." sex="1" ttr-match-count="16" lastname="Zwerg" ttr="1937" internal-nr="NU1137488" club-nr="105002" firstname="Nat" birthyear="1995"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER26">
     |            <person licence-nr="113006076" club-federation-nickname="ByTTV" club-name="TSV 1864 Twin Peaks" sex="1" ttr-match-count="245" lastname="Elmine" ttr="1831" internal-nr="NU1021171" club-nr="113006" firstname="Nick" birthyear="1961"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER27">
     |            <person licence-nr="112023004" club-federation-nickname="ByTTV" club-name="TTC Orgrimmar" sex="1" ttr-match-count="356" lastname="Sitter" ttr="1792" internal-nr="NU1022538" club-nr="112023" firstname="Till" birthyear="1960"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER28">
     |            <person licence-nr="113015101" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" sex="1" ttr-match-count="47" lastname="Ister" ttr="1932" internal-nr="NU1020762" club-nr="113015" firstname="Thorn" birthyear="1944"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER29">
     |            <person licence-nr="402010406" club-federation-nickname="ByTTV" club-name="DJK SB Pandora 64" sex="1" ttr-match-count="19" lastname="Retisch" ttr="2290" internal-nr="NU1334904" club-nr="402010" firstname="Theo" birthyear="1972"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER30">
     |            <person licence-nr="103008070" club-federation-nickname="ByTTV" club-name="TSV 1907 Indersdorf" sex="1" ttr-match-count="184" lastname="Ate" ttr="1802" internal-nr="NU1025642" club-nr="103008" firstname="Tom" birthyear="1967"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER31">
     |            <person licence-nr="112025021" club-federation-nickname="ByTTV" club-name="FC Askaban" sex="1" ttr-match-count="134" lastname="Zafen" ttr="1913" internal-nr="NU1129013" club-nr="112025" firstname="Wilhelm" birthyear="1967"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER32">
     |            <person licence-nr="401009068" club-federation-nickname="ByTTV" club-name="SV Sarching" sex="1" ttr-match-count="166" lastname="Elb-Rösel" ttr="1874" internal-nr="NU1023346" club-nr="401009" firstname="Sam" birthyear="1973"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER33">
     |            <person licence-nr="110018285" club-federation-nickname="ByTTV" club-name="TTC Phatt Island" sex="1" ttr-match-count="429" lastname="Rator" ttr="1847" internal-nr="NU1023592" club-nr="110018" firstname="Sepp A." birthyear="1994"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER34">
     |            <person licence-nr="110010102" club-federation-nickname="ByTTV" club-name="ESV Wonderland" sex="1" ttr-match-count="38" lastname="ter Holen" ttr="1956" internal-nr="NU1023635" club-nr="110010" firstname="Ron" birthyear="1964"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER35">
     |            <person licence-nr="110011468" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" sex="1" ttr-match-count="76" lastname="Held" ttr="1883" internal-nr="NU1184966" club-nr="110011" firstname="Roman" birthyear="1976"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER36">
     |            <person licence-nr="112004159" club-federation-nickname="ByTTV" club-name="TSV Posemuckl 1862" sex="1" ttr-match-count="392" lastname="Matisierung" ttr="1790" internal-nr="NU1029682" club-nr="112004" firstname="Stig" birthyear="1988"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER37">
     |            <person licence-nr="112623062" club-federation-nickname="ByTTV" club-name="TTC Orgrimmar" sex="1" ttr-match-count="350" lastname="Chen" ttr="1878" internal-nr="NU1029426" club-nr="112023" firstname="Qu" birthyear="1990"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER38">
     |            <person licence-nr="112020089" club-federation-nickname="ByTTV" club-name="TSV Schatzinsel" sex="1" ttr-match-count="353" lastname="Zufall" ttr="1765" internal-nr="NU1031921" club-nr="112020" firstname="Rainer" birthyear="1955"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER39">
     |            <person licence-nr="110411432" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" sex="1" ttr-match-count="76" lastname="Bär" ttr="1661" internal-nr="NU1229922" club-nr="110011" firstname="Roy" birthyear="1976"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER40">
     |            <person licence-nr="202004161" club-federation-nickname="ByTTV" club-name="TV Quahog" sex="1" ttr-match-count="54" lastname="Getter" ttr="1531" internal-nr="NU1136164" club-nr="202004" firstname="David" birthyear="1992"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER41">
     |            <person licence-nr="202004168" club-federation-nickname="ByTTV" club-name="TV Quahog" sex="1" ttr-match-count="82" lastname="Haar" ttr="1904" internal-nr="NU1281105" club-nr="202004" firstname="Ross" birthyear="1999"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER42">
     |            <person licence-nr="113009184" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" sex="1" ttr-match-count="181" lastname="Kommen" ttr="1904" internal-nr="NU1034057" club-nr="113009" firstname="Ryan" birthyear="1987"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER43">
     |            <person licence-nr="112023107" club-federation-nickname="ByTTV" club-name="TTC Orgrimmar" sex="1" ttr-match-count="58" lastname="Pler" ttr="1752" internal-nr="NU1200096" club-nr="112023" firstname="Sam" birthyear="1969"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER44">
     |            <person licence-nr="104010110" club-federation-nickname="ByTTV" club-name="SC Xanadu" sex="1" ttr-match-count="142" lastname="Dom" ttr="1801" internal-nr="NU1039231" club-nr="104010" firstname="Stefan S." birthyear="1958"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER45">
     |            <person licence-nr="105002194" club-federation-nickname="ByTTV" club-name="TSV Lummerland e.V." sex="1" ttr-match-count="81" lastname="O'Wierung" ttr="2019" internal-nr="NU1231950" club-nr="105002" firstname="Ted" birthyear="1956"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER46">
     |            <person licence-nr="103022295" club-federation-nickname="ByTTV" club-name="TTC Zaremonien" sex="1" ttr-match-count="242" lastname="Harmonie" ttr="1711" internal-nr="NU1042740" club-nr="103022" firstname="Phil" birthyear="1985"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER47">
     |            <person licence-nr="109014130" club-federation-nickname="ByTTV" club-name="SV Blood Island" sex="1" ttr-match-count="187" lastname="Motor" ttr="1778" internal-nr="NU1043423" club-nr="109014" firstname="Otto" birthyear="1984"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER48">
     |            <person licence-nr="102006122" club-federation-nickname="ByTTV" club-name="Post SV Avalon" sex="1" ttr-match-count="187" lastname="Söhnlich" ttr="1771" internal-nr="NU1052396" club-nr="102006" firstname="Peer" birthyear="1975"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER49">
     |            <person licence-nr="105008044" club-federation-nickname="ByTTV" club-name="SC Dinky Island" sex="1" ttr-match-count="106" lastname="Pferd" ttr="1851" internal-nr="NU1258460" club-nr="105008" firstname="Nils" birthyear="1965"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER50">
     |            <person licence-nr="106017121" club-federation-nickname="ByTTV" club-name="SC Narnia" sex="1" ttr-match-count="465" lastname="Leum" ttr="1962" internal-nr="NU1056499" club-nr="106017" firstname="Pedro" birthyear="1961"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER51">
     |            <person licence-nr="110011433" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" sex="1" ttr-match-count="37" lastname="Päde" ttr="1657" internal-nr="NU1055357" club-nr="110011" firstname="Otto" birthyear="1976"/>	
     |          </player>
     |        
     |          <player type="single" id="PLAYER52">
     |            <person licence-nr="103022128" club-federation-nickname="ByTTV" club-name="TTC Zaremonien" sex="1" ttr-match-count="223" lastname="Welle" ttr="1733" internal-nr="NU1060735" club-nr="103022" firstname="Mirko" birthyear="1977"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER53">
     |            <person licence-nr="205003172" club-federation-nickname="ByTTV" club-name="SV PanneKlopper" sex="1" ttr-match-count="200" lastname="Rofon" ttr="1793" internal-nr="NU1060146" club-nr="205003" firstname="Mike" birthyear="1992"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER54">
     |            <person licence-nr="106013324" club-federation-nickname="ByTTV" club-name="TSV 1880 Reichenstadt" sex="1" ttr-match-count="17" lastname="Jess" ttr="1975" internal-nr="NU1311290" club-nr="106013" firstname="Matt" birthyear="1992"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER55">
     |            <person licence-nr="105018109" club-federation-nickname="ByTTV" club-name="TTC Torpedo Moskau" sex="1" ttr-match-count="43" lastname="Strammer" ttr="1784" internal-nr="NU1175847" club-nr="105018" firstname="Max" birthyear="1978"/>
     |          </player>
     |        
     |      </players>
     |    </competition>
     |  
     |    <competition start-date="${dateStr} 08:00" ttr-from="0" ttr-to="1200" ttr-remarks="D-Klasse" age-group="Damen" type="Einzel">
     |      <players>
     |          <player type="single" id="PLAYER56">
     |            <person licence-nr="102011102" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" sex="1" ttr-match-count="155" lastname="Arm" ttr="1195" internal-nr="NU998576" club-nr="102011" firstname="Lene" birthyear="1976"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER57">
     |            <person licence-nr="110018353" club-federation-nickname="ByTTV" club-name="TTC Phatt Island" sex="1" ttr-match-count="82" lastname="Macchiato" ttr="1000" internal-nr="NU1279403" club-nr="110018" firstname="Lotte" birthyear="1962"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER58">
     |            <person licence-nr="106003307" club-federation-nickname="ByTTV" club-name="TSV Jux" sex="1" ttr-match-count="260" lastname="Ten Platz" ttr="1020" internal-nr="NU1127543" club-nr="106003" firstname="Liz" birthyear="1991"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER59">
     |            <person licence-nr="104008192" club-federation-nickname="ByTTV" club-name="1. SC Haarzopf" sex="1" ttr-match-count="264" lastname="Fehr" ttr="1172" internal-nr="NU999584" club-nr="104008" firstname="Luzie" birthyear="1984"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER60">
     |            <person licence-nr="106017260" club-federation-nickname="ByTTV" club-name="SC Narnia" sex="1" ttr-match-count="348" lastname="Quark" ttr="954" internal-nr="NU1005416" club-nr="106017" firstname="Marga" birthyear="1980"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER61">
     |            <person licence-nr="113009212" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" sex="1" ttr-match-count="119" lastname="Sport" ttr="873" internal-nr="NU1004059" club-nr="113009" firstname="Rita" birthyear="1995"/>	
     |          </player>
     |        
     |          <player type="single" id="PLAYER62">
     |            <person licence-nr="116024054" club-federation-nickname="ByTTV" club-name="SV Buntekuh" sex="1" ttr-match-count="229" lastname="Nist" ttr="1111" internal-nr="NU1002499" club-nr="116024" firstname="Pia" birthyear="1984"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER63">
     |            <person licence-nr="105010060" club-federation-nickname="ByTTV" club-name="SV Champhausen" sex="1" ttr-match-count="315" lastname="Zeih" ttr="1146" internal-nr="NU1008415" club-nr="105010" firstname="Polly" birthyear="1966"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER64">
     |            <person licence-nr="109012113" club-federation-nickname="ByTTV" club-name="TV Entenhausen" sex="1" ttr-match-count="311" lastname="Enzien" ttr="1003" internal-nr="NU1009163" club-nr="109012" firstname="Ingrid" birthyear="1996"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER65">
     |            <person licence-nr="113009214" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" sex="1" ttr-match-count="166" lastname="Tick" ttr="935" internal-nr="NU1008691" club-nr="113009" firstname="Joyce" birthyear="1995"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER66">
     |            <person licence-nr="102010067" club-federation-nickname="ByTTV" club-name="TSV Lederhose" sex="1" ttr-match-count="174" lastname="Ass" ttr="826" internal-nr="NU1014014" club-nr="102010" firstname="Caro" birthyear="1995"/>	
     |          </player>
     |        
     |          <player type="single" id="PLAYER67">
     |            <person licence-nr="113009215" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" sex="1" ttr-match-count="151" lastname="Biener-Haken" ttr="1177" internal-nr="NU1014471" club-nr="113009" firstname="Cara" birthyear="1995"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER68">
     |            <person licence-nr="112011158" club-federation-nickname="ByTTV" club-name="SG Hogsmeade" sex="1" ttr-match-count="450" lastname="Leuchter" ttr="1129" internal-nr="NU1019633" club-nr="112011" firstname="Christa" birthyear="1961"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER69">
     |            <person licence-nr="113009233" club-federation-nickname="ByTTV" club-name="TSV WuChiChu" sex="1" ttr-match-count="53" lastname="Manten" ttr="1009" internal-nr="NU1177338" club-nr="113009" firstname="Claudia" birthyear="1986"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER70">
     |            <person licence-nr="103029171" club-federation-nickname="ByTTV" club-name="SV Feucht" sex="1" ttr-match-count="252" lastname="Grube" ttr="1331" internal-nr="NU1022365" club-nr="103029" firstname="Claire" birthyear="1973"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER71">
     |            <person licence-nr="202005099" club-federation-nickname="ByTTV" club-name="DJK SV Mückenloch" sex="1" ttr-match-count="214" lastname="Fähre" ttr="1169" internal-nr="NU1021958" club-nr="202005" firstname="Conny" birthyear="1967"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER72">
     |            <person licence-nr="105005337" club-federation-nickname="ByTTV" club-name="FT Vogelsang 1966" sex="1" ttr-match-count="314" lastname="Schiert" ttr="1084" internal-nr="NU1020649" club-nr="105005" firstname="Cora" birthyear="1957"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER73">
     |            <person licence-nr="112006264" club-federation-nickname="ByTTV" club-name="TTC Garching" sex="1" ttr-match-count="225" lastname="Kaemmerling" ttr="917" internal-nr="NU239198" club-nr="112006" firstname="Thorsten" birthyear="1982"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER74">
     |            <person licence-nr="112004206" club-federation-nickname="ByTTV" club-name="TSV Posemuckl 1862" sex="1" ttr-match-count="397" lastname="Littchen" ttr="933" internal-nr="NU1025584" club-nr="112004" firstname="Donna" birthyear="1995"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER75">
     |            <person licence-nr="111012015" club-federation-nickname="ByTTV" club-name="TSV Altenheim" sex="1" ttr-match-count="397" lastname="Kette" ttr="1085" internal-nr="NU1026037" club-nr="111012" firstname="Anka" birthyear="1965"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER76">
     |            <person licence-nr="608004133" club-federation-nickname="ByTTV" club-name="TSV Greding" sex="1" ttr-match-count="268" lastname="Kern jun." ttr="1046" internal-nr="NU1024310" club-nr="608004" firstname="Martin" birthyear="1992"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER77">
     |            <person licence-nr="608014266" club-federation-nickname="ByTTV" club-name="1. FC Katzenhirn" sex="1" ttr-match-count="318" lastname="Probe" ttr="1145" internal-nr="NU1023481" club-nr="608014" firstname="Ann" birthyear="1985"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER78">
     |            <person licence-nr="109001045" club-federation-nickname="ByTTV" club-name="TTC Kuhbier" sex="1" ttr-match-count="239" lastname="Nass" ttr="1130" internal-nr="NU1045465" club-nr="109001" firstname="Anna" birthyear="1962"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER79">
     |            <person licence-nr="110015224" club-federation-nickname="ByTTV" club-name="TSV Schlägerholz" sex="1" ttr-match-count="209" lastname="Stoppel" ttr="1183" internal-nr="NU1048485" club-nr="110815" firstname="Bart" birthyear="1973"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER80">
     |            <person licence-nr="102011076" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" sex="1" ttr-match-count="255" lastname="Lldich" ttr="1112" internal-nr="NU1053373" club-nr="102011" firstname="Bea" birthyear="1986"/>
     |          </player>
     |      </players>
     |    </competition>
     |  
     |    <competition start-date="${dateStr} 08:00" ttr-from="1501" ttr-to="1650" ttr-remarks="B-Klasse" age-group="Damen" type="Einzel">
     |      <players>
     |          <player type="single" id="PPPPPP81">
     |            <person licence-nr="103006228" club-federation-nickname="ByTTV" club-name="TSV Hundeluft 65" sex="1" ttr-match-count="244" lastname="Wetter" ttr="1547" internal-nr="NU995873" club-nr="103006" firstname="Donna" birthyear="1992"/>
     |          </player>
     |        
     |          <player type="single" id="LLLLLL82">
     |            <person licence-nr="113013283" club-federation-nickname="ByTTV" club-name="VfL Busenberg e.V." sex="1" ttr-match-count="275" lastname="Nut" ttr="1547" internal-nr="NU996465" club-nr="113013" firstname="Ella" birthyear="1963"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER83">
     |            <person licence-nr="110011360" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" sex="1" ttr-match-count="258" lastname="Poly" ttr="1535" internal-nr="NU997673" club-nr="110011" firstname="Esther" birthyear="1995"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER84">
     |            <person licence-nr="111010235" club-federation-nickname="ByTTV" club-name="TSV Sommerloch" sex="1" ttr-match-count="326" lastname="Muhn" ttr="1649" internal-nr="NU205625" club-nr="111010" firstname="Hanni" birthyear="1999"/>	
     |          </player>
     |        
     |          <player type="single" id="PLAYER85">
     |            <person licence-nr="112005008" club-federation-nickname="ByTTV" club-name="TTC Scheibenwelt" sex="1" ttr-match-count="238" lastname="Fisch" ttr="1618" internal-nr="NU998085" club-nr="112005" firstname="Grete" birthyear="1981"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER86">
     |            <person licence-nr="402015203" club-federation-nickname="ByTTV" club-name="SC Pandora 64 e.V." sex="1" ttr-match-count="384" lastname="Ihr" ttr="1534" internal-nr="NU1001065" club-nr="402015" firstname="Insa" birthyear="1989"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER87">
     |            <person licence-nr="109012147" club-federation-nickname="ByTTV" club-name="TV Entenhausen" sex="1" ttr-match-count="290" lastname="Sehr" ttr="1616" internal-nr="NU1009164" club-nr="109012" firstname="Malte" birthyear="1964"/>
     |          </player>
     |        
     |          <player type="single" id="PLAYER88">
     |            <person licence-nr="103009274" club-federation-nickname="ByTTV" club-name="TSV Gammelshausen e.V." sex="1" ttr-match-count="199" lastname="Krissmes" ttr="1454" internal-nr="NU1008382" club-nr="103009" firstname="Mary" birthyear="1978"/>
     |          </player>
     |             <player type="single" id="PLAYER89">
     |            <person licence-nr="103009274" club-federation-nickname="ByTTV" club-name="TSV Gammelshausen e.V." sex="1" ttr-match-count="199" lastname="Nengraben" ttr="1454" internal-nr="NU1008382" club-nr="103009" firstname="Maria" birthyear="1978"/>
     |          </player>					
     |              
     |          <player type="single" id="PLAYER90">
     |            <person licence-nr="102011102" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" sex="1" ttr-match-count="155" lastname="Arm" ttr="1195" internal-nr="NU998576" club-nr="102011" firstname="Lene" birthyear="1976"/>
     |          </player>
     |          
     |          <player type="single" id="PLAYER91">
     |            <person licence-nr="102011076" club-federation-nickname="ByTTV" club-name="MBB SG Motzen" sex="1" ttr-match-count="255" lastname="Lldich" ttr="1112" internal-nr="NU1053373" club-nr="102011" firstname="Bea" birthyear="1986"/>
     |          </player>
     |
     |          <player type="single" id="PLAYER92">
     |            <person licence-nr="177011076" club-federation-nickname="ByTTV" club-name="TTC Freising" sex="1" ttr-match-count="255" lastname="Lldich" ttr="1112" internal-nr="NU1057773" club-nr="102011" firstname="Bea" birthyear="1964"/>
     |          </player>
     |          
     |      </players>
     |    </competition>
     |  
     |    <competition start-date="${dateStr} 14:00" ttr-from="1650" ttr-to="3000" ttr-remarks="S-Klasse" age-group="Herren" type="Doppel">		
     |      <players>
     |          <player type="double" id="PLAYER93">
     |            <person licence-nr="111009155" club-federation-nickname="ByTTV" club-name="TV Traktor Leipzig" sex="1" ttr-match-count="351" lastname="Kahn" ttr="1792" internal-nr="NU1000999" club-nr="111009" firstname="Ada" birthyear="1965"/>
     |            <person licence-nr="105018106" club-federation-nickname="ByTTV" club-name="TTC Torpedo Moskau" sex="1" ttr-match-count="118" lastname="Tion" ttr="1846" internal-nr="NU210649" club-nr="105018" firstname="Addi" birthyear="1947"/>
     |          </player>
     |        
     |          <player type="double" id="PLAYER94">
     |            <person licence-nr="113015005" club-federation-nickname="ByTTV" club-name="TSV Rostocker Flammen" sex="1" ttr-match-count="250" lastname="Lette" ttr="1922" internal-nr="NU999395" club-nr="113015" firstname="Adi" birthyear="1960"/>
     |            <person licence-nr="112020111" club-federation-nickname="ByTTV" club-name="TSV Schatzinsel" sex="1" ttr-match-count="315" lastname="ter Native" ttr="1680" internal-nr="NU1004283" club-nr="112020" firstname="Al" birthyear="1957"/>
     |          </player>
     |        
     |          <player type="double" id="PLAYER95">
     |            <person licence-nr="110011337" club-federation-nickname="ByTTV" club-name="FC Lokomotive Berlin" sex="1" ttr-match-count="77" lastname="Herum" ttr="2190" internal-nr="NU1009099" club-nr="110011" firstname="Albert" birthyear="1981"/>				
     |            <person licence-nr="103013138" club-federation-nickname="ByTTV" club-name="SpVgg Röhrmoos-Großinzemoos" sex="1" ttr-match-count="123" lastname="Platz" ttr="1761" internal-nr="NU1314710" club-nr="103013" firstname="Alexander" birthyear="1962"/>
     |          </player>
     |        
     |          <player type="double" id="PLAYER96">
     |            <person licence-nr="112002146" club-federation-nickname="ByTTV" club-name="FC Kiffen 08" sex="1" ttr-match-count="392" lastname="Bert" ttr="1836" internal-nr="NU1006294" club-nr="112002" firstname="Ali" birthyear="1965"/>
     |            <person licence-nr="112024034" club-federation-nickname="ByTTV" club-name="VfR Netzroller" sex="1" ttr-match-count="251" lastname="Mente" ttr="1927" internal-nr="NU1006207" club-nr="112024" firstname="Ali" birthyear="1972"/>
     |          </player>
     |        
     |      </players>
     |    </competition>
     |  
     |</tournament>
    """.stripMargin
  }

}