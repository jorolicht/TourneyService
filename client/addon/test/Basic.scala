package addon.test

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service imports
//import shared.utils._
import shared.utils.Routines._

import shared.model._
import shared.utils.Constants._

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }

import org.scalajs.dom


@JSExportTopLevel("AddonBasic")
object AddonBasic extends UseCase("AddonBasic") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, param: String):Future[Boolean]= {
    number match {
      case  0 => test_0(param)
      case  1 => test_1(param); Future(true)
      case  2 => test_2();      Future(true)
      case  3 => test_3(param); Future(true)
      case  4 => test_4(param); Future(true)
      case  5 => test_5(param)
      case  6 => test_6(param)
      case  7 => test_7(param)
      case  8 => test_8(param)
      case  9 => test_9(param)
      case 10 => test_10(param)
      case 11 => test_11(param)
      case 12 => test_12(param)
      case 13 => test_13(param)

      case _ => AddonMain.setOutput(s"ERROR: invalid test number ${number}"); Future(true)
    }
  }


  // Test 0 - default basic tests
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test;param=hello
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test;param=blossom
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test;param=login
  def test_0(param: String) = {
    import cats.data.EitherT
    import cats.implicits._ 
    val test = s"START: Basic Test param:${param}"
    AddonMain.setOutput(test)

    param.toLowerCase()  match {
      case "login"   => {
        var toId = 1L
        
        (for {
          valid   <- EitherT(authBasicContext("info@turnier-service.com", "", "Ah3tQE"))
          result  <- EitherT(App.loadRemoteTourney(toId))
        } yield { (valid, result) }).value.map {
          case Left(err)    => AddonMain.setOutput(s"ERROR: load tourney toId:${toId} failed with: ${err.msgCode}"); true
          case Right(res)   => {
            val coId = App.tourney.getCurCoId
            App.execUseCase("OrganizeCompetition", "", "")
            AddonMain.addOutput(s"Competition status: ${App.tourney.calcCompStatus(coId).toString} / ${App.tourney.comps(coId).status.toString}")
            AddonMain.addOutput(s"SUCCESS: Basic Test param:${param}")
            true
          }
        }
      } 

      case "hello"   => AddonMain.addOutput(s"Hello ${param}!"); Future(true)
      case "blossom" => {
        var edmonds = new Edmonds(js.Array(js.Array(1,2,10), js.Array(1,7,10), js.Array(2,3,12), js.Array(3,4,20), js.Array(3,5,20),
                                        js.Array(4,5,25), js.Array(5,6,10), js.Array(6,7,10), js.Array(7,8,8)), true)

        val result = edmonds.maxWeightMatching()
        AddonMain.addOutput(s"result maxWeightMatching")
        AddonMain.addOutput(s"Output: ${result.toString()}")
        Future(true)
      }
      case _       => {
        AddonMain.addOutput(s"ERROR: invalid param: ${param}")
        AddonMain.addOutput(s"Valid param: hello, blossom & login")
        Future(true)
      }  
    }      
    
    
  }  

// @js.native
// @JSGlobal
// class Edmonds(input: Array[Array[Int]], maxCardinality: Boolean) extends js.Object {
//   def maxWeightMatching(): js.Any = js.native
// }


  // Test 1 - date formatting
  def test_1(testDate: String): String = {
    import java.time._
    import java.time.format.DateTimeFormatter

    val sDate = testDate.toInt

    val year  = sDate / 10000
    val month = (sDate / 100) % 100
    val day   = sDate % 100

    val sourceDate = LocalDate.of(year, month, day)            // Source Date
    val newDate    = sourceDate.plusDays(10)                   // Adding 10 days to source date.
    val formatter  = DateTimeFormatter.ofPattern("yyyy-MM-dd") // Setting date format
    newDate.format(formatter)
  }

  // Test 2 - matching types in scalajs
  def test_2() = {
    def fun[T](data:T)= data match{
      case _:Int => "Int"
      case _:Double => "Double"
      case _:String => "String"
      case _ => "Undefined"
    } 

    val x = 5.6
    val y = "hero"
    val z = 234
    println(fun(x))
    println(fun(y))
    println(fun(z))
  }

  // Test 3 - csv generating
  def test_3(param: String) = {
    import shared.utils.Routines._
    import shared.utils.CSVConverter

    case class CCTest(name: String, surname: String, age: Int, yNo: Boolean, id: Option[Int], height: Int, width: Long)
    val input = s"""Robert·Lichtenegger·60·true··183·${param}"""
    println(s"START Test Basic 3: CSV generating with long param ${param}")
    
    val cct = CSVConverter[CCTest].from(input).toEither match {
      case Left(err)  => println(s"Read csv Failure: ${err}")
      case Right(res) => println(s"Read csv Success: ${res}")
    }

    val cct2 = CCTest("Robert", "Lichtenegger", 63, false, Some(45), 99, 121L)
    println(s"Generate csv: ${CSVConverter[CCTest].to(cct2)}")

  }  

  // Test 4 - Enum function
  def test_4(param: String) = {
    import shared.model.CompStatus
    import shared.model.CompTyp

    val cs = CompStatus.RUN
    val ct = CompTyp.SINGLE

    println(s"START Test Enum Function param->${param}")
    println(s"Get CompStatus message code: ${gM(cs.msgCode,"VORRRUNDE")}")
    println(s"Get CompTyp message code: ${gM(CompTyp.SINGLE.msgCode)}")

  }


  // Test 5 - Bootstrap Functionality
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%205%20%2Dp%20HALLOOOOO 
  def test_5(param: String): Future[Boolean] = {

    val test = s"START Test Basic 5 -> ${param}"
    AddonMain.setOutput(test)

    val content = """ 
      <div class="dropdown">
        <button class="btn btn-xs btn-outline-secondary dropdown-toggle" type="button" id="dropdownMenuButton" data-toggle="dropdown">
          Dropdown button
        </button>
        <div class="dropdown-menu">
          <a class="dropdown-item" href="javascript:AddonBasic.callbackTest5('cfg',this)">zurück zur Konfiguration</a>
          <a class="dropdown-item" href="javascript:AddonBasic.callbackTest5('aus',this)">zurück zur Ausloung</a>
          <a class="dropdown-item" href="javascript:AddonBasic.callbackTest5('ein',this)">Ergebniseingabe löschen</a>
        </div>
      </div>

      <div class="dropdown">
        <button class="btn btn-xs btn-outline-secondary dropdown-toggle" type="button" id="dropdownMenu2" data-toggle="dropdown">
          Dropdown
        </button>
        <div class="dropdown-menu" >
          <button class="dropdown-item" type="button" disabled>Action</button>
          <button class="dropdown-item" type="button">Another action</button>
          <button class="dropdown-item" type="button">Something else here</button>
        </div>
      </div>

    """

    App.execUseCase("HomeMain", "Test", content)

    Future(true)
  }  

  @JSExport
  def callbackTest5(cmd: String, elem: dom.raw.HTMLElement): Unit = {
    
    val level = getData(elem, "level", "")
    dom.window.alert(s"Data from button: ${cmd}")

  }  


  // Test 6 - Basic Test write Option[Int] -> Json
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%206%20%2Dparam%208 
  def test_6(param: String):Future[Boolean] = {
    import upickle.default._
    import upickle.default.{ReadWriter => RW, macroRW}

    val test = s"START Test Basic 6 encode Option[Int]-> ${param}"
    AddonMain.setOutput(test)

    val testInt = param.toIntOption

    AddonMain.setOutput(s"Write ${testInt} as JSON: ${write(testInt)}")
    Future(true)
  }

  // Test 7 - Basic Test for Shapeless HList
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%207%20%2D2Dparam%20gen 
  def test_7(param: String):Future[Boolean] = {
    import scala.collection.mutable.ArrayBuffer
    import shapeless.{::, HList, HNil}
    import shapeless.Generic

    class MyClass[H <: HList](hs: H)
    object MyClass {
      def apply[P <: Product, L <: HList](p: P)(implicit gen: Generic.Aux[P, L]) = new MyClass[L](gen.to(p))
    }

    val test = s"START Test Basic 7 Shapeless HList -> ${param}"
    AddonMain.setOutput(test)

    val x = MyClass(2,4)
    type MyHList = Int :: String :: HNil

    val xxx = new ArrayBuffer[MyClass[Int :: String :: HNil]]()
    val testInt = param.toIntOption

    Future(true)
  }





  // Test 8 - Basic Test Read File Input
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%208%20%2D%2Dparam%20gen 
  def test_8(param: String):Future[Boolean] = {
    import org.scalajs.dom.{ Event, FileReader, UIEvent}
    import scala.collection.mutable.ArrayBuffer

    val test = s"START Test Basic 8 Read Input File -> ${param}"
    AddonMain.setOutput(test)

    val content = """ 
      <input type="file" id="myFile" > 
    """
    App.execUseCase("HomeMain", "Test", content)

    val fileInput = dom.document.getElementById("myFile").asInstanceOf[dom.html.Input]

    fileInput.onchange = e => {
      val reader = new dom.FileReader()
      reader.readAsText(fileInput.files(0))
      reader.onload = (e:UIEvent) => {
        val contents = reader.result.asInstanceOf[String]
        dom.console.log(contents)
      }
    }
    // Name, Vorname, Club, TTR, Year
    Future(true)
  }


  // Test 9 - Basic Test generate hashCode
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%209%20%2D%2Dparam%20gen 
  def test_9(param: String):Future[Boolean] = {

    val test = s"START Test 9 - Basic Test generate hashCode -> ${param}"
    val hCodeTest = "Abcde@ß"
    AddonMain.setOutput(test)
    AddonMain.addOutput(s"Hashcode from ${param} -> ${param.hashCode.toString}")
    AddonMain.addOutput(s"Hashcode from ${hCodeTest} -> ${hCodeTest.hashCode.toString}")

    Future(true)
  }

  // Test 10 - Basic Test Tree Structure
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%2010%20%2D%2Dparam%20gen 
  def test_10(param: String):Future[Boolean] = {
    import shared.model.DrawTree

    val test = s"START Test 10 - Basic Test Tree Structure -> ${param}"
    //AddonMain.setOutput(test)
    val x = DrawTree.init[Long](1,16, 0L)

    x match {
      case None        => {}
      case Some(tree)  => {
        tree.addItem[Long](tree, 2, 1001L)
        tree.addItem[Long](tree, 16, 1002L)
        tree.addItem[Long](tree, 3, 1001L)
        tree.addItem[Long](tree, 4, 1003L)
        tree.addItem[Long](tree, 15, 1003L)
      }
    }

    DrawTree.treePrint(x)
    //val tree = BTree[Int](4, Some(BTree(1)), Some(BTree(2, Some(BTree(5)), Some(BTree(6)))))

    val y = DrawTree.init[String](1,16, "")
    y match {
      case None        => {}
      case Some(tree)  => {
        tree.addItem[String](tree, 2, "A")
        tree.addItem[String](tree, 16, "B")
        tree.addItem[String](tree, 3, "C")
        tree.addItem[String](tree, 4, "D")
        tree.addItem[String](tree, 15,"A")
      }
    }
    DrawTree.treePrint(y)

    Future(true)
  }

  // Test 11 - Basic Test Basic Test Pairing Tables by Richard Schurig
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%2011%20%2D%2Dparam%207 
  def test_11(param: String):Future[Boolean] = {
  
    def getBye(pair: (Int,Int), value: Int) = if (value == pair._1) pair._2 else pair._1

    def rotateLeft[A](seq: Seq[A], i: Int): Seq[A] = {
      val size = seq.size
      seq.drop(i % size) ++ seq.take(i % size)
    }

    def rotateRight[A](seq: Seq[A], i: Int): Seq[A] = {
      val size = seq.size
      seq.drop(size - (i % size)) ++ seq.take(size - (i % size))
    }

    def rotateUntilByeIsHead(seq: Array[(Int,Int)], bye: Int ): Array[(Int,Int)] = {
      val index = seq.indexWhere(elem => (elem._1 == bye) || (elem._2 == bye))
      rotateLeft(seq, index).to(Array)
    }  

    def rotateNotHead(seq: Array[(Int,Int)], last: (Int,Int)): Int = 
      seq.indexWhere(elem => (elem._1 != last._1) && (elem._1 != last._2)  && (elem._2 != last._1) && (elem._2 != last._2) )

    def rotateUntilLastIsNotHead(seq: Array[(Int,Int)], last: (Int,Int)): Array[(Int,Int)] = {
      val index = seq.indexWhere(elem => (elem._1 != last._1) && (elem._1 != last._2)  && (elem._2 != last._1) && (elem._2 != last._2) )
      rotateLeft(seq, index).to(Array)
    } 

    val test = s"START Test 11 - Basic Test Pairing Tables by Richard Schurig -> ${param}"
    val size = param.toIntOption.getOrElse(0)

    // examples
    // size  = 3  4  5  6  7  8  
    // even  = 4  4  6  6  8  8 
    // noMpR = 2' 2  3' 3  4' 4   number of matches per Round incl bye(') 
    // rnds  = 3  3  5  5  7  7 
                                                       
    val even    = size + size%2                        
    val noMpR   = even/2                                                            
    val rnds    = even - 1                           
    val rsArray = Array.ofDim[Int](rnds, noMpR)
    val rs2Array = Array.ofDim[Int](rnds, noMpR)

    val rsPair  = Array.ofDim[(Int,Int)](rnds, noMpR)
    val rsPair1 = Array.ofDim[(Int,Int)](rnds, noMpR)
    val rsPair2 = Array.ofDim[(Int,Int)](rnds, noMpR)

    // fill array as Richard Schurig stated
    var cnt = 1
    for (i<-0 until rnds; j<-0 until noMpR) {
      rsArray(i)(j) = if ((cnt % (rnds)) == 0) rnds else (cnt % (rnds))
      cnt = cnt +1 
    }
    
    // construct 2nd table
    for (i<-0 until rnds; j<-0 until noMpR) {
      // val row = (i+1)%rnds
      // val col = if (j!=0) noMpR-j-1
      rs2Array(i)(j) = if (j!=0) rsArray((i+1)%rnds)(noMpR-j-1) else even
    }
  
    // generate paaring, swap first column entry on every second row
    for (i<-0 until rnds; j<-0 until noMpR) {
      rsPair(i)(j) = if ((i%2)==1 && j==0)  (rs2Array(i)(j), rsArray(i)(j)) else (rsArray(i)(j), rs2Array(i)(j))
    }

    println(s"Step 1 - GENERATE PAIRING")
    // step 1 - change rounds (first keep first .. last goes 2nd)
    for (i<-0 until rnds) {
      val j = if (i==0) 0 else rnds-i 
      rsPair1(i) = rsPair(j)
    }

    // step 2 - fix time to next game for each player/team
    println(s"Step 2 - OPTIMIZE") 
    if (size%2 == 1) { 
      // odd sized groups
      for (i<-0 until rnds) {
        // first line  
        if (i == 0) { 
          // println(s"odd (${i+1})")
          // println(s"odd (${i+1}) ${rsPair1(0).drop(1).mkString(":")}")
          // println(s"odd (${i+1}) ${rsPair1(0).take(1)}")

          rsPair2(0) = rsPair1(0).drop(1) ++ rsPair1(0).take(1)
          
        } else {
          //println(s"odd (${i+1}) bye: ${getBye(rsPair1(i-1)(0), even)} ") 
          //bye in one round should have first match in next round
          val bye    = getBye(rsPair1(i-1)(0), even) 
          //println(s"odd rotate (${i+1})") 
          rsPair2(i) = rotateUntilByeIsHead(rsPair1(i).drop(1), bye) ++ rsPair1(i).take(1)
        }
      }
    } else {          
      // even sized groups
      for (i<-0 until rnds) {
        // keep first line
        if (i == 0) { 
          rsPair2(0) = rsPair1(0) 
        } else {
          //get last of previous round, should not be first in next round
          val last = rsPair2(i-1)(noMpR-1)
          //println(s"rotate(${i+1}) ${rsPair1(i).mkString(":")} last ${last} times: ${rotateNotHead(rsPair1(i),last)}")
          rsPair2(i) = rotateUntilLastIsNotHead(rsPair1(i), last)
        }
      }
    }
 

    println(s"Step 3 - PRINT" ) 
    if (size%2 == 1) {
      for (i<-0 until rnds) println(s"Runde ${i+1} -> ${rsPair2(i).dropRight(1).mkString(":")}")
    } else {
      for (i<-0 until rnds) println(s"Runde ${i+1} -> ${rsPair2(i).mkString(":")}")
    }
    Future(true)
  }


  // Test 12 - Basic Test PantStatus Check
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%2012%20%2D%2Dparam%207 
  def test_12(param: String):Future[Boolean] = {
    import shared.model.PantStatus
    import shared.model.CompTyp

    val test = s"START Test 12 - Basic Test PantStatus Check -> ${param}"
    AddonMain.addOutput(test)

    val x = PantStatus.REDY
    val a = x.equalsTo(PantStatus.REDY)
    val b = x.equalsTo(PantStatus.REGI, PantStatus.FINI)
    val c = x.equalsTo(PantStatus.REGI, PantStatus.FINI, PantStatus.REDY)


    val xt = CompTyp.SINGLE.name(gM)

    AddonMain.addOutput(s"Check PantStatus.REDY with PantStatus.REDY  ${a}")
    AddonMain.addOutput(s"Check PantStatus.REDY with PantStatus.REGI, PantStatus.FINI ${b}")
    AddonMain.addOutput(s"Check PantStatus.REDY with PantStatus.REGI, PantStatus.FINI, PantStatus.REDY ${c}")

    AddonMain.addOutput(s"Get CompTyp.SINGLE name ${xt}")

    Future(true)
  }    

  // Test 13 - Basic Test UTC Timestamp
  // http://localhost:9000/start?ucName=HomeMain&ucParam=Debug&ucInfo=test%20%2Ds%20basic%20%2Dn%2013%20%2D%2Dparam%207 
  def test_13(param: String):Future[Boolean] = {

    def getUTCTimestamp(): String = {
      val date  = new js.Date()
      f"${date.getUTCFullYear().toInt}%04d${date.getUTCMonth().toInt+1}%02d${date.getUTCDate().toInt}%02d${date.getUTCHours().toInt}%02d${date.getUTCMinutes().toInt}%02d${date.getUTCSeconds().toInt}%02d"
    }

    val test = s"START Test 13 - Basic Test UTC Timestamp -> ${param}"
    AddonMain.addOutput(test)

    AddonMain.addOutput(s"UTC Time: ${getUTCTimestamp()}")

    Future(true)
  }  


}


@js.native
@JSGlobal
class Edmonds(input: js.Array[js.Array[Int]], maxCardinality: Boolean) extends js.Object {
  def maxWeightMatching(): js.Array[Int] = js.native
}
