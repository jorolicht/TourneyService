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

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }

object AddonBasic extends UseCase("AddonBasic") 
  with TourneySvc with LicenseSvc with AuthenticateSvc with WrapperSvc
{
  
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, param: String)= {
    number match {
      case 0 => test_0(param)
      case 1 => test_1(param)
      case 2 => test_2()
      case 3 => test_3(param)
      case _ => println("Invalid BASIC test option")
    }
  }


  // Test 0 - hello world
  def test_0(text: String) = s"Hello ${text}!"

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


}
