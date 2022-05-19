import org.scalatest._
import org.scalatest.matchers._
import org.scalatestplus.selenium._
import org.openqa.selenium._
import org.openqa.selenium.WebDriver
import org.openqa.selenium.support.ui._

class BasicHello extends flatspec.AnyFlatSpec with should.Matchers with WebBrowser {
  implicit val webDriver: WebDriver = new org.openqa.selenium.safari.SafariDriver()  
  val host = "http://localhost:9000/"

  "BasicHello" should "return hello message" in {
    go to (host)
    var result = executeScript("return Start.testBasicHello('Robert');")
    
    info(s"result: ${result}")
    result should be ("Hello Robert!")
    quit(); Thread.sleep(500)
  }
}

class BasicDate extends flatspec.AnyFlatSpec with should.Matchers with WebBrowser {
  implicit val webDriver: WebDriver = new org.openqa.selenium.safari.SafariDriver()  
  val host = "http://localhost:9000/"

  "BasicDate" should "return correct date" in {
    go to (host)
    var result = executeScript("return Start.testBasicDate('20220219');")
    
    info(s"result: ${result}")
    result should be ("2022-03-01")
    quit(); Thread.sleep(500)
  }
}
