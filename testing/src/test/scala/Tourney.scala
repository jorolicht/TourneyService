import org.scalatest._
import org.scalatest.matchers._
import org.scalatestplus.selenium._
import org.openqa.selenium._
import org.openqa.selenium.WebDriver
import org.openqa.selenium.support.ui._

class TourneyLoad extends flatspec.AnyFlatSpec with should.Matchers with WebBrowser {
  implicit val webDriver: WebDriver = new org.openqa.selenium.safari.SafariDriver()  
  val host = "http://localhost:9000/"


  "TourneyLoad" should "be successful and return tourney" in {
    go to (host)
    executeScript("Start.testTourneyLoad();")

    val resultElement = new WebDriverWait(webDriver, 10).until(
      new ExpectedCondition[WebElement] { override def apply(d: WebDriver) = d.findElement(By.id("APP_Result")) }
    )

    val resText = resultElement.getText()
    info(resText)

    resText should startWith regex "SUCCESS"
    quit(); Thread.sleep(500)
  }
}

