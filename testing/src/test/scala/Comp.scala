import org.scalatest._
import org.scalatest.matchers._
import org.scalatestplus.selenium._
import org.openqa.selenium._
import org.openqa.selenium.WebDriver
import org.openqa.selenium.support.ui._

class CompEncode extends flatspec.AnyFlatSpec with should.Matchers with WebBrowser {
  implicit val webDriver: WebDriver = new org.openqa.selenium.safari.SafariDriver()  
  val host = "http://localhost:9000/"

  "CompEncode" should "update/create competition KO and group phases" in {
    go to (host)
    executeScript("Start.testCompEncode('182');")

    val resultElement = new WebDriverWait(webDriver, 10).until(
      new ExpectedCondition[WebElement] { override def apply(d: WebDriver) = d.findElement(By.id("APP_Result")) }
    )

    val resText = resultElement.getText()
    info(resText)

    resText should startWith regex "SUCCESS"
    quit(); Thread.sleep(500)
  }


}

