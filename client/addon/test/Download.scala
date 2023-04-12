package addon.test

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs._

import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

// tourney service imports
import shared.utils._
import shared.utils.Routines._

import scalajs.usecase.component.BasicHtml._
import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import org.w3c.dom.html.HTMLAnchorElement


object AddonDownload extends UseCase("AddonDownload") 
  with TourneySvc with AuthenticateSvc with WrapperSvc
{
  def render(testCase:String = "", testOption:String = "", reload:Boolean = false) = {}

  def execTest(number: Int, param: String)= {
    number match {
      case 0 => test_0(param)
      case 1 => test_1(param)
      // case 6 => test_6(param)
    }
  }

  def test_0(param: String) = {
    import cats.data.EitherT
    import cats.implicits._     
    import scalajs.usecase.dialog.DlgSpinner
    import scalajs.usecase.dialog.DlgBox
    import shared.utils.Constants._

    val toId = param.toLongOption.getOrElse(185L)
    
    println(s"---> Start Download Test 0: toId ${param}")
    
    (for {
      pw        <- EitherT(authReset("", "ttcdemo/FED89BFA1BF899D590B5", true ))
      coValid   <- EitherT(authBasicContext("","ttcdemo/FED89BFA1BF899D590B5", pw))
      result    <- EitherT(App.loadRemoteTourney(toId))
    } yield { (pw) }).value.map {
      case Left(err)  => dom.window.alert(s"ERROR: authentication failed with: ${getError(err)}")
      case Right(res) => { 
        DlgSpinner.start("Start Transfer")
        downloadFile(DownloadType.CTTResult).map {
          case Left(err)  => DlgSpinner.error("Fehler")  
          case Right(res) => DlgBox.standard("Bestätigung Speichern ...", s"Soll die Ergebnisdatei ${res._1} gespeichert werden", Seq("no", "yes"), 1, true). map {
            case 2 => {
              DlgSpinner.close()
              println(s"downloafFile => RESULT fName: ${res._1} content: ${res._2.take(100)}")
            }   
            case _ => {
              DlgSpinner.close()
              println(s"downloafFile => Abbruch")
            }
          }
        }
      }
    }
  }


  def test_1(text: String) = {   
    import js.JSConverters._ 
    import scalajs.usecase.dialog.DlgBox
    import org.scalajs.dom.raw.HTMLAnchorElement
    println(s"---> Start Download Test 1:  ${text}")
    
    val data = new dom.Blob("Hallo WorldXXXX".toJSArray.asInstanceOf[scala.scalajs.js.Array[scala.scalajs.js.Any]], dom.raw.BlobPropertyBag("text/plain"))
    //val file = new dom.File("Hallo world", "file.txt", dom.raw.BlobPropertyBag("text/plain"))
    
    var url = dom.raw.URL.createObjectURL(data)

    getElemById_("APP__Download").asInstanceOf[HTMLAnchorElement].href = url
    setAttribute_("APP__Download", "download", "example.txt")

    DlgBox.standard("Datei Dlg", "Datei <xy> speichern", Seq("cancel", "yes"),0,true).map { _ match {
      case 1 => debug("confirm", "cancel"); false
      case 2 => {
        getElemById_("APP__Download").asInstanceOf[HTMLAnchorElement].click()
        debug("confirm", "yes"); true
      }  
      case _ => debug("confirm", "unknown"); false
    }}


  }


}