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

import scalajs.usecase.component._
import scalajs.service._
import scalajs.{ App, AppEnv }
import org.w3c.dom.html.HTMLAnchorElement

@JSExportTopLevel("StartDownload")
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
    import org.querki.jquery._ 
    import cats.data.EitherT
    import cats.implicits._     
    import scalajs.usecase.dialog.DlgSpinner
    import scalajs.usecase.dialog.DlgBox
    import scalajs.usecase.dialog.DlgInfo
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
        startSpinner()
        downloadFile(DownloadType.ClickTT).map {
          case Left(err)  => stopSpinner(); DlgInfo.show(gM("dlg.info.download.error.hdr"), getError(err), "danger") 
          case Right(res) => {
            stopSpinner()
            DlgBox.saveStringAsFile(gM("dlg.box.save.verify.hdr"), gM("dlg.box.save.verify.msg", res._1), res._1,  res._2)
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

    gE("APP__Download").asInstanceOf[HTMLAnchorElement].href = url
    setAttribute(gE("APP__Download"), "download", "example.txt")

    DlgBox.standard("Datei Dlg", "Datei <xy> speichern", Seq("cancel", "yes"),0,true).map { _ match {
      case 1 => debug("confirm", "cancel"); false
      case 2 => {
        gE("APP__Download").asInstanceOf[HTMLAnchorElement].click()
        debug("confirm", "yes"); true
      }  
      case _ => debug("confirm", "unknown"); false
    }}
  }

}