package scalajs.usecase.home

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.URIUtils
import scala.scalajs._

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.yamlijs.YAML                  // facade for yaml
import org.highlight._                   // highlight.org
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import org.scalajs.dom.raw.{ Event, HTMLInputElement } // for ScalaJs bindings

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import shared.model._
import shared.utils._

import scalajs.usecase.dialog._
import scalajs.usecase.component._
import scalajs.service._
import scalajs._

@JSExportTopLevel("HomeMain")
object HomeMain extends UseCase("HomeMain") 
  with TourneySvc
{

  def render(ucParam: String = "", ucInfo: String = "", reload: Boolean=false) = {
    ucParam match {
      case "None"              => { }

      case "Content"           => setContent(ucInfo)
      case "Error"             => setMainContent(clientviews.home.html.Error(ucInfo).toString)
      case "ErrorCode"         => setMainContent(clientviews.home.html.Error(getError(ucInfo)).toString)
      case "WelcomeOrganizer"  => setMainContent(clientviews.home.html.Welcome(AppEnv.getOrganizer, AppEnv.getFullName, getMsg("Welcome")).toString)
      case "WelcomeAdmin"      => { }
      case _                   => getContent("/content/welcome.md").map {
                                    case Left(errMsg)   => {}
                                    case Right(result)  => {
                                      val (content, author, pubDate) = markdown2html(result)
                                      setMainContent(clientviews.home.html.Main("Test"))
                                      $("#carouselContent").html(s"""<article class="markdown-body">$content</article>""") 
                                      setFooter(author, pubDate)
                                    }
                                  }
    }

  }
  

  def setContent(fName: String): Unit = {
    //debug("setContent", s"fName: ${fName}") 
    fName.split("\\.").last match {
      case "md"   => setContentMD(fName, false)
      case "html" => setContentFile(fName) 
      case "txt"  => setContentFile(fName) 
      case _       => setContentMD("/content/file_not_found.md", false)
    }
  }

  
  def setContentMD(fName: String, highlight: Boolean=false): Unit = {
    // info(s"setContentMD", s"fName: ${fName}") 
    getContent(fName).map {
      case Left(errMsg)   => setMainContent(clientviews.home.html.Error(getError(Error("err0125.setContentMD", fName))).toString)
      case Right(result)  => {
        val (content, author, pubDate) = markdown2html(result)
        setMainContent(s"""<article class="markdown-body">${content}</article>""")
        if (highlight) Highlight.initHighlighting
        setFooter(author, pubDate)
      }
    }
  }    

  def setContentFile(fName: String): Unit = {
    try { Ajax.get(fName).map(_.responseText).map { content => setMainContent(content) } }
    catch { case e: Throwable => { render("Error", getError(Error("err0126.fileNotAvailable", fName))) } }
  }


  /** markdown2html - convert markdown to html cgetHtmlFromMarkdown 
   *                        
   * @return triple (content, author, date) 
   */
  def markdown2html(mdContent: String):(String, String, String) = {
    import scala.util.matching.Regex
    
    // getYaml get everything line between --- and ---
    def getYaml(data: Array[String]): String = {
      var yBuffer  = new StringBuffer("")
      var endFound = false
      if (data(0) == "---") {
        for (i <- 1 until data.length if !endFound) {
          if (data(i) != "---") yBuffer.append(s"${data(i)}\n") else endFound = true
        }  
      }
      yBuffer.toString
    }
    
    // read js.Dynamic
    def getJsObject(o: js.Dynamic): String = if (!js.isUndefined(o) && o != null) o.toString else "" 
    
    val yData   = YAML.parse(getYaml(mdContent.split("\n").filter(_ != "")))
    val (author, pubDate) = if (!js.isUndefined(yData) && yData != null) {          
      (getJsObject(yData.author), getJsObject(yData.date))                     
    } else {
      ("", "")
    }

    val linkPat: Regex = """\(\/content\/[^\)]+\)""".r
    val converter = new Converter()  
    converter.setOption("metadata", true)
    converter.setOption("tables", true)   

    ( 
      converter.makeHtml {
        linkPat.replaceAllIn(mdContent, link => s"(javascript:Link.show('${link.toString.substring(1,link.toString.length-1)}'))")  
      }, 
      author, 
      pubDate
    )
  }

}  