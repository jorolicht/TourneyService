package tourn.services

import play.api.libs.mailer._
import javax.inject.Inject

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._


object DateTime {
  import java.util.Calendar
  import java.util.Date
  import java.util.Locale
  import java.text.SimpleDateFormat

  //val DATE_FORMAT = "EEE, MMM dd, yyyy h:mm a"
  val DATE_FORMAT = "yyyy-MM-dd_HH:mm"

  def getDateAsString(d: Date): String = {
    val dateFormat = new SimpleDateFormat(DATE_FORMAT)
    dateFormat.format(d)
  }

  def convertStringToDate(s: String): Date = {
    val dateFormat = new SimpleDateFormat(DATE_FORMAT)
    dateFormat.parse(s)
  }

  def getDateTimeNow: String = {
    val now = Calendar.getInstance().getTime()
    getDateAsString(now)
  }
  

  def getDateNow: String = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val now = Calendar.getInstance().getTime()
    dateFormat.format(now)
  }
  

  // fmtIntToDateString formats an Int YYYYMMDD (e.g. 20190601) to
  // an String according to lang (not Locale ...)
  def fmtIntToDateString(time: Int, lang: String): String = {
    val dateFmtUS = "MM-dd-yyyy"
    val dateFmtDE = "d. MMMM yyyy"

    val df = new SimpleDateFormat("yyyyMMdd")
    val d  = df.parse(time.toString)
    lang match {
      case "de" => new SimpleDateFormat(dateFmtDE, Locale.GERMAN).format(d)
      case "en" => new SimpleDateFormat(dateFmtUS, Locale.US).format(d)
      case _    => new SimpleDateFormat(dateFmtUS, Locale.US).format(d)
    }
  }
  
  // fmtCTTToTimeString formats an Int YYYY-MM-DD hh:mm (e.g. 2019-06-01 16:00) to
  // an String according to lang (not Locale ...)
  def fmtCTTToTimeString(dateString: String, lang: String): String = {
    val dateFmtUS = "HH:mm aaa"
    val dateFmtDE = "HH':'mm"
    val inpFmt    = "yyyy-MM-dd HH:mm"

    try {
      val df = new SimpleDateFormat(inpFmt)
      val d  = df.parse(dateString)
      
      lang match {
        case "de" => new SimpleDateFormat(dateFmtDE, Locale.GERMAN).format(d)
        case "en" => new SimpleDateFormat(dateFmtUS, Locale.US).format(d)
        case _    => new SimpleDateFormat(dateFmtUS, Locale.US).format(d)
      }      
    } catch { case _: Throwable => "" }
  }  
  
  // fmtCTTToDateString formats an Int YYYY-MM-DD hh:mm (e.g. 2019-06-01 16:00) to
  // an String according to lang (not Locale ...)
  def fmtCTTToDateString(dateString: String, lang: String): String = {
    val dateFmtUS = "yyyy-MM-dd"
    val dateFmtDE = "dd.MM.yyyy"
    val inpFmt    = "yyyy-MM-dd HH:mm"

    try {
      val df = new SimpleDateFormat(inpFmt)
      val d  = df.parse(dateString)
      
      lang match {
        case "de" => new SimpleDateFormat(dateFmtDE, Locale.GERMAN).format(d)
        case "en" => new SimpleDateFormat(dateFmtUS, Locale.US).format(d)
        case _    => new SimpleDateFormat(dateFmtUS, Locale.US).format(d)
      }      
    } catch { case _: Throwable => dateString }
  }  
  
}









