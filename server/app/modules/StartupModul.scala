package modules

import java.util.Base64
import java.nio.charset.StandardCharsets
import javax.inject._
import scala.concurrent._
import scala.io.Source

import com.google.inject.AbstractModule
import com.google.inject.name.Names
import play.api.{ Environment, Configuration, Logging }
import play.api.libs.json._
// import play.twirl.api.HtmlFormat
// import play.twirl.api.Html

import models.daos.{ LicenseDAO, UserDAO, TourneyDAO }
import models.{ User }      
import shared.model.{ License }

import tourn.services.{ TourneyService, TourneyServiceImpl, DateTime, Crypto }
import tourn.utils.Helper._
import tourn.utils._


@Singleton
class StartupRoutine @Inject()(
  env:     Environment, 
  config:  Configuration, 
  licDao:  LicenseDAO, 
  userDao: UserDAO
)(
  implicit exco:      ExecutionContext,
           tourDao:   TourneyDAO
) extends Logging {
  
  implicit val cfg = config
  implicit val gpeFormat    = Json.format[GameEntry]
  implicit val gpFormat     = Json.format[GamePlanEntry]

  // def getConfig(config: Configuration, entry: String, default:String="") = {
  //   if (config.has(entry)) config.get[String](entry) else default
  // }
  
  val licStr   = getConfig("local.license")
  val orgDir   = try licStr.split("/")(0) catch { case _: Throwable => ""}
  val password = getConfig("local.password", "0000")
  val club     = getConfig("local.club", "TTC Demo e.V." )
  val fullname = getConfig("local.fullname", "Mustermann, Max" )
  val (lastname, firstname) = fullname.split(",") match { case Array(a,b) => (a.trim, b.trim); case _ => (fullname,"") }  
  val email    = getConfig("local.fullname", "max.mustermann@demo.com")
  val date     = DateTime.getDateNow
  val homeDir  = getConfig("service.home")

  // set global local run mode lrm
  Crypto.rml        = (orgDir!="")
  Crypto.accessCtrl = getConfig("server.access.ctrl", true)
  
  if (Crypto.rml) { 
    licDao.findLicenseByOrgDir(orgDir).map{ f => f match {
      case None => {
        val pwHash = new String(Base64.getEncoder.encode(password.getBytes(StandardCharsets.UTF_8)))
        val license = License(0L, Crypto.getLocalUserUUID().toString, club, orgDir, licStr, email, fullname, None, pwHash, date, false, true )
        licDao.createLicense(license).map{ lic => logger.info(s"Startup local installation -> club:$club licId:${lic.id}") }   
      }
      case Some(l)    => { logger.info(s"Startup local installation -> club:$club licId:${l.id} reqTStamp:${l.reqTStamp}") }   
    }}
  } else {
    logger.info(s"Startup cloud instance -> date:$date home:${homeDir} accessCtrl:${Crypto.accessCtrl}")
  }

  val source: String = Source.fromFile(s"${env.rootPath}/conf/GamePlan.json").getLines().mkString
  GamePlan.init(Json.parse(source).as[List[GamePlanEntry]])
}


class StartupModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[StartupRoutine]).asEagerSingleton();
  }
}