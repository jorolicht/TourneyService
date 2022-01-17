package controllers

import javax.inject._

// imports for akka
import akka.actor._
import akka.dispatch._
import akka.stream._
import akka.stream.scaladsl._
import controllers.ActorRefManager._

import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.impl.providers._
import models.services.UserService
import utils.auth.DefaultEnv

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent._
import scala.util._
import sys.process._

import play.api.{ Environment, Configuration, Logging }
import play.api.i18n.{ I18nSupport, Messages, Langs, Lang }
import play.api.inject.ApplicationLifecycle
import play.api.http.ContentTypes
import play.api.libs.mailer._
import play.api.libs.EventSource
import play.api.libs.json.{JsValue, Json }
import play.api.cache._
import play.api.mvc._

import play.Application
import play.twirl.api.HtmlFormat
import play.twirl.api.Html

import play.api.mvc.MultipartFormData.FilePart
import play.core.parsers.Multipart.FileInfo

import models.daos.{ TourneyDAO, LicenseDAO, UserDAO }
import models.{ User, LocalUser }
import shared.model.{ License, LicRequest, Sidebar, SidebarEntry, SidebarConfig }
import shared.utils._
import tourn.services.{ Crypto }


/**
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class AuthenticateCtrl @Inject()(
    loginCache:             SyncCacheApi, 
    env:                    Environment,
    cfg:                    Configuration,
    tourneyDao:             TourneyDAO,
    licDao:                 LicenseDAO,
    usrDao:                 UserDAO,
    cc:                     ControllerComponents,
    mailer:                 MailerClient,
    silhouette:             Silhouette[DefaultEnv],
    userService:            UserService,
    authInfoRepository:     AuthInfoRepository,
    socialProviderRegistry: SocialProviderRegistry,
    langs:                  Langs
)(
    implicit
    ec:                ExecutionContext, 
    assetsFinder:      AssetsFinder
) extends AbstractController(cc) 
  with I18nSupport with Logging {
   
  val lS = for (lng <- langs.availables) yield (lng.toLocale.getLanguage, lng.toLocale.getDisplayLanguage(lng.toLocale).capitalize)

  
  // --------------------------------------------------------------------------
  //
  // signOut/authenticate with silhouette social provide google
  //
  // --------------------------------------------------------------------------  

  /**
    * Authenticates a user against a social provider.
    *
    * @param provider The ID of the provider to authenticate against.
    * @return The result to display.
    */
  def authGoogle = Action.async { implicit request: Request[AnyContent] =>
    val provider = "google"
    
    (socialProviderRegistry.get[SocialProvider](provider) match {
      case Some(p: SocialProvider with CommonSocialProfileBuilder) =>
        p.authenticate().flatMap {
          case Left(result) => Future.successful(result)
          case Right(authInfo) => for {
            profile       <-  p.retrieveProfile(authInfo)
            user          <-  userService.save(profile)
            authInfo      <-  authInfoRepository.save(profile.loginInfo, authInfo)
            authenticator <-  silhouette.env.authenticatorService.create(profile.loginInfo)
            value         <-  silhouette.env.authenticatorService.init(authenticator)
            lic           <-  licDao.findLicenseByEmail(user.email.map(_.toString).getOrElse("unknown"))

            result        <-  lic match {
                                case Some(license) => {
                                  if (!(license.uuid==user.userID.toString)) licDao.update(license.copy(uuid=user.userID.toString))
                                  Future(
                                    Redirect(
                                     routes.HomeCtrl.start("HomeMain", "WelcomeOrganizer", "")
                                    ).withCookies(Cookie("TuSe", Crypto.genSessionCookie(Some(license), user.admin)))  
                                  )
                                }
                                case None          => {
                                  val email = user.email.map(_.toString).getOrElse("unknown")
                                  Future(
                                    Redirect(
                                      routes.HomeCtrl.start("HomeMain", "Error", Messages("err0131.login.emailnotfound", email) )
                                    ).discardingCookies(DiscardingCookie("TuSe")) 
                                  )
                                }
                              }
                           

          } yield {
            logger.info(s"authenticate: User ${user.email} ClubId: ${user.orgId}")
            silhouette.env.eventBus.publish(LoginEvent(user, request))
            result
          } 
        }     
      case _ => Future.failed(new ProviderException(s"Cannot authenticate with unexpected social provider $provider"))
    }).recover {
      case e: ProviderException =>
        logger.error("Unexpected provider error", e)
        Redirect(routes.HomeCtrl.start("HomeMain", "ErrorCode", "err0087.access.UnexpectedProviderError"))
          .flashing("error" -> Messages("err0088.access.couldNotAuthenticate"))
    }
  }
  
  /** authLogout
    * 
    * param redirect
    */
  def authLogout(redirect: Boolean=true) = silhouette.UserAwareAction.async { implicit request =>
    logger.info(s"logout(redirect=${redirect})")
    val result = if (redirect) {
      Redirect(routes.HomeCtrl.start("HomeLogin", "Logout", "")).discardingCookies(DiscardingCookie("TuSe"))
    } else {
      Ok(Return(true).encode).discardingCookies(DiscardingCookie("TuSe"))  
    }   
    request.identity match {
      case Some(identity) => {
        //logger.info(s"logout(sihouette")
        request.authenticator match {
          case Some(authenticator) => {
            silhouette.env.eventBus.publish(LogoutEvent(identity, request))
            silhouette.env.authenticatorService.discard(authenticator, result)
          }
          case None                => Future(result)
        }
      }  
      case None  => Future(result)
    }
  }
  

  /** reset
    * 
    * @param params
    */
  def authReset(params: String="") = silhouette.UserAwareAction.async { implicit request =>
    import shared.utils.Routines._
    import org.mindrot.jbcrypt.BCrypt
    import org.apache.commons.mail.EmailException
    import views.html.component.EmailPassword
    import java.util.Base64
    import java.nio.charset.StandardCharsets

    logger.info(s"reset(params=${params})")

    val msgs: Messages  = messagesApi.preferred(request)
    val param   = Crypto.encParam(params)
    val email   = param("email")

    // search license with email
    val lic = licDao.findLicenseByEmail(email)

    lic.flatMap { _ match {
      case Some(license) => {
        /* Password is base64 encoded and hashed with BCrypt stored in the license database.
         * Random password only contains characters: A-Za-z0-9
         */ 
        val randPW   = randomString()
        val base64PW = new String(Base64.getEncoder.encode(randPW.getBytes(StandardCharsets.UTF_8)))
        val hashPW   = BCrypt.hashpw(base64PW, BCrypt.gensalt(11))

        logger.info(s"authReset Id: ${license.id} Club: ${license.club}")
        for { cnt <- licDao.update(license.copy(password=hashPW)) } yield {
          cnt match {
            case 1 => {
              val name = license.name.split(",").map(_.trim).reverse.mkString(" ") 
              val email = license.email
              val lang = msgs("app.lang")

              val mail = Email(
                msgs("email.password.subject"),
                msgs("email.password.from"),
                Seq(s"$name <$email>"),
                bodyText = Some(EmailPassword(name, randPW, lang, false).toString),
                bodyHtml = Some(EmailPassword(name, randPW, lang, true).toString)
              )
              try   { mailer.send(mail); Ok(Return(true).encode) }  
              catch { 
                case e: EmailException => BadRequest( Error("err0092.email.password.send", e.getMessage()).encode )
                case _: Throwable      => BadRequest( Error("err0093.email.user.notfound", email).encode )
              }
           }
           case _                      => BadRequest( Error("err0093.email.user.notfound", email).encode )
          }
        }
      }  
      case None    => Future( BadRequest(Error("err0093.email.user.notfound", email).encode) ) 
    }}
  }


  /**
   *  authExternal - authentication for external programs
   */
  def authExternal = Action.async { implicit request =>
    import play.filters.csrf.CSRF
    import upickle.default._
    
    val lic = licDao.findLicenseByLicCode(Crypto.getLicenseCodeFromBasicAuth(request.headers.get("Authorization")))
    
    val token = CSRF.getToken(request)
    logger.info(s"authExternal ${token.mkString}")
    
    lic.flatMap { 
      license => license match {
        case Some(l) => {
          logger.info(s"authExternal ${l.id} ${l.club}")
          Future( Ok(write( (l.id, l.club) )).withCookies(Cookie("TuSe", Crypto.genSessionCookie(Some(l), false))) )  
        }
        case None    => Future(BadRequest(Error("error.login.noLic").encode).discardingCookies(DiscardingCookie("TuSe"))) 
      } 
    } 
  }


  /** authBasic - basic authentication (with email/password)
   * 
   */
  def authBasic(params: String="") = Action.async { implicit request =>
    import scala.concurrent.duration._
    import org.mindrot.jbcrypt.BCrypt
    // logger.info(s"authBasic(${params})")

    val param   = Crypto.encParam(params)
    val email   = param("email")
    val licCode = param("licCode")

    // search license either with email or with licCode
    val lic = if (email.contains("@")) licDao.findLicenseByEmail(email) else licDao.findLicenseByLicCode(licCode)
    
    lic.flatMap { _ match {
      case Some(license) => {
        val loginCnt = loginCache.get[Int](license.orgDir).getOrElse(0) + 1
        val candidate = Crypto.getPasswordFromBasicAuth(request.headers.get("Authorization"))
        logger.info(s"authBasic Header: ${request.headers.get("Authorization")} Password: ${candidate}")

        val hash = BCrypt.hashpw(candidate, BCrypt.gensalt(11))
        logger.info(s"authBasic hash/license DB: ${hash}/${license.password}")

        if (loginCnt <= 3 && BCrypt.checkpw(candidate, license.password)) {
          Future( 
            Ok(Crypto.genSessionBase64(Some(license), false)).withCookies(Cookie("TuSe", Crypto.genSessionCookie(Some(license), false)) ) 
          )  
        } else {
          // increase and save failed login counter
          loginCache.set(license.orgDir, loginCnt ,1.hour)
          logger.warn(s"authBasic wrong password Id: ${license.id} Club: ${license.club} count: ${loginCnt}")
          if (loginCnt<3) Future(BadRequest(Error("err0005.login.password", loginCnt.toString).encode).discardingCookies(DiscardingCookie("TuSe")))
          else            Future(BadRequest(Error("err0004.login.blocked").encode).discardingCookies(DiscardingCookie("TuSe")))
        }
      }
      case None    => {
        logger.warn(s"authBasic license or email ${licCode} / ${email} wrong")
        Future(BadRequest(Error("err0006.login.licEmail", email, licCode).encode).discardingCookies(DiscardingCookie("TuSe"))) 
      }  
    }}
  }


  /** authChange - change password
   * 
   */
  def authChange(params: String="") = Action.async { implicit request: Request[AnyContent] =>
    import org.mindrot.jbcrypt.BCrypt

    val ctx       = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val data      = request.body.asText.getOrElse("")
    val paramMap  = Crypto.encParam(params)
    val email     = Crypto.getParam(paramMap, "email")
    val pw2check  = Crypto.getParam(paramMap, "pw2check")

    // basic validation or request
    if (email=="" | pw2check=="") logger.warn(s"authChange -> email: ${email} or pw2check: XXXXX wrong")
    
    licDao.findLicenseById(ctx.orgId).flatMap { _ match {
      case Some(license) => {
        val pwCheck    = BCrypt.checkpw(pw2check, license.password)
        val emailCheck = (email == license.email)

        // verify that old password and given email match
        if (pwCheck & emailCheck) {
          // update password, post data contain base64 encoded new password
          val hashPW = BCrypt.hashpw(data, BCrypt.gensalt(11))
          // update license with new password hash
          for { cnt <- licDao.update(license.copy(password=hashPW)) } yield {
            if (cnt==1) Ok(Return(true).encode) else BadRequest(Error("err0095.login.licenseupdate").encode) 
          }
        } else {
          Future( BadRequest(Error("err0096.login.emailpassword").encode) ) 
        }
      }
      case None    => Future( BadRequest(Error("err0097.login.invalid").encode) ) 
    }}
  }



  /** authUpdate - change name, email or address
   * 
   */
  def authUpdate(params: String="") = Action.async { implicit request: Request[AnyContent] =>
    import org.mindrot.jbcrypt.BCrypt

    val ctx       = Crypto.getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val data      = request.body.asText.getOrElse("")
    val paramMap  = Crypto.encParam(params)
    val newEmail  = Crypto.getParam(paramMap, "email")
    val newName   = Crypto.getParam(paramMap, "name")
    val pw2check  = Crypto.getParam(paramMap, "pw2check")

    // basic validation or request
    if (newEmail=="" | pw2check=="" | newName=="") logger.warn(s"authUpdate -> email: ${newEmail} name: ${newName} or pw2check: XXXXX wrong")
    
    licDao.findLicenseById(ctx.orgId).flatMap { _ match {
      case Some(license) => {
        val pwCheck = BCrypt.checkpw(pw2check, license.password)

        // verify that password is ok
        if (pwCheck) {
          // update license with new password hash
          for { cnt <- licDao.update(license.copy(email=newEmail, name=newName, address=Some(data))) } yield {
            if (cnt==1) Ok(Return(true).encode) else BadRequest(Error("err0095.login.licenseupdate").encode) 
          }
        } else {
          Future( BadRequest(Error("err0096.login.emailpassword").encode) ) 
        }
      }
      case None    => Future( BadRequest(Error("err0097.login.invalid").encode) ) 
    }}
  }

}