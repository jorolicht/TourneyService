import play.api.http.HttpErrorHandler
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent._
import javax.inject.Singleton

import play.api.i18n.Lang
import play.api.i18n.Langs
import play.api.i18n.MessagesApi
import play.api.Logging


import javax.inject._
import views.html._

import shared.utils._

@Singleton
class ErrorHandler@Inject() (langs: Langs, messagesApi: MessagesApi) 
extends HttpErrorHandler with Logging {

  def onClientError(request: RequestHeader, statusCode: Int, message: String) = {
    val lang: Lang = langs.availables.head

    val errMsg: String = messagesApi("app.error")(lang)
    val errTip: String = messagesApi("app.error.tip")(lang)
    val errLang: String = messagesApi("app.lang")(lang)

    Future.successful(Status(statusCode)(views.html.component.ErrorPage.render(errMsg, errTip, errLang)))
  }

  def onServerError(request: RequestHeader, exception: Throwable) = {
    val lang: Lang = langs.availables.head

    val reason = exception.getMessage()
    logger.error(reason.toString)
    val errMsg: String = messagesApi("app.srv.error")(lang)
    val errTip: String = messagesApi("app.srv.error.tip")(lang)
    val errLang: String = messagesApi("app.lang")(lang)

    Future.successful(
      BadRequest(Error("err0046.internal.server", reason).encode)
      //InternalServerError(views.html.component.ErrorPage.render(reason, errTip, errLang))
    )
  }
}