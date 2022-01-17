package utils.auth

import javax.inject.Inject
import com.mohiva.play.silhouette.api.actions.UnsecuredErrorHandler
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc.RequestHeader
import play.api.mvc.Results._

import scala.concurrent.Future

/**
  * Custom unsecured error handler.
  */
class CustomUnsecuredErrorHandler @Inject()(val messagesApi: MessagesApi)
  extends UnsecuredErrorHandler {

  /**
    * Called when a user is authenticated but not authorized.
    *
    * As defined by RFC 2616, the status code of the response should be 403 Forbidden.
    *
    * @param request The request header.
    * @return The result to send to the client.
    */
  override def onNotAuthorized(implicit request: RequestHeader) =
    Future.successful(
      Redirect(controllers.routes.HomeCtrl.start("HomeMain", "ErrorCode", "err0085.access.insufficientRights"))
    )
}
