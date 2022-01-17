package tourn.services

import play.api.libs.mailer._
import play.api.i18n._
import javax.inject.Inject

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._

class MailerService @Inject()(mailerClient: MailerClient) {

  /**
    * Sending license string to user
    */
  //Seq(s"$name <$toEmail>"),
  def sendLicense(toEmail: String, name: String, licStr: String, lang: String): Boolean = {
    import views.html.component.EmailRegister
    import views.html.component.EmailRegisterPlain
    val intName = name.replaceAll(",", "")
    val email = Email(
      "Turnier-Programm Registration",
      "Turnier Service <info@turnier-service.org>",
      Seq(s"$intName <$toEmail>"),
      bodyText = Some(EmailRegisterPlain(licStr, lang).toString),
      bodyHtml = Some(EmailRegister(licStr, lang).toString)
    )
    
    try {
       mailerClient.send(email)
       true
    } catch {
       case _: Throwable => false
    }
  }

  /**
    * Sending log information home
    */
  def sendLog(toEmail: String, name: String, club: String, licStr: String, updAllow: Boolean, msg: String = "", logInfo: String = "successful"): Boolean = {
    val email = Email(
      s"User Registration Log: Name: $name Club: $toEmail",
      "<info@turnier-service.org>",
      Seq("Robert Lichtenegger <robert.lichtenegger@icloud.com>"),
      bodyText = Some(
        s"REGISTRATION: User $name (EMail: $toEmail Club: $club License: $licStr Updates: $updAllow with Message: $msg - $logInfo"
      ),
      bodyHtml = Some(s"""<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" 
                            "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
                          <html xmlns="http://www.w3.org/1999/xhtml">
    		                     <head>
                        		  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
                        		  <title>Log EMail Turnier-Service Registration</title>
                        		  <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                    		    </head>
                        		<body style="margin: 0; padding: 0;">
                        		  <h3>Registration: $logInfo</h3>
                        		  	  <ul>
                          		  <li>Name: $name</li>
                          		  <li>EMail: $toEmail</li>
                          		  <li>Verein: $club</li>
                          		  <li>LizenzString: $licStr</li>
                          		  <li>Updates: $updAllow</li>
                          		  <li>Datum: </li>    
                          		  <li>Nachricht: $msg</li>       
                        		  </ul>
                    		  </body>
                      """)
    )
    
    try {
       mailerClient.send(email)
       true
    } catch {
       case _: Throwable => false
    }
  }


}









