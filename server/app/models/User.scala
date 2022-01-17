package models

import java.util.UUID
import play.api.mvc.Cookie
import com.mohiva.play.silhouette.api.{ Identity, LoginInfo }

/**
  * The user object.
  *
  * @param userID The unique ID of the user.
  * @param loginInfo The linked login info.
  * @param firstName Maybe the first name of the authenticated user.
  * @param lastName Maybe the last name of the authenticated user.
  * @param fullName Maybe the full name of the authenticated user.
  * @param email Maybe the email of the authenticated provider.
  * @param avatarURL Maybe the avatar URL of the authenticated provider.
  * @param orgId the club/organization identification of the user
  */
case class User(userID   : UUID,
                loginInfo: LoginInfo,
                firstName: Option[String],
                lastName : Option[String],
                fullName : Option[String],
                email    : Option[String],
                avatarURL: Option[String],
                admin    : Boolean,
                orgId    : Long)
    extends Identity {

  /**
    * Tries to construct a name.
    *
    * @return Maybe a name.
    */
  def name = fullName.orElse {
    firstName -> lastName match {
      case (Some(f), Some(l)) => Some(f + " " + l)
      case (Some(f), None)    => Some(f)
      case (None, Some(l))    => Some(l)
      case _                  => None
    }
  }
}

case class LocalUser(userID: UUID, firstName: String, lastName: String, 
                     fullName: String, email: String, clubName: String, admin:Boolean, clubId: Long)
                     
object LocalUser {
  def stringify(userId: UUID, fiNa: String, laNa: String, fuNa: String, email: String, 
                organizer: String, admin:Boolean, orgId: Long) = {
    s"""{
    |	 "userId"    : "${userId.toString}",
    |	 "firstName" : "$fiNa", 
    |	 "lastName"  : "$laNa",
    |	 "fullName"  : "$fuNa", 
    |	 "email"     : "$email",
    |	 "organizer" : "$organizer",
    |  "admin"     :  $admin,
    |	 "orgId"     :  $orgId
    }""".stripMargin
  }

  def stringify(user: User, organizer: String) = {
    s"""{
    |	 "userId"    : "${user.userID.toString}",
    |	 "firstName" : "${user.firstName.getOrElse("")}", 
    |	 "lastName"  : "${user.lastName.getOrElse("")}",
    |	 "fullName"  : "${user.fullName.getOrElse("")}", 
    |	 "email"     : "${user.email.getOrElse("")}",
    |	 "organizer" : "$organizer",
    |  "admin"     :  ${user.admin},
    |	 "orgId"     :  ${user.orgId}
    }""".stripMargin
  } 
  
  def getFromCookie(cookie: Option[Cookie]) : LocalUser = {
    import tourn.services.Crypto
    LocalUser(Crypto.getLocalUserUUID(),"","","","","",false,999)
  }
  
  
}
