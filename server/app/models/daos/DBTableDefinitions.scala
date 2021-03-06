package models.daos

import com.mohiva.play.silhouette.api.LoginInfo
import slick.jdbc.JdbcProfile
import slick.lifted.ProvenShape.proveShapeOf

trait DBTableDefinitions {

  protected val driver: JdbcProfile
  import driver.api._

  case class DBUser(
      userID:    String,
      firstName: Option[String],
      lastName : Option[String],
      fullName : Option[String],
      email    : Option[String],
      avatarURL: Option[String],
      admin    : Boolean,
      orgId    : Long
  )

  class Users(tag: Tag) extends Table[DBUser](tag, "USER") {
    def id        = column[String]("userID", O.PrimaryKey)
    def firstName = column[Option[String]]("firstName")
    def lastName  = column[Option[String]]("lastName")
    def fullName  = column[Option[String]]("fullName")
    def email     = column[Option[String]]("email")
    def avatarURL = column[Option[String]]("avatarURL")
    def admin     = column[Boolean]("admin")
    def orgId     = column[Long]("orgId")
    def *         = (id, firstName, lastName, fullName, email, avatarURL, admin, orgId) <> (DBUser.tupled, DBUser.unapply)
  }

  case class DBLoginInfo(
      id: Option[Long],
      providerID: String,
      providerKey: String
  )

  class LoginInfos(tag: Tag) extends Table[DBLoginInfo](tag, "LOGININFO") {
    def id          = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def providerID  = column[String]("providerID")
    def providerKey = column[String]("providerKey")
    def *           = (id.?, providerID, providerKey) <> (DBLoginInfo.tupled, DBLoginInfo.unapply)
  }

  case class DBUserLoginInfo(
      userID: String,
      loginInfoId: Long
  )

  class UserLoginInfos(tag: Tag) extends Table[DBUserLoginInfo](tag, "USERLOGININFO") {
    def userID      = column[String]("userID")
    def loginInfoId = column[Long]("loginInfoId")
    def *           = (userID, loginInfoId) <> (DBUserLoginInfo.tupled, DBUserLoginInfo.unapply)
  }

  case class DBPasswordInfo(
      hasher: String,
      password: String,
      salt: Option[String],
      loginInfoId: Long
  )

  class PasswordInfos(tag: Tag) extends Table[DBPasswordInfo](tag, "PASSWORDINFO") {
    def hasher      = column[String]("hasher")
    def password    = column[String]("password")
    def salt        = column[Option[String]]("salt")
    def loginInfoId = column[Long]("loginInfoId")
    def *           = (hasher, password, salt, loginInfoId) <> (DBPasswordInfo.tupled, DBPasswordInfo.unapply)
  }

  case class DBOAuth1Info(
      id: Option[Long],
      token: String,
      secret: String,
      loginInfoId: Long
  )

  class OAuth1Infos(tag: Tag) extends Table[DBOAuth1Info](tag, "OAUTH1INFO") {
    def id          = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def token       = column[String]("token")
    def secret      = column[String]("secret")
    def loginInfoId = column[Long]("loginInfoId")
    def *           = (id.?, token, secret, loginInfoId) <> (DBOAuth1Info.tupled, DBOAuth1Info.unapply)
  }

  case class DBOAuth2Info(
      id: Option[Long],
      accessToken: String,
      tokenType: Option[String],
      expiresIn: Option[Int],
      refreshToken: Option[String],
      loginInfoId: Long
  )

  class OAuth2Infos(tag: Tag) extends Table[DBOAuth2Info](tag, "OAUTH2INFO") {
    def id           = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def accessToken  = column[String]("accesstoken")
    def tokenType    = column[Option[String]]("tokentype")
    def expiresIn    = column[Option[Int]]("expiresin")
    def refreshToken = column[Option[String]]("refreshtoken")
    def loginInfoId  = column[Long]("logininfoid")
    def * =
      (id.?, accessToken, tokenType, expiresIn, refreshToken, loginInfoId) <> (DBOAuth2Info.tupled, DBOAuth2Info.unapply)
  }

  case class DBOpenIDInfo(
      id: String,
      loginInfoId: Long
  )

  class OpenIDInfos(tag: Tag) extends Table[DBOpenIDInfo](tag, "OPENIDINFO") {
    def id          = column[String]("id", O.PrimaryKey)
    def loginInfoId = column[Long]("logininfoid")
    def *           = (id, loginInfoId) <> (DBOpenIDInfo.tupled, DBOpenIDInfo.unapply)
  }

  case class DBOpenIDAttribute(
      id: String,
      key: String,
      value: String
  )

  class OpenIDAttributes(tag: Tag) extends Table[DBOpenIDAttribute](tag, "OPENIDATTRIBUTES") {
    def id    = column[String]("id")
    def key   = column[String]("key")
    def value = column[String]("value")
    def *     = (id, key, value) <> (DBOpenIDAttribute.tupled, DBOpenIDAttribute.unapply)
  }


  
  // table query definitions
  val slickUsers            = TableQuery[Users]
  val slickLoginInfos       = TableQuery[LoginInfos]
  val slickUserLoginInfos   = TableQuery[UserLoginInfos]
  val slickPasswordInfos    = TableQuery[PasswordInfos]
  val slickOAuth1Infos      = TableQuery[OAuth1Infos]
  val slickOAuth2Infos      = TableQuery[OAuth2Infos]
  val slickOpenIDInfos      = TableQuery[OpenIDInfos]
  val slickOpenIDAttributes = TableQuery[OpenIDAttributes]


  // queries used in multiple places
  def loginInfoQuery(loginInfo: LoginInfo) =
    slickLoginInfos.filter(
      dbLoginInfo =>
        dbLoginInfo.providerID === loginInfo.providerID && dbLoginInfo.providerKey === loginInfo.providerKey
    )
}