package models.daos

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api.LoginInfo
import slick.dbio.DBIOAction
import play.api.db.slick.DatabaseConfigProvider
import play.db.NamedDatabase
import scala.concurrent.{ ExecutionContext, Future }
import models.User

/**
  * Give access to the user object using Slick
  */
class UserDAOImpl @Inject()(
    @NamedDatabase("tourneyapp") protected val dbConfigProvider: DatabaseConfigProvider
)(implicit ec: ExecutionContext)
    extends UserDAO
    with DAOSlick {

  //import driver.api._
  import dbConfig.profile.api._

  /**
    * Finds a user by its login info.
    *
    * @param loginInfo The login info of the user to find.
    * @return The found user or None if no user for the given login info could be found.
    */
  def find(loginInfo: LoginInfo) = {
    val userQuery = for {
      dbLoginInfo     <- loginInfoQuery(loginInfo)
      dbUserLoginInfo <- slickUserLoginInfos.filter(_.loginInfoId === dbLoginInfo.id)
      dbUser          <- slickUsers.filter(_.id === dbUserLoginInfo.userID)
    } yield dbUser
    db.run(userQuery.result.headOption).map { dbUserOption =>
      dbUserOption.map { user =>
        User(UUID.fromString(user.userID),
             loginInfo, user.firstName, user.lastName,
             user.fullName, user.email,
             user.avatarURL, user.admin, user.orgId)
      }
    }
  }

  /**
    * Finds a user by its user ID.
    *
    * @param userID The ID of the user to find.
    * @return The found user or None if no user for the given ID could be found.
    */
  def find(userID: UUID) = {
    val query = for {
      dbUser          <- slickUsers.filter(_.id === userID.toString)
      dbUserLoginInfo <- slickUserLoginInfos.filter(_.userID === dbUser.id)
      dbLoginInfo     <- slickLoginInfos.filter(_.id === dbUserLoginInfo.loginInfoId)
    } yield (dbUser, dbLoginInfo)
    db.run(query.result.headOption).map { resultOption =>
      resultOption.map {
        case (user, loginInfo) =>
          User(
            UUID.fromString(user.userID),
            LoginInfo(loginInfo.providerID, loginInfo.providerKey),
            user.firstName, user.lastName, user.fullName,
            user.email, user.avatarURL,
            user.admin, user.orgId
          )
      }
    }
  }


   /**
    * Find a (first) user by its orgId
    *
    * @param orgId of the user to find.
    * @return The found user or None if no user for the given Id could be found.
    */
  def findByOrgId(orgId: Long) = {
    val query = for {
      dbUser          <- slickUsers.filter(_.orgId === orgId)
      dbUserLoginInfo <- slickUserLoginInfos.filter(_.userID === dbUser.id)
      dbLoginInfo     <- slickLoginInfos.filter(_.id === dbUserLoginInfo.loginInfoId)
    } yield (dbUser, dbLoginInfo)
    db.run(query.result.headOption).map { resultOption =>
      resultOption.map {
        case (user, loginInfo) =>
          User(
            UUID.fromString(user.userID),
            LoginInfo(loginInfo.providerID, loginInfo.providerKey),
            user.firstName, user.lastName, user.fullName,
            user.email, user.avatarURL,
            user.admin, user.orgId
          )
      }
    }
  }



  /**
    * Saves a user.
    *
    * @param user The user to save.
    * @return The saved user.
    */
  def save(user: User) = {
    val dbUser = DBUser(user.userID.toString,
                        user.firstName, user.lastName,
                        user.fullName, user.email,
                        user.avatarURL, user.admin,
                        user.orgId)
    val dbLoginInfo = DBLoginInfo(None, user.loginInfo.providerID, user.loginInfo.providerKey)
    // We don't have the LoginInfo id so we try to get it first.
    // If there is no LoginInfo yet for this user we retrieve the id on insertion.
    val loginInfoAction = {
      val retrieveLoginInfo = slickLoginInfos
        .filter(
          info =>
            info.providerID === user.loginInfo.providerID &&
            info.providerKey === user.loginInfo.providerKey
        )
        .result
        .headOption
      val insertLoginInfo = slickLoginInfos
        .returning(slickLoginInfos.map(_.id))
        .into((info, id) => info.copy(id = Some(id))) += dbLoginInfo
      for {
        loginInfoOption <- retrieveLoginInfo
        loginInfo       <- loginInfoOption.map(DBIO.successful(_)).getOrElse(insertLoginInfo)
      } yield loginInfo
    }
    // combine database actions to be run sequentially
    val actions = (for {
      _         <- slickUsers.insertOrUpdate(dbUser)
      loginInfo <- loginInfoAction
      _         <- slickUserLoginInfos += DBUserLoginInfo(dbUser.userID, loginInfo.id.get)
    } yield ()).transactionally
    // run actions and return user afterwards
    db.run(actions).map(_ => user)
  }
}