package models.daos

import javax.inject.{ Inject, Singleton }
import play.api.db.slick.DatabaseConfigProvider
import play.db.NamedDatabase
import play.api.Logging
import slick.jdbc.JdbcProfile

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

import shared.model.License
import tourn.services.Crypto

class InvalidLicException(s: String, e: Exception) extends Exception(s: String, e: Exception)

/**
  * A repository for tournaments.
  *
  * @param dbConfigProvider The Play db config provider. Play will inject this
  */
@Singleton
class LicenseDAO @Inject()(
  @NamedDatabase("license") protected val dbConfigProvider: DatabaseConfigProvider
)(
  implicit ec: ExecutionContext
) extends Logging {

  // We want the JdbcProfile for this provider
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db       = dbConfig.db

  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig.profile.api._

  /*
   * LicenseTable defines all information needed
   * to handle reqested uses licenses
   * Please don't ask about the tag ... I do not see any usage
   * in the program. Think its internally necessary.
   * slick documentation: http://slick.typesafe.com/doc/3.0.0/queries.html#inserting
   */
  class LicenseTable(tag: Tag) extends Table[License](tag, "LICENSE") {
    def id           = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def uuid         = column[String]("uuid")
    def club         = column[String]("club")
    def orgDir       = column[String]("orgDir")
    def licStr       = column[String]("licStr")
    def email        = column[String]("email")
    def name         = column[String]("name")
    def address      = column[Option[String]]("address")
    def password     = column[String]("password")
    def reqTStamp    = column[String]("reqTStamp")
    def allowContact = column[Boolean]("allowContact")
    def fullVersion  = column[Boolean]("fullVersion")

    /*
     * The * method provides a default projec on that maps between columns in
     * the table and instances of our case class. Slick’s mapTo macro creates a
     * two-way mapping between the columns and thefields in License.
     * Example: def * = (id.?, club, licStr, email, name, reqTStamp, updAllowed) <> (License.tupled, License.unapply _)
     */

    def * = (id, uuid, club, orgDir, licStr, email, name, address, password, reqTStamp, allowContact, fullVersion) <> (License.tupled, License.unapply)
    
  }

  /*
   * definition of a TableQuery (a subtype of a more general Query)
   * extension methods i.g. result, filter, ...
   * The starting point for all queries on the license table.
   */
  private val licenses = TableQuery[LicenseTable]

  val insertQuery = licenses returning licenses.map(_.id) into ((license, id) => license.copy(id = id))

  /**
    * Create a license with the given name, ...
    *
    * This is an asynchronous operation, it will return a future of the created License,
    * which can be used to obtain the id for that License.
    */
  def createLicense(license: License): Future[License] = {
    val action = insertQuery += license
    db.run(action)
  }

  /**
   * insert a license
   */
  def insertLicense(license: License): Future[Option[Int]] = {
    val insertAction = licenses ++= Seq(license)
    db.run(insertAction)
  }


  /**
   * Delete a License with the given id
   *
   * This is an asynchronous operation, it will return a future (number of affected rows)
   */
  def delete(liId: Long): Future[Int] = {
    val action = licenses.filter(_.id === liId).delete
    db.run(action)
  }

  /**
   * insert a license
   */
  def update(lic: License): Future[Int] = {
    val action = ((for {
      l <- licenses if (l.id === lic.id)
    } yield (l)).result.headOption.flatMap {
      case Some(license) =>
        logger.info(s"LicenseDAO.update done")
        licenses.insertOrUpdate(
          License(license.id, lic.uuid, license.club, license.orgDir, license.licStr, lic.email, lic.name, lic.address, lic.password, license.reqTStamp, lic.allowContact, lic.fullVersion)
        )
      case None =>
        logger.info(s"LicenseDAO.update not found")
        // produces a DBIO[Int] for a constant value
        DBIO.successful(0)
    })
    db.run(action)
  }


  /**
    * List all the licenses in the database.
    */
  def list(): Future[Seq[License]] = db.run {
    licenses.result
  }

  /** findLicenseByLic returns Some[License] with licStr or None
   * 
   * @param lic license string
   */
  def findLicenseByLic(lic: String) = db.run {
    licenses.filter(_.licStr === lic).result.headOption
  }

  /** findLicenseByOrgDir returns Some[License] from club with this OrgDir or None
   * 
   * @param orgdir unique directory of club
   */
  def findLicenseByOrgDir(orgdir: String): Future[Option[License]] = db.run {
    licenses.filter(_.orgDir === orgdir).result.headOption
  }

  /** findLicenseById returns Some[License] or None
   * 
   * @param  id: license identificator
   * @return Option[License] 
   */
  def findLicenseById(id: Long) = db.run {
    licenses.filter(_.id === id).result.headOption
  }

  /** findLicenseByClub returns Some[License] or None
   * 
   * @param  club name of club
   * @return Option[License] 
   */
  def findLicenseByClub(club: String) = db.run {
    licenses.filter(_.club === club).result.headOption
  }

  
  /** findLicenseByEMail returns Some[License] or None
   * 
   * @param  email
   * @return Option[License] 
   */
  def findLicenseByEmail(email: String) = {
    val action = (for {
      lic <- licenses if (lic.email === email)
    } yield (lic)).result
    db.run(action).map(_.headOption)(ec)
  }


  /**
   * findLicenseByLicCode returns Some[License] with licStr or None
   */
  def findLicenseByLicCode(licCode: String): Future[Option[License]] =
    // generate action from query
    // then run action
    if (licCode.length < 20) {
      Future(None)(ec)
    } else {
      val forComprehensionAction = (for {
        lic <- licenses if lic.licStr like s"%$licCode%"
      } yield (lic)).result
      db.run(forComprehensionAction).map(_.headOption)(ec)
    }


  /** validateLicense
    *  @param  licCode - hashvalue of club
    *  @return true/false
    */
  def validateLicense(licCode: String): Future[Boolean] = {
    val invLic = new Exception("invalidLicense")
    if (licCode.length < 20) {
      Future(false)
      //throw new InvalidLicException("License to short", invLic)
    } else {
      val forComprehensionAction = (for {
        lic <- licenses if lic.licStr like s"%$licCode%"
      } yield (lic)).result
      db.run(forComprehensionAction)
        .map(
          res =>
            res match {
              case Seq(x) => true
              case _      => false
          }
        )
    }

  }

  /** getPathFromLicense
    *  @param  licCode - hashvalue of club
    *  @return cPath   - urified name of club (pathname)
    */
  def getPathFromLicense(licCode: String): Future[String] =
    if (licCode.length < 20) {
      Future("")
    } else {
      val forComprehensionAction = (for {
        lic <- licenses if lic.licStr like s"%$licCode%"
      } yield (lic)).result.headOption
      db.run(forComprehensionAction)
        .flatMap(
          res =>
            res match {
              case Some(x) => Future(x.orgDir)
              case _       => Future("")
          }
        )
    }
  

    /** getOrgDirFromClubId
    *  @param  clubId  - equals to license id
    *  @return cPath   - urified name of club (pathname)
    */
  def getOrgDirFromClubId(clubId: Long): Future[String] = { 
    val filterAction = licenses.filter(_.id === clubId).result
    db.run(filterAction).map(_.headOption).flatMap( res => res match {
      case Some(lic) => Future(lic.orgDir)
      case None      => Future("")
    })
  }
  
}