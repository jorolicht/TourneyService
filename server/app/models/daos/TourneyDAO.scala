package models.daos

import javax.inject.{ Inject, Singleton }
import play.api.db.slick.DatabaseConfigProvider
import play.api.db.slick.HasDatabaseConfigProvider

import play.api.Logging
import play.db.NamedDatabase
import slick.jdbc.JdbcProfile

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }
import shared.model.TournBase

/**
  * A repository for tournament service
  *
  * @param dbConfigProvider The Play db config provider. Play will inject this
  */
@Singleton
class TourneyDAO @Inject()(
  @NamedDatabase("tourneyapp") protected val dbConfigProvider: DatabaseConfigProvider
)(implicit ec: ExecutionContext)
  extends HasDatabaseConfigProvider[JdbcProfile] with Logging {

  // We want the JdbcProfile for this provider
  // This could be replaced by Mixin DAOSlick with HasDatabaseConfigProvider[JdbcProfile] !!
  //val dbConfig = dbConfigProvider.get[JdbcProfile]
  //val db = dbConfig.db


  // These imports are important, the first one brings db into scope, which will let you do the actual db operations.
  // The second one brings the Slick DSL into scope, which lets you define the table and other queries.
  import dbConfig.profile.api._

  /*
   * EventTable defines the names and types of six columns
   * and any constraints on them at the database
   * level. id is a column of Long values, which is also
   * an auto-incremening prmary key.
   * Please don't ask about the tag ... I do not see any usage
   * in the program. Think its internally necessary.
   * slick documentation: http://slick.typesafe.com/doc/3.0.0/queries.html#inserting
   * TABLE NAME EVENT
   */
  
  class TourneyTable(tag: Tag) extends Table[TournBase](tag, "TOURNEY") {
    def id        = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def typ       = column[Int]("TYP")
    def name      = column[String]("NAME")
    def organizer = column[String]("ORGANIZER")
    def orgDir    = column[String]("ORGDIR")
    def startDate = column[Int]("STARTDATE")
    def endDate   = column[Int]("ENDDATE")
    def ident     = column[String]("IDENT")
    def contact   = column[String]("CONTACT")
    def address   = column[String]("ADDRESS")
    def privat    = column[Boolean]("PRIVAT")

    /*
     * The * method provides a default projec on that maps between columns in
     * the table and instances of our case class. Slick’s mapTo macro creates a
     * two-way mapping be- tween the three columns and the three fields in Message.
     */
    def * = (name, organizer, orgDir, startDate, endDate, ident, typ, privat, contact, address, id) <> (TournBase.tupled, TournBase.unapply)
  }  
  

  /*
   * definition of a eventQuery (a subtype of a more general Query)
   * extension methods i.g. result, filter, ...
   * The starting point for all queries on the tourney table.
   */
  val tourneys = TableQuery[TourneyTable] //now not yet in DBTableDefinitions
  val insertQuery = tourneys returning tourneys.map(_.id) into ((tourney, id) => tourney.copy(id = id))

  def getTourney(id: Long): DBIO[TournBase] = tourneys.filter(_.id === id).result.head

  /**
    * Create a event with the given name, path, ...
    *
    * This is an asynchronous operation, it will return a future of the created tourney,
    * which can be used to obtain the id for that tourney.
    */
  def insert(tb: TournBase): Future[Int] = {
    val action = insertQuery += tb
    db.run(action).map { t =>
      t.id.toInt
    }
  }

  /**
    * Create a Tourney with the given name, path, ...
    *
    * This is an asynchronous operation, it will return a future of the created event,
    * which can be used to obtain the id for that event.
    */
  def insertIfNotExist(tony: TournBase): Future[TournBase] = {

    // tony
    //val action = (
    // Verknüpfung mit 2 Filter Optionen nicht möglich
    //ERROR: tourneys.filter(_.path===tony.path && _.startDate===tony.startDate).result.headOption.flatMap {
    //OK:    tourneys.filter(_.path===tony.path).result.headOption.flatMap {

    val action = ((for {
      // ERROR: === is not a method of plain String / Int (lifted values)
      // t <- tourneys if (tony.path === t.path) && (tony.startDate === t.startDate)
      t <- tourneys if (t.orgDir === tony.orgDir) && (t.startDate === tony.startDate)
    } yield (t)).result.headOption.flatMap {

      case Some(tour) =>
        logger.info(s"ServiceDAO.insertIfNot elem found")
        DBIO.successful(tour)
      case None =>
        logger.info(s"ServiceDAO.insertIfNot insert elem")

        (tourneys returning tourneys.map(_.id)
        into ((tour, id) => tour.copy(id = id))) += TournBase(
          tony.name, tony.organizer, tony.orgDir, tony.startDate, tony.endDate, tony.ident, tony.typ, tony.privat, tony.contact, tony.address
        )
    }).transactionally
    db.run(action)
  }

  /**
   * Update a tourney with the given name, path, ...
   *
   * This is an asynchronous operation, it will return a future of the created tourney,
   * which can be used to obtain the id for that tourney.
   */
  def updateIfExist(tony: TournBase): Future[Int] = {

    /* OK, at least compile, update only for name ....
    val q = for {
      t <- tourneys if (t.path === tony.path) && (t.startDate === tony.startDate)
    } yield t.name
    val updateAction = q.update(tony.name)
    db.run(updateAction)
     */
    val action = ((for {
      t <- tourneys if (t.orgDir === tony.orgDir) && (t.startDate === tony.startDate)
    } yield (t)).result.headOption.flatMap {

      case Some(tour) =>
        logger.info(s"ServiceDAO.updateEventIfExist done")
        tourneys.insertOrUpdate(
          TournBase(tony.name, tony.organizer, tony.orgDir, tony.startDate, tony.endDate, tony.ident, tony.typ, tony.privat, tony.contact, tony.address, tour.id)
        )

      case None =>
        logger.info(s"ServiceDAO.updateEventIfExist not found")
        // produces a DBIO[Int] for a constant value
        DBIO.successful(0)
    })
    db.run(action)
  }

  /**
   * Create or update a Tourney with the given name, path, ...
   * This is an asynchronous operation, it will return a future of the created tourney,
   * which can be used to obtain the id for that tourney.
  */
  def insertOrUpdate(tony: TournBase): Future[TournBase] = {
    val action = (for {
      existing <- tourneys.filter(_.orgDir===tony.orgDir).filter(_.startDate===tony.startDate).result.headOption
      row      = existing.map(_.copy(name=tony.name, organizer=tony.organizer, endDate=tony.endDate, privat=tony.privat, contact=tony.contact, address=tony.address)) getOrElse  tony
      result   <- if (row.id > 0) {
                    //logger.info(s"insertOrUpdate (update)")
                    tourneys.insertOrUpdate(row).flatMap {_ => getTourney(row.id)}.transactionally
                  } else {
                    //logger.info(s"insertOrUpdate (insert)")
                    val insertAction = tourneys.returning(tourneys.map(_.id)) += row
                    val finalAction = insertAction.flatMap( id => getTourney(id)).transactionally //transactionally is important
                    finalAction
                  } 
    } yield result).transactionally
    db.run(action)
  }


  /**
   * Delete a tourney with the given name, path, ...
   *
   * This is an asynchronous operation, it will return a future of the created tourney,
   * which can be used to obtain the id for that tourney.
   */
  def delete(tony: TournBase): Future[Int] = {

    val query = for {
      ev <- tourneys if (ev.orgDir === tony.orgDir) && (ev.startDate === tony.startDate)
    } yield (ev)
    val action = query.delete
    db.run(action)
  }
  
  /**
    * Delete a Tourney with the given id
    *
    * This is an asynchronous operation, it will return a future of the created event,
    * which can be used to obtain the id for that event.
    */
  def deleteById(evtId: Long): Future[Int] = {

    val query = for {
      ev <- tourneys if (ev.id === evtId)
    } yield (ev)
    val action = query.delete
    db.run(action)
  }
  
  
  /**
    * Delete a Event with the given id
    *
    * This is an asynchronous operation, it will return a future of the created event,
    * which can be used to obtain the id for that event.
    */
  def deleteByIdOrgDir(toId: Long, odi: String): Future[Int] = {

    val query = for {
      tr <- tourneys if (tr.id === toId) && (tr.orgDir === odi)
    } yield (tr)
    val action = query.delete
    db.run(action)
  }
  

  /**
    * List all the events in the database.
    */
  def list(): Future[Seq[TournBase]] = {
    // generate action, then run action
    val resultAction = tourneys.result
    db.run(resultAction)
  }

  /**
    * findByPathDate returns Sequence of events
    */
  def findByPathDate(path: String, date: Int): Future[Option[TournBase]] = {

    /*
     * A Query can be converted into an Action by calling its result method.
     * The Action can then be executed directly in a streaming or fully materialized way,
     * or composed further with other Actions
     */

    // generate action from query
    // then run action
    if (path != "" && date != 0) {
      val forComprehensionAction = (for {
        tony <- tourneys if (tony.orgDir === path) && (tony.startDate === date)
      } yield (tony)).result.headOption
      db.run(forComprehensionAction)
    } else {    
      Future(None)
    }
  }
  
  /**
    * findByPathId returns Sequence of tourneys
    */
  def findByPathId(path: String, toId: Long): Future[Option[TournBase]] = {

    /*
     * A Query can be converted into an Action by calling its result method.
     * The Action can then be executed directly in a streaming or fully materialized way,
     * or composed further with other Actions
     */

    // generate action from query
    // then run action
    val forComprehensionAction = (for {
      tony <- tourneys if (tony.orgDir === path) && (tony.id === toId)
    } yield (tony)).result.headOption

    db.run(forComprehensionAction)
  } 

    /**
    * findById returns a TournBase entry
    */
  def findById(path: String, toId: Long): Future[Option[TournBase]] = {
    // generate action from query
    // then run action
    val forComprehensionAction = (for {
      tony <- tourneys if (tony.id === toId)
    } yield (tony)).result.headOption

    db.run(forComprehensionAction)
  }  

  
  /**
    * findByStringTypDate returns Sequence of events
    */
  def findByStringTypDate(search: String, cTyp: Int, cYear: Int): Future[Seq[TournBase]] = {

    /*
     * A Query can be converted into an Action by calling its result method.
     * The Action can then be executed directly in a streaming or fully materialized way,
     * or composed further with other Actions
     */

    val tourneys1 = if (cYear > 0) tourneys.filter(_.startDate/10000 === cYear) else tourneys
    val tourneys2 = if (cTyp != 0) tourneys1.filter(_.typ === cTyp) else tourneys1
    
    // generate action from query
    // then run action
    val forComprehensionAction = (for {
      tony  <- tourneys2 if (tony.name like s"%$search%") || (tony.organizer like s"%$search%")
    } yield { tony }).result
    db.run(forComprehensionAction)
  }


  /**
    * findBySearchString returns Sequence of events
    */
  def findByString(search: String): Future[Seq[TournBase]] = {

    /*
     * A Query can be converted into an Action by calling its result method.
     * The Action can then be executed directly in a streaming or fully materialized way,
     * or composed further with other Actions
     */

    // generate action from query
    // then run action
    val forComprehensionAction = (for {
      tony <- tourneys if (tony.name like s"%$search%") || (tony.organizer like s"%$search%")
    } yield (tony)).result

    db.run(forComprehensionAction)
  }

  /**
    * findById returns Option of Event
    */
  def findById(id: Long): Future[Option[TournBase]] = {
    // filterQuery: tourneys.filter(_.id === id).take(1)
    logger.info(s"TourneyDAO.findById -> id: ${id}")
    val filterAction = tourneys.filter(_.id === id).take(1).result
    db.run(filterAction).map(_.headOption)
  }

  /**
    * findByOrgDir return Sequence of events
    */
  def findByOrgDir(ordi: String): Future[Seq[TournBase]] = {
    val filterAction = tourneys.filter(_.orgDir === ordi).result
    db.run(filterAction)
  }
  

}
