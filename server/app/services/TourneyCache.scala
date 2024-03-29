package tourn.services

import java.util.UUID
import javax.inject.Inject
import java.time.Clock

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._

import play.api.{ Environment, Configuration, Logger }
import play.api.i18n.Messages

import shared.model._
import shared.model.Tourney._
import shared.utils.Routines._
import shared.utils.CSVConverter
import shared.utils._
import models.daos.TourneyDAO

/**
 * Database inteface for tourneys
 * 
 *  def getTrny(tse: TournSVCEnv, writeAccess: Boolean=false)
 *             (implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]]
 *  def get(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]]
 *  def add(trny: Tourney)(implicit ec: ExecutionContext, env: Environment, tonyDao: TourneyDAO): Future[Either[Error, Long]]
 *  def delete(toId: Long, orgDir: String)(implicit ec: ExecutionContext, tonyDao: TourneyDAO, cfg: Configuration): Future[Either[Error, Boolean]]
 *  def update(tb: TournBase)(implicit  ec: ExecutionContext, tonyDao: TourneyDAO, env: Environment): Future[Either[Error, Boolean]]
 *  def update(totx: TourneyTx)(implicit ec: ExecutionContext, env: Environment, tonyDao: TourneyDAO): Future[Either[Error, Long]]
 *  def update(ctt: CttTournament, orgDir: String, organizer: String, sDate: Int=0, eDate: Int=0,
 *             contact: String = "lastname·firstname·phone·email",
 *             address: String = "description·country·zip·city·street")
 *             (implicit ec: ExecutionContext, env: Environment, tonyDao: TourneyDAO, msgs : Messages): Future[Either[Error, Long]]
 *  def load(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]]
 *  def save(toId: Long)(implicit ec: ExecutionContext, env: Environment): Either[Error, Boolean]
 *  def clean()(implicit  ec: ExecutionContext, env: Environment): Unit
 * 
 */

// Tourney Input Output (Information Cache)
object TIO {
  import upickle.default._
  import java.nio.file.{Paths, Files, StandardCopyOption }
  import java.nio.charset.StandardCharsets

  import scala.util.{Try, Success, Failure}
  import scala.concurrent._
  import scala.collection.mutable.HashMap
  import play.api.libs.json.Json
  
  val logger:   Logger = Logger(this.getClass())
  val clock:    Clock  = Clock.systemUTC()
  val tourney:  HashMap[Long, Tourney] = HashMap()

  def mapDefault[X,Y](hm: HashMap[X,Y], key: X, default: Y) = if (!hm.isDefinedAt(key)) default else hm(key)

  def moveFile(source: String, destination: String): Either[Error, Boolean] = {
    try {
      val path = Files.move(Paths.get(source), Paths.get(destination), StandardCopyOption.REPLACE_EXISTING)
      Right(true)
    } catch { case _: Throwable => 
        logger.error(s"failed to move file from ${source} to ${destination}")
        Left(Error("err0149.cache.moveFile", source, "", "moveFile"))
    }
  }


  /** getTrny get either a tourney or an error 
    * 
    * @param tse     tourney service environment
    * @param trCmd   trigger command
    * @param ec
    * @param tonyDao
    * @return
    */
  def getTrny(tse: TournSVCEnv, writeAccess:Boolean=false)
          (implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]] = 
  {
    def checkAccessWrite(trny: Tourney, orgDir: String, writeAccess: Boolean): Either[Error, Tourney] = {
      if (Crypto.checkAccess(trny.orgDir, orgDir, writeAccess)) {
        if (writeAccess) {
          val timeNow = clock.millis()
          trny.writeTime  = timeNow
          logger.info(s"getTrny(${trny.id}): write access at ${timeNow}")
        } else {
          trny.accessTime  = clock.millis()
        }  
        Right(trny)
      } else {
        Left(Error("err0013.trny.insufficient.rights"))
      }
    }
    
    if (tourney.isDefinedAt(tse.toId)) { 
      // use cached values
      Future(checkAccessWrite(tourney(tse.toId), tse.orgDir, writeAccess))
    } else {
      load(tse.toId).map { 
        case Left(err)   => Left(err)
        case Right(trny) => checkAccessWrite(trny, tse.orgDir, writeAccess)
      }  
    }
  }


  /** get a tourney for read access without access ctrl 
    * 
    * @param toId    tourney identifieer
    * @param ec      execution context
    * @param cfg     application configuration
    * @param tonyDao database access
    * @return
    */
  def get(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]] = 
  {
    if (tourney.isDefinedAt(toId)) { 
      // use cached values
      tourney(toId).accessTime  = clock.millis()
      Future(Right(tourney(toId)))
    } else {
      load(toId).map { 
        case Left(err)   => Left(err)
        case Right(trny) => Right(trny)
      }  
    }
  }


  /** add adds a tourney to the database if not existing yet
   * 
   * @param  trny
   * @param  ec
   * @param  env
   * @param  tonyDao
   * @return 
   */
  def add(trnyBase: TournBase)(implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]] = 
    trnyBase.check() match {
       case Left(err)  => Future(Left(err))
       case Right(res) => tonyDao.insertOrUpdate(trnyBase).map { tony => 
          if (tony.id > 0) {
            val trny = Tourney.init(tony)
            if (tourney.isDefinedAt(trny.id)) tourney.remove(trny.id) 
            tourney(trny.id) = trny
            save(trny.id)
            Right(trny)
          } else {
            Left(Error("err0081.database.add"))
          } 
        }
    }


  /** add adds a tourney to the database if not existing yet
   * 
   * @param  trny
   * @param  ec
   * @param  env
   * @param  tonyDao
   * @return 
   */
  def add(trny: Tourney)(implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]] = { 
    tonyDao.insertOrUpdate(trny.getBase()).map { tony => 
      if (tony.id > 0) {
        if (tourney.isDefinedAt(tony.id)) tourney.remove(tony.id) 
        tourney(tony.id) = trny.copy(id=tony.id)        
        save(tony.id)
        Right(tourney(tony.id))
      } else {
        Left(Error("err0081.database.add"))
      } 
    }
  } 

  /** delete database  return true if tourney was found otherwise false
   *
   * @param toId     tourney identifier
   * @param orgDir   organization directory
   * @return 
   */
  def delete(toId: Long, orgDir: String, sDate: Int=0)(implicit ec: ExecutionContext, tonyDao: TourneyDAO, cfg: Configuration): 
    Future[Either[Error, String]] = {
    import scala.util.{Try, Success, Failure, Using}
    import scala.util.Using
    import scala.io.Source

    for {
      tony   <- if (sDate!= 0) tonyDao.findByPathDate(orgDir, sDate) else tonyDao.findByPathId(orgDir, toId)
      sDate  <- tony match { case Some(to) => Future(to.startDate); case None => Future("xxxxxxxx") }
      tonyId <- tony match { case Some(to) => Future(to.id); case None => Future(0L) }
      cnt    <- tonyDao.deleteById(tonyId)
    } yield {
      if (cnt==1 & tonyId !=0) {
        val trnyDir = cfg.get[String]("server.tourney.dir")
        val fSep   = System.getProperty("file.separator")
        val fNCfg  = s"${trnyDir}${fSep}${orgDir}${fSep}${sDate}_${tonyId}_Tourney.json"        
        val xfNCfg  = s"${trnyDir}${fSep}${orgDir}${fSep}x_${sDate}_${tonyId}_Tourney.json"

        (for {
          res1 <- moveFile(fNCfg, xfNCfg)
        } yield (res1)) match {
          case Left(err)  => Left(err.add("Cache.delete"))
          case Right(res) => Right( tony match { case Some(to) => to.name; case None => ""} )
        }
      } else {
        Right("")
      } 
    }
  }


  /** update tourney with basic information, database and file
   * 
   * @param  tb basic tourney information
   */ 
  def update(tb: TournBase)(implicit  ec: ExecutionContext, cfg: Configuration, tony: TourneyDAO): Future[Either[Error, Boolean]] = 
    get(tb.id).map {
      case Left(err)    => Left(err)
      case Right(trny)  => {
        tony.insertOrUpdate(tb).map { tb => Right(tb.id == trny.id) }
        trny.name    = tb.name
        trny.endDate = tb.endDate
        trny.typ     = TourneyTyp(tb.typ)
        trny.privat  = tb.privat

        (for {
          contact <- Contact.decode(tb.contact)
          address <- Address.decode(tb.address) 
        } yield { (contact, address) }) match {
          case Left(err)  => Left(err)
          case Right(res) => trny.contact = res._1; trny.address = res._2; Right(true)
        }
      }
    }


  /** update / insert tourney configuration from tourney transfer data
   * 
   * @param  trny tourney data
   */    
  def update(trny: Tourney)(implicit ec: ExecutionContext, env: Environment, tonyDao: TourneyDAO): Future[Either[Error, Long]] = 
  {
    val tb = trny.getBase()
    tonyDao.insertOrUpdate(tb).map { tony => 
      if (tony.id > 0) {
        logger.info(s"insert from tx: ${tony.name}(${tony.id})")
        if (tourney.isDefinedAt(tony.id)) { tourney.remove(tony.id) }
        tourney(tony.id) = trny.copy(id=tony.id)
        tourney(tony.id).writeTime  = clock.millis()
        Right(tony.id)
      } else {
        logger.error(s"insert: ${tb.name}/${tb.orgDir}.id})")
        Left(Error("err0083.database.insert"))
      }
    }
  }
  

  /** update - tourney configuration from CTT participant XML-file
   *           add tourney with competitions, players and clubs 
   */
  def update(cttTrny: CttTournament, toId: Long)
            (implicit  ec: ExecutionContext, env: Environment, cfg: Configuration, tony: TourneyDAO): Future[Either[Error,Seq[(Long, Int)]]] = 
    get(toId).map {
      case Left(err)    => Left(err)
      case Right(trny)  => if (date2Int(cttTrny.startDate) != trny.startDate) Left(Error("err0205.upload.file.startDate")) else {
        import scala.collection.mutable.ArrayBuffer

        // sets license of player which match the following criteria:
        // either name has one match or clubname has one match or ttr or birthyear
        def setPlayerCttLicense(player: Player, name2person: HashMap[String, ArrayBuffer[CttPerson]]) = {
          val name = s"${player.lastname}·${player.firstname}"
          player.setLicense(CttLicense(""))
          if (name2person.isDefinedAt(name)) {
            val peArray = (name2person(name)) 
            if (peArray.length == 1) player.setLicense(CttLicense(peArray(0).licenceNr)) else {
              val cpeArray = peArray.filter(_.clubName == player.clubName)
              if (cpeArray.length == 1) player.setLicense(CttLicense(cpeArray(0).licenceNr)) else {
                val tcpeArray = peArray.filter(_.ttr == player.getTTR)
                if (tcpeArray.length == 1) player.setLicense(CttLicense(tcpeArray(0).licenceNr))
              }  
            } 
          }
        }

        // update Pant2Comp database
        // Step 1: delete/remove all ident info in Pant2Comp database
        // Repeat Step 2-3 for all competitions, if competition matches
        // Return error for competitions with no matching CTT-Competition
        // Step 2: - generate licence -> ident info map 
        //         - generate lastname, firstname -> licence map
        //         - update licence info for every player who has not yet a licence number
        // Step 3: for every Pant2Comp entry update ident
        //

        // Step 0: initialize result
        val result = new ArrayBuffer[(Long, Int)]

        // Step 1 - Remove all ident values from Pant2Comp database entries
        trny.pl2co.foreach(_._2.ident = "") 

        
        for ((coId, comp) <- trny.comps) {
          // get approbriate competition
          val cttCompList = cttTrny.competitions.filter( _.matchWith(comp.getAgeGroup, comp.typ, comp.getRatingLowLevel, comp.getRatingUpperLevel, comp.getRatingRemark, comp.startDate) )
          if (cttCompList.size != 1) { 
            result += ((coId, -1))
          } else {
            val cttComp = cttCompList.head
            val name2person   = new HashMap[String, ArrayBuffer[CttPerson]]()
            // competition specific mapping of license to player ident
            // person/license has different player idents in different competitions
            val licence2Ident = new HashMap[String, String]().withDefaultValue("")

            // generate licence2Ident 
            for (pl <- cttComp.players) {
              val lic = pl.persons.map(p => p.licenceNr).mkString("·")
              licence2Ident(lic) = pl.id 
            }

            // generate license info map, containing detailed person info(csv) for each license

            for (pl <- cttComp.players; pe <- pl.persons) {
              // add license if necessary
              val peCsv = CSVConverter[CttPerson].to(pe)
              if (!trny.licenses.contains(pe.licenceNr)) trny.licenses(pe.licenceNr) = peCsv
              
              // generate name2person(licence) map which generates for every lastname·firstname an array of CttPersons
              val name = s"${pe.lastname}·${pe.firstname}"
              if (!name2person.isDefinedAt(name)) name2person(name) = ArrayBuffer(pe) else name2person(name) += (pe)
            }
            
            // update pl2co database, count number of entryies with ctt identifier
            var cnt = 0
            comp.typ match {

              case CompTyp.SINGLE =>
                trny.pl2co.filter(_._1._2 == coId).foreach { case (key, entry) => {
                  val plId    = entry.getSingleId
                  // update licence if necessary
                  setPlayerCttLicense(trny.players(plId), name2person)
                  entry.ident = licence2Ident(trny.players(plId).getLicense.value)
                  if (entry.ident != "") cnt = cnt + 1
                }}

              case CompTyp.DOUBLE =>
                trny.pl2co.filter(_._1._2 == coId).foreach { case (key, entry) => {
                  // update licence if necessary
                  entry.getDoubleId match {
                    case Left(err) => println(s"ERROR: ${err.toString()}")
                    case Right(id) => {
                      setPlayerCttLicense(trny.players(id._1), name2person)
                      setPlayerCttLicense(trny.players(id._2), name2person)
                      val (lic1, lic2)   = (trny.players(id._1).getLicense.value, trny.players(id._2).getLicense.value)
                      entry.ident = if (licence2Ident.isDefinedAt(s"${lic1}·${lic2}")) licence2Ident(s"${lic1}·${lic2}") else licence2Ident(s"${lic2}·${lic1}")
                      if (entry.ident != "") cnt = cnt + 1  
                    }
                  }

                }}

              case _ => logger.info(s"coId: ${coId} with competition typ: ${comp.typ} not supported" ) 

            } // ent match competition typ

            // update result info
            result += ((coId, cnt))

          } // valid coId
        }   // for every defined competition

        // return result list
        Right(result.toSeq)
      }  
    }            






  /** add - tourney configuration from CTT participant XML-file
   *        add tourney with competitions, players and clubs 
   */
  def add(ctt: CttTournament, orgDir: String, organizer: String, 
          contact: String = "lastname·firstname·phone·email",
          address: String = "description·country·zip·city·street")
         (implicit ec: ExecutionContext, env: Environment, cfg: Configuration, tonyDao: TourneyDAO, msgs : Messages): Future[Either[Error, Tourney]] = 
  {
    val startDate = date2Int(ctt.startDate) 
    val endDate   = date2Int(ctt.endDate)  

    val tb = TournBase(ctt.name, organizer, orgDir, startDate, endDate, ctt.ident, TourneyTyp.TT.id, true, contact, address, 0)

    tonyDao.findByPathDate(orgDir, startDate).flatMap {
      case Some(a) => { 
        logger.error(s"add CttTourney failed, tourney with this start date: ${startDate}  already exists.")
        Future(Left(Error("err0206.upload.file.sDateExists"))) 
      }
      case None    => tonyDao.insertOrUpdate(tb).map { tony => 
        if (tony.id > 0) {
          val license2id:  scala.collection.mutable.Map[String, Long] = scala.collection.mutable.Map().withDefaultValue(0L)
          tourney(tony.id) = Tourney.init(tony)
          for((club,i) <- ctt.getClubs.zipWithIndex) {
            tourney(tony.id).clubs(i+1) = club.copy(id = i+1)      
            tourney(tony.id).club2id(club.hash) = (i+1)
            tourney(tony.id).clubIdMax = i+1
          }
          for((person,i) <- ctt.getPersons.zipWithIndex) {
            val pl =  CttService.cttPers2Player(person)
            tourney(tony.id).players(i+1) = pl.copy(id=i+1, clubId=tourney(tony.id).club2id(pl.clubName.hashCode()))
            // generate hash value
            tourney(tony.id).player2id(pl.hash) = i+1
            license2id(pl.getLicense.value) = i+1
            tourney(tony.id).playerIdMax = i+1
            tourney(tony.id).licenses(person.licenceNr) = CSVConverter[CttPerson].to(person)
          }

          for((co,i) <- ctt.competitions.zipWithIndex) { 
            val comp = CttService.cttComp2Comp(co)
            tourney(tony.id).comps(i+1) = comp.copy(id=i+1)
            //tourney(tony.id).coName2id(comp.hash) = i+1
            // generate hash value
            tourney(tony.id).comp2id(comp.hash()) = i+1          
            tourney(tony.id).compIdMax = i+1
            val coId = tourney(tony.id).compIdMax 
            // ctt players map to pl2co or do2co entries. 
            comp.typ match {
              case CompTyp.SINGLE => 
                for(pls <- co.players) if (pls.persons.length == 1) {
                  val plId = license2id.getOrElse(pls.persons(0).licenceNr, 0L)
                  val sPant = Pant2Comp.single(plId, coId, PantStatus.REGI)
                  tourney(tony.id).pl2co((sPant.sno, coId)) = sPant
                }
              case CompTyp.DOUBLE => 
                for(pls <- co.players) if (pls.persons.length == 2) {
                  val plId1 = license2id.getOrElse(pls.persons(0).licenceNr,0L)
                  val plId2 = license2id.getOrElse(pls.persons(1).licenceNr,0L)
                  val dPant = Pant2Comp.double(plId1, plId2, coId, PantStatus.REGI)
                  tourney(tony.id).pl2co((dPant.sno, coId)) =  dPant
                }
              case _                   => logger.info(s"insert error: invalid competition typ")
            }
          } 

          tourney(tony.id).backupTime = clock.millis()
          tourney(tony.id).accessTime = clock.millis()
          tourney(tony.id).writeTime  = clock.millis()
          save(tony.id)
          Right(tourney(tony.id))
        } else {
          Left(Error("err0083.database.insert"))
        }
      }
    }
  }


  /** load - first check if there is one tourney with orgDir and startDate,
   *         then load data from disk
   *         return error if something failed
   */
  def load(orgDir: String, startDate: Int)(implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]] = 
    tonyDao.findByPathDate(orgDir, startDate).map {
      case None     => {
        logger.error(s"load -> orgDir: ${orgDir} startDate: ${startDate} not found")
        Left(Error("err0011.trny.load", s"${orgDir} / ${startDate.toString}" ))
      }   
      case Some(tB) => cache(tB.id, tB.orgDir, tB.startDate)
    }      


  /** load - first get orgDir and startDate form tourney database,
   *         then load data from disk
   *         return error if something failed
   */
  def load(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Future[Either[Error, Tourney]] = {
    logger.info(s"load -> toId: ${toId}")
    tonyDao.findById(toId).map {
      case None     => logger.error(s"load -> toId: ${toId} not found"); Left(Error("err0011.trny.load", toId.toString))
      case Some(tB) => logger.info(s"load -> toId: ${toId} result"); cache(toId, tB.orgDir, tB.startDate)
    }   
  }       


  /** save cached database to disk return true if tourney was cached otherwise false
    *
    * @param toId
    * @param ec
    * @param env
    * @return 
    */
  def save(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration): Either[Error, Boolean] = {
    if (tourney.isDefinedAt(toId)) {
      try {
        val trnyDir       = cfg.get[String]("server.tourney.dir")
        val fNameTourney  = s"${trnyDir}/${tourney(toId).orgDir}/${tourney(toId).startDate}_${toId}_Tourney.json"
        val pathToFileCfg = Paths.get(fNameTourney)

        Files.createDirectories(pathToFileCfg.getParent())
        Files.write(pathToFileCfg, tourney(toId).encode().getBytes(StandardCharsets.UTF_8))
        logger.info(s"save to disk: ${tourney(toId).name}(${toId}) version: ${Tourney.defaultEncodingVersion} -> ${fNameTourney}")
        Right(true)         
      } catch { case _: Throwable => 
        logger.error(s"save to disk failed: ${tourney(toId).name}(${toId})")
        Left(Error("err0082.database.save"))
      }
    } else {
      logger.warn(s"save to disk error: key for ${toId} not defined")
      Right(false)
    } 
  }


  /** clean removes unused cache entries, dump info to disk
   * 
   */
  def clean()(implicit  ec: ExecutionContext, cfg: Configuration): Unit = {
    val start = clock.millis
    for ((key, tourn) <- tourney) {
      logger.info(s"check backup(${key}): ${tourn.backupTime}/${tourn.writeTime}")

      // check for backup
      if ((tourn.backupTime < tourn.writeTime) & (key!=0)) {
        logger.info(s"backup(${key}): ${tourn.backupTime}/${tourn.writeTime}")
        tourn.backupTime = clock.millis
        save(key)
      }

      // check for unload
      if ((tourn.accessTime + 7200000L < clock.millis) & (key!=0)) {
        logger.info(s"unload(${key}): ${tourn.accessTime}/${clock.millis}")
        save(key)
        tourney.remove(key)
      }
    }
    logger.info(s"Backup and cleaning start/duration: ${clock.millis / 1000}s / ${clock.millis-start}ms ") 
  }


  /** cache loads tourney data from disk into cache
   * 
   * @param orgDir
   * @param startDate
   * @param ec
   * @param cfg
   * @param tonyDao
   * @return
   */
  def cache(toId: Long, orgDir: String, startDate: Int)
           (implicit ec: ExecutionContext, cfg: Configuration, tonyDao: TourneyDAO): Either[Error, Tourney] = 
  {     
    // import tourn.utils.Helper._
    import scala.util.{Try, Success, Failure, Using}
    import scala.util.Using
    import scala.io.Source

    logger.info(s"cache -> toId: ${toId} orgDir: ${orgDir} startDate: ${startDate}")

    val trnyDir = cfg.get[String]("server.tourney.dir")
    logger.info(s"cache -> trnyDir: ${trnyDir}")

    val fSep    = System.getProperty("file.separator")
    val fNCfg   = s"${trnyDir}${fSep}${orgDir}${fSep}${startDate}_${toId}_Tourney.json"
    //val fNRun   = s"${trnyDir}${fSep}${orgDir}${fSep}${startDate}_${toId}_TournRun.json"

    logger.info(s"cache -> toId: ${toId} file: ${fNCfg}")

    (for {
      res1 <- Using(Source.fromFile(fNCfg)) { source => source.mkString }
    } yield (res1)) match {
      case Failure(f) => { logger.error(f.toString); Left(Error("err0146.fileaccess.config", fNCfg, "cache")) }   
      case Success(cfgFile) => Tourney.decode(cfgFile) match {
        case Left(err)   => {
          logger.error(s"cache -> ${err}")
          logger.error(s"cache -> decode config file: ${cfgFile.take(50)}"); Left(err.add("cache(cfg)"))
        }  
        case Right(trny) => {
            try {
              // setup complete tourney
              // trny.run = trnyRun          
              trny.accessTime = clock.millis()
              trny.writeTime  = clock.millis()
              trny.backupTime = clock.millis()

              // initially generate playfield entries
              for(coph <- trny.cophs.values; m <- coph.matches) {   
                if (m.status == MEntry.MS_RUN && (m.playfield != "")) trny.playfields(m.playfield) = trny.genPlayfieldFromMatch(m, "")
              }

              if (tourney.isDefinedAt(toId)) { tourney.remove(toId) }
              tourney(toId) = trny
              logger.info(s"--------------------------------------------------------------------------")
              logger.info(s"  load tourney: ${trny.name} id: ${toId}")
              logger.info(s"  competitions / IdMax: ${trny.comps.size} / ${trny.compIdMax}")
              logger.info(s"  players / IdMax: ${trny.players.size} / ${trny.playerIdMax}")
              logger.info(s"  clubs / IdMax: ${trny.clubs.size} / ${trny.clubIdMax}")
              logger.info(s"  players in competitions: ${trny.pl2co.size}")
              logger.info(s"--------------------------------------------------------------------------") 

              Right(tourney(toId))
            } catch { case _: Throwable => Left(Error("err0012.trny.read", toId.toString, s"${orgDir} / ${startDate}")) }
        }  
      }
    }
  }

}