// package tourn.services

// import java.util.UUID
// import javax.inject._
// import java.time.Clock
// import java.nio.file.{Paths, Files}
// import java.nio.charset.StandardCharsets

// import scala.concurrent.{ ExecutionContext, Future }
// import scala.concurrent.duration._
// import scala.util.{Try, Success, Failure}
// import scala.collection.mutable.HashMap

// import play.api.i18n.Messages
// import play.api.libs.json.Json
// import play.api.{ Environment, Configuration, Logging }

// import upickle.default._

// import shared.model._
// import shared.utils._
// import shared.utils.Constants._
// import shared.utils.Routines._

// import models.daos.TourneyDAO
// import tourn.utils.Helper._

// import controllers.{ EventActor, ActorRefManager }

// @Singleton
// class  TrnyDBImpl @Inject()(
//   implicit ec:  ExecutionContext, cfg: Configuration, tdao: TourneyDAO
// ) extends TrnyDBIntf with Logging {

//   val clock:    Clock  = Clock.systemUTC()
//   val tourney:  HashMap[Long, Tourney] = HashMap()

//   implicit val kotxFormat = Json.format[KoRoundTx]
//   implicit val grtxFormat = Json.format[GroupTx]
//   implicit val cophFormat = Json.format[CompPhaseTx]
//   implicit val tCfgFormat = Json.format[TourneyTx]
//   implicit val tRunFormat = Json.format[TournRunTx]
  

//   def mapDefault[X,Y](hm: HashMap[X,Y], key: X, default: Y) = if (!hm.isDefinedAt(key)) default else hm(key)

//   /** get - returns a tourney or error
//    * 
//    * @param toId
//    * @param caller
//    * @param trCmd
//    * @param ec
//    * @param env
//    * @return
//    */
//   def get(toId: Long, callerOrgDir: String, trCmd: UpdateTrigger)(implicit ec: ExecutionContext): 
//     Future[Either[String, Tourney]] = 
//   {
//     if (tourney.isDefinedAt(toId)) { 
//       if (Crypto.checkAccess(tourney(toId).orgDir,callerOrgDir)) {
//         if (trCmd.cmdName!="") { 
//           //logger.info(s"trigger: ${trCmd.toString}")
//           tourney(toId).writeTime  = clock.millis()
//           EventActor.manager ! ActorRefManager.SendMessage(s"${ tourney(toId).orgDir}", trCmd.toString) 
//         } else {
//           tourney(toId).accessTime  = clock.millis()
//         }    
//         Future(Right(tourney(toId)))    
//       } else {
//         Future(Left("Access violation")) 
//       }

//     } else {
//       load(toId).map { 
//         case Left(err)   => Left(err)
//         case Right(trny) => {
//           if (Crypto.checkAccess(trny.orgDir, callerOrgDir)) {
//             if (trCmd.cmdName!="") EventActor.manager ! ActorRefManager.SendMessage(s"${ tourney(toId).orgDir}", trCmd.toString)
//             tourney(toId) = trny
//             Right(tourney(toId))
//           } else {
//             Left("Access violation") 
//           }
//         }
//       }
//     }
//   }  

//   def get(tse: TournSVCEnv, trCmd: UpdateTrigger)(implicit ec: ExecutionContext): Future[Either[String, Tourney]] =
//     get(tse.toId, tse.orgDir, trCmd)
    
//   def get(toId: Long, caller: Session, trCmd: UpdateTrigger)(implicit ec: ExecutionContext): Future[Either[String, Tourney]] = 
//     get(toId, caller.orgDir, trCmd)

     
//   def delete(toId: Long, orgDir: String, sDate: Int)(implicit ec: ExecutionContext, cfg: Configuration): Either[String, Boolean] = 
//   { 
//     try {
//       val trnyDir = getConfig("server.tourney.dir")
//       Files.deleteIfExists(Paths.get(s"${trnyDir}/${orgDir}/${sDate}_${toId}_TournCfg.json"))
//       Files.deleteIfExists(Paths.get(s"${trnyDir}/${orgDir}/${sDate}_${toId}_TournRun.json"))
//       Right(true)
//     } catch { case _: Throwable => Left("Error deleting tourney files") }
//   }


//   def save(toId: Long) (implicit ec: ExecutionContext, cfg: Configuration): Either[String, Boolean] = 
//   {
//     if (tourney.isDefinedAt(toId)) {
//       try {
//         val trnyDir = getConfig("server.tourney.dir")

//         val fNCfg  = s"${trnyDir}/${tourney(toId).orgDir}/${tourney(toId).startDate}_${toId}_TournCfg.json"
//         val fNRun  = s"${trnyDir}/${tourney(toId).orgDir}/${tourney(toId).startDate}_${toId}_TournRun.json"
//         val pathToFileCfg = Paths.get(fNCfg)
//         val pathToFileRun = Paths.get(fNRun)

//         Files.createDirectories(pathToFileCfg.getParent())
//         val tournCfgTx = tourney(toId).toTx()
//         val tournRunTx = tourney(toId).run.toTx(toId)

//         Files.write(pathToFileCfg, Json.toJson(tournCfgTx).toString.getBytes(StandardCharsets.UTF_8))
//         Files.write(pathToFileRun, Json.toJson(tournRunTx).toString.getBytes(StandardCharsets.UTF_8))
//         logger.info(s"save to disk: ${tourney(toId).name}(${toId}) -> ${fNCfg}")
//         Right(true)
//       } catch { case _: Throwable => Left(s"Couldn't save ${tourney(toId).name} with id: ${toId} to disk") }
//     } else {
//       Left(s"Couldn't save tourney with id: ${toId} to disk")
//     } 
//   }

//   /** insert transfer encoded tourney  
//     * 
//     *
//     * @param trnyTx
//     * @param ec
//     * @return
//     */

//   def insert(trnyTx: TourneyTx)( implicit ec: ExecutionContext): Either[String, Tourney] =
//   {
//     TournBase.decode(trnyTx.basis) match {
//       case Left(err)       => Left(err)
//       case Right(trnyBase) => {
//         val trny = new Tourney(trnyBase)
//         try {
//           trny.clubs    = collection.mutable.HashMap( trnyTx.clubs.map(ctx   => { val c  = Club.obify(ctx); c.id -> c}) : _*) 
//           trny.players  = collection.mutable.HashMap( trnyTx.players.map(ptx => { val p  = Player.obify(ptx); p.id -> p}) : _*)
//           trny.comps    = collection.mutable.HashMap( trnyTx.comps.map(cotx  => { val c  = Competition.obify(cotx); c.id -> c}) : _*)
//           trny.pl2co    = collection.mutable.HashMap( trnyTx.pl2co.map(p2c   => { val p  = Participant2Comp.obify(p2c); (p.sno,p.coId) -> p }) : _*)

//           for(club <- trny.clubs.values) {   
//             trny.clName2id(club.name) = club.id.toInt
//             if (club.id > trny.clubIdMax) trny.clubIdMax = club.id.toInt
//           }
//           for(pl <- trny.players.values) {   
//             trny.plNCY2id((pl.lastname, pl.firstname,pl.clubName,pl.birthyear)) = pl.id.toInt
//             if (pl.id > trny.playerIdMax) trny.playerIdMax = pl.id.toInt
//           }

//           for(comp <- trny.comps.values) {   
//             trny.coName2id(comp.name) = comp.id
//             if (comp.id > trny.compIdMax) trny.compIdMax = comp.id
//           }

//           logger.info(s"insert from tx: ${trny.name} with id: ${trny.id}")
//           if (tourney.isDefinedAt(trny.id)) { tourney.remove(trny.id) }
//           tourney(trny.id) = trny
//           tourney(trny.id).writeTime  = clock.millis()
//           Right(trny)
//         } catch { case _:Throwable => Left("Couldn't create tourney")}
//       }
//     }
//   } 


//   /** insert - tourney configuration from CTT participant XML-file
//    *           add tourney with competitions, players and clubs 
//    */
//   def insert(toId: Long, orgDir: String, organizer: String, ctt: CttTournament, contact: Contact, address: Address)
//             (implicit ec: ExecutionContext, cfg: Configuration, msgs: Messages): Either[String, Tourney] =
//   {          
//     import shared.utils.Routines._
    
//     val sDate = date2Int(ctt.startDate)
//     val eDate = date2Int(ctt.endDate)

//     val trnyBase = TournBase(ctt.name, organizer, orgDir, sDate, eDate, ctt.ident, TT_TT, true, contact.stringify, address.stringify, toId)
//     val trny = new Tourney(trnyBase)

//     for((club,i) <- ctt.getClubs.zipWithIndex) {
//       trny.clubs(i+1) = club.copy(id = i + 1)      
//       trny.clName2id(club.name) = (i+1)
//       trny.clubIdMax = i+1
//     }
//     for((person,i) <- ctt.getPersons.zipWithIndex) {
//       val pl =  CttService.cttPers2Player(person)
//       trny.players(i+1) = pl.copy(id=i+1, clubId=trny.clName2id(pl.clubName))
//       trny.plNCY2id((pl.lastname, pl.firstname,pl.clubName,pl.birthyear)) = i+1
//       trny.plLIC2id(pl.getLicenceNr) = i+1
//       trny.playerIdMax = i+1
//     }

//     for((co,i) <- ctt.competitions.zipWithIndex) { 
//       val comp = CttService.cttComp2Comp(co)
//       trny.comps(i+1) = comp.copy(id=i+1)
//       trny.coName2id(comp.name) = i+1
//       trny.compIdMax = i+1

//       // ctt players map to pl2co or do2co entries. 
//       comp.typ match {
//         case 1 => 
//           for(pls <- co.players) if (pls.persons.length == 1) {
//             val plId = mapDefault(trny.plLIC2id,pls.persons(0).licenceNr, 0L)
//             trny.pl2co((plId.toString, i+1)) =  Participant2Comp(plId.toString, i+1, pls.id, "",0)
//           }
//         case 2 => 
//           for(pls <- co.players) if (pls.persons.length == 2) {
//             val plId1 = mapDefault(trny.plLIC2id,pls.persons(0).licenceNr,0L)
//             val plId2 = mapDefault(trny.plLIC2id,pls.persons(1).licenceNr,0L)
//             val sno = plId1.toString + "·" + plId2.toString
//             trny.pl2co((sno, i+1)) =  Participant2Comp(sno, i+1, pls.id, "",0)
//           }
//         case _ => logger.info(s"insert error: invalid competition typ")
//       }
//     } 

//     trny.backupTime = clock.millis()
//     trny.accessTime = clock.millis()
//     trny.writeTime  = clock.millis()
//     tourney(toId) = trny
//     Right(tourney(toId))
//   }


//   /** load - load new tourney from disk if available otherwise take
//    *         basic tourney information from database if available
//    *         otherwise return false
//    */
//   def load(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration, tdao: TourneyDAO): Future[Either[String, Tourney]] = 
//   {
//     tdao.findById(toId).map {
//       case None            => Left(s"Error tourney with id: ${toId} not found")
//       case Some(tournBase) => { 
//         val orgDir    = tournBase.orgDir
//         val startDate = tournBase.startDate

//         try {
//           val trnyDir = getConfig("server.tourney.dir")
//           val fNCfg  = s"${trnyDir}/${orgDir}/${startDate}_${toId}_TournCfg.json"
//           val fNRun  = s"${trnyDir}/${orgDir}/${startDate}_${toId}_TournRun.json"
          
//           val trny = Tourney.fromTx(read[TourneyTx](Files.readAllBytes(Paths.get(fNCfg))))
//           trny.run = TournRun.fromTx(read[TournRunTx](Files.readAllBytes(Paths.get(fNRun))))

//           for(club <- trny.clubs.values) {   
//             trny.clName2id(club.name) = club.id.toInt
//             if (club.id > trny.clubIdMax) trny.clubIdMax = club.id.toInt
//           }
//           //logger.info(s"load(${tony.id}) step 4")
//           for(pl <- trny.players.values) {   
//             trny.plNCY2id((pl.lastname, pl.firstname,pl.clubName,pl.birthyear)) = pl.id.toInt
//             if (pl.id > trny.playerIdMax) trny.playerIdMax = pl.id.toInt
//           }
//           //logger.info(s"load(${tony.id}) step 5")
//           for(comp <- trny.comps.values) {   
//             trny.coName2id(comp.name) = comp.id
//             if (comp.id > trny.compIdMax) trny.compIdMax = comp.id
//           }

//           trny.accessTime = clock.millis()
//           trny.writeTime  = clock.millis()
//           trny.backupTime = clock.millis()

//           if (tourney.isDefinedAt(toId)) { tourney.remove(toId) }
//           tourney(toId) = trny
//           logger.info(s"loaded tourney: ${trny.name} id: ${toId} successfull with ${trny.compIdMax} competitions")
//           Right(tourney(toId))
//         } catch {
//           case _: Throwable => Left(s"Error reading tourney: id: ${toId} orgIdr: ${orgDir} startDate: ${startDate}")
//         }
//       }
//     }
//   }  


//   /** saveclean
//    */
//   def clean()(implicit ec: ExecutionContext): Unit = 
//   {
//     val start = clock.millis
//     for ((key, tourn) <- tourney) {

//       // check for backup
//       if ((tourn.backupTime < tourn.writeTime) & (key!=0)) {
//         logger.info(s"backup(${key}): ${tourn.backupTime}/${tourn.writeTime}")
//         tourn.backupTime = clock.millis
//         save(key)
//       }

//       // check for unload
//       if ((tourn.accessTime + 7200000L < clock.millis) & (key!=0)) {
//         logger.info(s"unload(${key}): ${tourn.accessTime}/${clock.millis}")
//         tourney.remove(key)
//       }
//     }
//     logger.info(s"Backup and cleaning start/duration: ${clock.millis / 1000}s / ${clock.millis-start}ms ") 
//   }

// }