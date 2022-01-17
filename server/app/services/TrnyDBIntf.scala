// package tourn.services

// import javax.inject.Inject
// import scala.concurrent.{ ExecutionContext, Future }
// import scala.concurrent.duration._

// import play.api.{ Configuration }
// import play.api.i18n.Messages

// import shared.model._

// import shared.utils._
// import shared.utils.Constants._

// import models.daos.TourneyDAO


// /**
//  * Database inteface for tourneys
//  */
// trait TrnyDBIntf {

//   // returns tourney from cache or loads it from disc
//   def get(toId: Long, callerOrgDir: String, trCmd: UpdateTrigger)(implicit ec: ExecutionContext): Future[Either[String, Tourney]] 
//   def get(toId: Long, caller: Session, trCmd: UpdateTrigger)(implicit ec: ExecutionContext): Future[Either[String, Tourney]] 
//   def get(tse: TournSVCEnv, trCmd: UpdateTrigger)(implicit ec: ExecutionContext): Future[Either[String, Tourney]] 
  
//   def delete(toId: Long, orgDir: String, sDate: Int)(implicit ec: ExecutionContext, cfg: Configuration): Either[String, Boolean]

//   def save(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration): Either[String, Boolean]

//   def insert(trnyTx: TourneyTx)(implicit ec: ExecutionContext): Either[String, Tourney]

//   /** insert - tourney configuration from CTT participant XML-file
//    *           add tourney with competitions, players and clubs 
//    */
//   def insert(toId: Long, orgDir: String, organizer: String, ctt: CttTournament, contact: Contact, address: Address)
//           (implicit ec: ExecutionContext, cfg: Configuration, msgs: Messages): Either[String, Tourney]


//   /** load - load new tourney from disk if available
//    */
//   def load(toId: Long)(implicit ec: ExecutionContext, cfg: Configuration, tdao: TourneyDAO): Future[Either[String, Tourney]]

//   /** clean - purges old database entries to disk
//    */
//   def clean()(implicit ec: ExecutionContext): Unit

// }