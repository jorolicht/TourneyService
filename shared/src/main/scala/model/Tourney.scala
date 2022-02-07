package shared.model

//import shared.model.gamesystem.Match
import scala.collection.mutable.{ ArrayBuffer, Map, HashMap }
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.{ Error, Return }
import shared.utils.Routines._
import shared.utils.Constants._


/*
**
**  TOURNEY
**
**    keeps all relevant information of a Tourney
**    - basic information is stored in a central SQLite database
**    - configuration information is stored in a file as json 
**    - input/run configuration is also stored in a file as Json
**
**      name:       String  = ""
**      organizer:  String  = ""    // name of the organizer (club or key word) of registered program/tourney user
**      orgdir:     String          // unified organizer name (used as directory)
**      startDate:  Int             // startdate format yyyymmdd as Integer
**      endDate:    Int     = 0 
**      ident:      String  = ""    // clickTTid 
**      typ:        Int     = 0     // 0 = unknown, 1 = Tischtennis, ...
**      privat:     Boolean = true  // privat tourneys are only seen by registered users
**      id:         Long            // primary key autoincrement 
**
*/
case class Tourney(
  var id:         Long,        
  var name:       String, 
  var organizer:  String, 
  val orgDir:     String, 
  val startDate:  Int, 
  var endDate:    Int, 
  var ident:      String, 
  var typ:        Int, 
  var privat:     Boolean                               = true, 
  var contact:    Contact                               = Contact("","","",""), 
  var address:    Address                               = Address("","","","",""), 
  var players:    Map[Long, Player]                     = Map(),
  var comps:      Map[Long, Competition]                = Map(),
  var clubs:      Map[Long, Club]                       = Map(),  // clubs map with key (id) 
  var pl2co:      Map[(String, Long), Participant2Comp] = Map(),  // registered player in a competition key: (sno, coId)
  var coSects:    Map[(Long, Int), CompSection]         = Map(),  // map (coId, secId) -> Competition Section
  var cophs:      Map[(Long, Int), CompPhase]           = Map(),  // map (coId, coPh)  -> Competition Phase
  var playfields: Map[Int, Playfield]                   = Map()   // map (playfieldNo) -> Playfield
)
{

  // inverse hashmaps for fast access to players, clubs, ...
  var clName2id:   HashMap[String, Long]                     = HashMap()  // club -> id
  var coName2id:   HashMap[String, Long]                     = HashMap()  // competition -> id
  var plNCY2id:    HashMap[(String,String,String,Int), Long] = HashMap()
  var plLIC2id:    HashMap[String, Long]                     = HashMap()  


  /*
   * tourney management data
   */
  var playerIdMax: Long = 0L
  var clubIdMax:   Long = 0L
  var compIdMax:   Long = 0L
  var accessTime:  Long = 0L
  var backupTime:  Long = 0L
  var writeTime:   Long = 0L  


  def encode(): String = write[Tourney](this)
  def isDummy() = (id == 0L)
  def getToId() = this.id
  def getBase() = TournBase(name, organizer, orgDir, startDate, endDate, ident, typ, privat, contact.stringify, address.stringify, id)
  def getStartDate(lang: String, fmt:Int=0): String = int2date(startDate, lang, fmt)

   
  /*
   * miscellanous tourney routines
   */
  def getParticipantName(sno: String, coId: Long, format: Int): String = {
    import shared.utils.Constants._ 
    try {
      comps(coId).typ match {
        case CT_SINGLE => players(getMDLong(sno, 0)).getName(format)
        case CT_DOUBLE => players(getMDLong(sno, 0)).lastname + "/" + players(getMDLong(sno, 1)).lastname
        case _         => "error participant name: invalid typ"
      }
    } catch { case _: Throwable => "error participant name: invalid sno" }
  }


  // get all competitions with id and name
  def getNamesComp(): Seq[(Long,String)] = {
    (for { (k,co)  <- comps } yield {
      (co.id, co.name, co.getAgeGroup, co.getRatingRemark, co.typ, co.startDate)
    }).toSeq.sortWith(_._5 > _._5).sortWith(_._6 < _._6).map(x => (x._1,x._2))
  }

  // get all players with id and name of the competitions
  def getNamesClubPlayer(coId: Long): Seq[(String,String)] = {
    val res = for { p2c  <- pl2co.values } yield { 
      if (p2c.coId == coId)  {
        comps(coId).typ match {
          case 1 => ( players(p2c.getPlayerId).getClub(), players(p2c.getPlayerId).getName() )
          case 2 => {
            ( s"${players(p2c.getPlayerId1).getClub()}/${players(p2c.getPlayerId2).getClub()}", 
              s"${players(p2c.getPlayerId1).lastname}/${players(p2c.getPlayerId2).lastname}" ) 
          }
          case _ => ("","")
        }
      } else {
        ("","")
      }
    }  
    res.filter(_._2 != "").toSeq.sortWith(_._2 < _._2)
  }

  /** addComp add a competition if it is possible, id must be 0 
   *
   */ 
  def addComp(co: Competition): Either[Error, Competition] = {
    if (co.id != 0)  {
      Left(Error("err0153.trny.addComp", co.id.toString)) 
    } else if (!co.validateDate(startDate, endDate)) {
      Left(Error("err0015.trny.compDate", co.startDate)) 
    } else if (coName2id.isDefinedAt(co.hash)) {
      Left(Error("err0016.trny.compExistsAlready"))
    } else {
      // ok add new one
      val coIdMax = compIdMax + 1
      compIdMax = coIdMax
      comps(coIdMax) = co.copy(id = coIdMax)     
      coName2id(co.hash) = coIdMax
      Right(comps(coIdMax))
    }
  }

  /** getCompCnt - returns number of registered and active participants  
   *
   */ 
  def getCompCnt(co: Competition): (Int, Int) = co.typ match {    
    case CT_SINGLE | CT_DOUBLE => {
      val pl2cos   = pl2co.values.filter(_.coId==co.id).toSeq
      val cnt      = pl2cos.length
      val cntActiv = pl2cos.filter(_.status > 0).length
      (cnt, cntActiv)
    }
    case _ => (0, 0)
  }

  /** setCompDraw - returns new secId  
   *  CompSection(val id: Int, val preId: Int, val coId: Long, val name: String, val secTyp: Int) 
   */
  def setCompDraw(co: Competition, nameSec: String, typSec: Int, prevSec: Int, 
                  winners: Boolean, noWinSets: Int, 
                  pants: Array[SNO]): Either[Error, Int] = {
    addSect(prevSec, co.id, nameSec, typSec, winners) match {
      case Left(err)    => Left(Error("err0171.trny.setCompDraw"))
      case Right(secId) => {        
        if (coSects.isDefinedAt((co.id, secId))) {
          coSects((co.id, secId)).pants     = pants
          coSects((co.id, secId)).noPlayer  = pants.filter(p => p.value != SNO_BYE).size
          coSects((co.id, secId)).size      = pants.size
          coSects((co.id, secId)).noWinSets = noWinSets
          Right(secId) 
        } else {
          Left(Error("err0172.trny.setCompDraw"))
        }
      }
    }
  }

  /** setComp - sets new values of existing competitions
   * 
   */
  def setComp(co: Competition): Either[Error, Competition] = {
    if (co.id == 0)  {
      Left(Error("err0154.trny.setComp")) 
    } else if (!co.validateDate(startDate, endDate)) {
      Left(Error("err0015.trny.compDate", co.startDate)) 
    } else if (coName2id.isDefinedAt(co.hash)) {   
      // hash value exists, if it belongs to different id
      // don't allow it
      val coId = coName2id(co.hash)
      if (coId != co.id) {
        // do not allow to change a competition to an existing
        Left(Error("err0016.trny.compExistsAlready"))
      } else {
        // set new competition and return it
        comps(co.id) = co
        Right(comps(co.id))
      }
    } else {
      // ok changes on name, type, .... which result in hash changes
      // thats ok, caveat hash changes remove old hash and add new one!
      coName2id -= comps(co.id).hash
      coName2id(co.hash) = co.id
      comps(co.id) = co
      Right(comps(co.id))
    }
  }

  /** delComp - delete competition with id
   * 
   */ 
  def delComp(coId: Long):Either[Error, Boolean] = {
    if (coId == 0) {
      Left(Error("err0014.trny.compNotFound", coId.toString)) 
    } else if (comps.isDefinedAt(coId)) {
      val co = comps(coId)
      if (coName2id.isDefinedAt(co.hash)) coName2id.remove(co.hash)
      comps.remove(coId)        
      Right(true)
    } else {  
      Left(Error("err0014.trny.compNotFound", coId.toString))
    }
  }

  /** getCompName - get name of competition
   * 
   */ 
  def getCompName(coId: Long): String = if (comps.isDefinedAt(coId)) comps(coId).name else ""


  //addSect - adds a competition section to a competition 
  def addSect(prevSec: Int, coId: Long, nameSec: String, typSec: Int, winSec: Boolean=true): Either[Error, Int] = { 
    val newId = coSects.keys.filter(x => x._1 == coId).map(x => x._2).max + 1

    if (coSects.isDefinedAt((coId, newId))) {
      Left(Error("err0169.trny.addSect"))
    } else if (prevSec == 0) {
      // new (first) entry without previous entry
      coSects((coId, newId)) = new CompSection(newId, prevSec, coId, nameSec, typSec)
      Right(newId)
    } else if (!coSects.isDefinedAt((coId, prevSec))) { 
      // now previous entry
      Left(Error("err0048.trny.addSect"))
    } else {
      // new entry with previous entry
      coSects((coId, newId)) = new CompSection(newId, prevSec, coId, nameSec, typSec)
      if (winSec) {
        coSects((coId, prevSec)).winId = newId
      } else {
        coSects((coId, prevSec)).looId = newId
      }
      Right(newId)
    }
  }

  /** setPlayer updates existing player 
   *  if necessary creates new club entry
   */
  def setPlayer(pl: Player): Either[Error, Player] =
    if (pl.id == 0) {
      Left(Error("err0157.svc.setPlayer", pl.getName()))
    } else {
      val plId = plNCY2id.getOrElse((pl.lastname, pl.firstname, pl.clubName, pl.birthyear), 0L)
      if (plId == 0) {
        // add clubname (idempotent)
        val club = addClub(pl.clubName)
        // remove old hash entry, change require new one
        val oPlayer = players(pl.id)
        plNCY2id -= ((oPlayer.lastname, oPlayer.firstname, oPlayer.clubName, oPlayer.birthyear))
        plNCY2id += ((pl.lastname, pl.firstname, pl.clubName, pl.birthyear) -> pl.id)
        players(pl.id) = pl.copy(clubId = club.id)
        Right(players(pl.id))
      } else if (plId == pl.id) {
        // change with same hash entry
        players(pl.id) = pl
        Right(players(pl.id))
      } else {
        // change with hash collision - no allowed
        Left(Error("err0158.svc.setPlayer", pl.getName()))
      }
    }

  
  /** addPlayer - adds new player if it does not exist already
   */
  def addPlayer(pl: Player): Either[Error, Player] =
    if (pl.id != 0) {
      Left(Error("err0155.svc.addPlayer", pl.id.toString))
    } else {
      // check whether an other player exists with same name ....
      if (plNCY2id.getOrElse((pl.lastname, pl.firstname, pl.clubName, pl.birthyear), 0L) > 0) {
        Left(Error("err0156.svc.addPlayer", pl.getName()))
      } else {
        val club = addClub(pl.clubName)
        // critical path (lock?)
        val newId = playerIdMax + 1
        playerIdMax = newId
        plNCY2id((pl.lastname, pl.firstname, pl.clubName, pl.birthyear)) = newId
        players(newId) = pl.copy(id = newId, clubId = club.id)
        Right(players(newId))
      }
    }

  /** addClub - adds new Club or returns existing
   */
  def addClub(name: String): Club = {
    val clId = clName2id.getOrElse(name, 0L)
    if (clId == 0) {
      val nclId = clubIdMax + 1
      clubIdMax = nclId
      clubs(nclId) = new Club(nclId, name)
      clName2id(name) = nclId
      clubs(nclId)
    } else {
      clubs(clId)
    }
  }  

  def setParticipantStatus(coId: Long, sno: String, status: Int): Either[Error, Int] = {
    if ( pl2co.isDefinedAt((sno, coId)) ) { 
      pl2co((sno,coId)).status = status
      Right(status)
    } else { 
      Left(Error("err0027.tourney.setParticipantStatus", sno, coId.toString)) 
    }      
  }


  /*  getParticipants - returns List/Array of participant entries, empty Array if 
   *                    no participants or wrong/invalid coId
   */
  // def getParticipants(coId: Long): Array[ParticipantEntry] = {
  //   if (comps.isDefinedAt(coId)) {
  //     val cTyp = comps(coId).typ
  //     pl2co.keys
  //      .filter(x => (x._2 == coId))
  //      .filter(x => pl2co(x).status == 1)
  //      .map( x => pl2co(x).getEntry(players, cTyp) ).toArray
  //   } else {
  //     Array[ParticipantEntry]()
  //   }
  // }

  // print readable tourney - for debug purposes
  override def toString() = {
    def compsStr() = {
      val str = new StringBuilder("-- COMPETITIONS\n")
      for { (k,c) <- comps }  yield { str ++= s"  ${c.toString}\n" }; str.toString
    }
    def playersStr() = {
      val str = new StringBuilder("-- PLAYERS\n")
      for { (k,p) <- players }  yield { str ++= s"  ${p.toString}\n" }; str.toString
    }
    def clubssStr() = {
      val str = new StringBuilder("-- CLUBS\n")
      for { (k,cl) <- clubs }  yield { str ++= s"  ${cl.toString}\n" }; str.toString
    }    
    def pl2coStr() = {
      val str = new StringBuilder("-- PLAYERS-2-COMPETITIONS\n")
      for { (k,p2c) <- pl2co }  yield { str ++= s"  ${p2c.toString}\n" }; str.toString
    }

    def pfsStr() = {
      val str = new StringBuilder("-- PLAYFIELDS\n")
      for { (k,pf) <- playfields }  yield { str ++= s"  ${pf.toString}\n" }; str.toString
    }
    def cphsStr() = {
      val str = new StringBuilder("-- COMPETITION PHASES\n")
      for { (k,coph) <- cophs }  yield { str ++= s"  ${coph.toString}\n" }; str.toString
    }

    def coSecStr() = {
      val str = new StringBuilder("-- COMPETITION SECTIONS\n")
      for { (k, cSecs) <- coSects }  yield { str ++= s"  ${cSecs.toString}\n" }; str.toString
    } 

    s"""\nTOURNEY[${id}]
      |  ${name} von: ${startDate} bis: ${endDate}
      |  ${organizer} directory: ${orgDir} typ: ${typ}
      |  Contact: ${contact} 
      |  Address: ${address} 
      |  --
      |  ${compsStr()}
      |  ${clubssStr()}
      |  ${playersStr()}
      |  ${pl2coStr()}
      |  ${cphsStr()}
      |  ${coSecStr()}
      |  ${pfsStr()}
      |""".stripMargin('|')
  } 

}


object Tourney {
  implicit def rw: RW[Tourney] = macroRW

  def init                   = Tourney(0L, "dummy", "", "", 19700101, 19700101, "", 0)
  def init(tBase: TournBase) = Tourney(tBase.id, tBase.name, tBase.organizer, tBase.orgDir, tBase.startDate, tBase.endDate, tBase.ident, tBase.typ)

  def decode(trnyStr: String): Either[Error, Tourney] = 
    if (trnyStr.length > 0 ){
      try {
        val trny = read[Tourney](trnyStr)
        // create inverse hashtables and maxId entries
        for(club <- trny.clubs.values) {   
          trny.clName2id(club.name) = club.id.toInt
          if (club.id > trny.clubIdMax) trny.clubIdMax = club.id.toInt
        }
        for(pl <- trny.players.values) {   
          trny.plNCY2id((pl.lastname, pl.firstname,pl.clubName,pl.birthyear)) = pl.id.toInt
          if (pl.id > trny.playerIdMax) trny.playerIdMax = pl.id.toInt
        }
        for(comp <- trny.comps.values) {   
          trny.coName2id(comp.hash) = comp.id
          if (comp.id > trny.compIdMax) trny.compIdMax = comp.id
        }        
        Right(trny)
      }  
      catch { case _: Throwable => Left( Error("err0070.decode.Tourney", trnyStr.take(20), "", "Tourney.decode") ) }
    } else {
      Left(Error("err0070.decode.Tourney", "<empty input>", "", "Tourney.decode"))
    }

}


/*
**
**  TOURNEY RUN - Tourney Runtime Information
**
*/
// class TournRun(val id: Long) 
// {
//   /*
//    * tourney runtime data
//    */
//   // map coId, secId -> Competition Section
//   var coSects:    HashMap[(Long, Int), CompSection] = HashMap() 
//   // map coId, coPh  -> Competition Phase
//   var cophs:      HashMap[(Long, Int), CompPhase]   = HashMap() 
//   var playfields: HashMap[Int, Playfield]           = HashMap() 
  
//   /*
//    * methods for converting
//    */
//   def encode(): String = write[TournRunTx](toTx(id))

//   def toTx(id: Long): TournRunTx = {
//     val pfs     = for ((k,v) <- this.playfields) yield v.stringify
//     val cphs    = for ((k,v) <- this.cophs)      yield v.toTx
//     val cSects  = for ((k,v) <- this.coSects)    yield v.toTx
//     //val cSecs   = for (v <- this.coSects)        yield v.toTx 
//     TournRunTx(id, cSects.toList, cphs.toList, pfs.toList) 
//   }

//   override def toString(): String = {
//     def pfsStr() = {
//       val str = new StringBuilder("-- PLAYFIELDSS\n")
//       for { (k,pf) <- playfields }  yield { str ++= s"  ${pf.toString}\n" }; str.toString
//     }
//     def cphsStr() = {
//       val str = new StringBuilder("-- COMPETITION PHASES\n")
//       for { (k,coph) <- cophs }  yield { str ++= s"  ${coph.toString}\n" }; str.toString
//     }

//     def coSecStr() = {
//       val str = new StringBuilder("-- COMPETITION SECTIONS\n")
//       for { (k, cSecs) <- coSects }  yield { str ++= s"  ${cSecs.toString}\n" }; str.toString
//     }    

//     s"""\nTOURNEY RUN INFORMATION[${id}]
//        |  ${pfsStr()}
//        |  ${cphsStr()}
//        |  ${coSecStr()}
//        |""".stripMargin('|')
//   }
// }


// object TournRun {

//   def decode(trnyStr: String): Either[Error, TournRun] = 
//     try Right(fromTx(read[TournRunTx](trnyStr)))
//     catch { case _: Throwable => Left(Error("err0071.decode.TournRun", trnyStr.take(20))) }

//   def fromTx(tx: TournRunTx): TournRun = {
//     val tR         = new TournRun(tx.id)
//     val plfs       = tx.playfields.map(x => Playfield.decode(x)).partitionMap(identity)._2
//     //tR.coSects     = tx.coSects.map(x => CompSection.fromTx(x)).to(ArrayBuffer) 
//     tR.coSects     = collection.mutable.HashMap( tx.coSects.map(c => (c.coId, c.id)-> CompSection.fromTx(c) ) : _*) 
//     tR.cophs       = collection.mutable.HashMap( tx.cophs.map(c => (c.coId,c.coPh)-> CompPhase.fromTx(c) ) : _*) 
//     tR.playfields  = collection.mutable.HashMap( plfs.map(p => p.nr -> p) : _*) 
    
//     tR
//   }
// }

// case class TournRunTx(id: Long, coSects: Seq[CompSectionTx], cophs: Seq[CompPhaseTx], playfields: Seq[String])
// object TournRunTx { implicit def rw: RW[TournRunTx] = macroRW }