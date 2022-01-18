package shared.model

//import shared.model.gamesystem.Match
import scala.collection.mutable.{ ArrayBuffer, HashMap }
import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils.{ Error, Return }
import shared.utils.Routines._
import shared.utils.Constants._

/*
**
**  TOURNBASE
**
**  representing a row in the Tourney Table with
**  unique id, name of Tourney, club, start and 
**  endDate (format yyyymmdd as Integer)
**  and clickTTid 
*/
case class TournBase(
  val name:      String, 
  var organizer: String,          // name of the organizer (club or key word) of registered program/tourney user
  val orgDir:    String,          // unified organizer name (used as directory)
  val startDate: Int, 
  var endDate:   Int, 
  var ident:     String,          // clickTTid 
  val typ:       Int,             // 0 = unknown, 1 = Tischtennis, ...
  var privat:    Boolean,         // privat tourneys are only seen by registered users
  var contact:   String = "",     // lastname·firstname·phone·email
  var address:   String = "",     // "description·country·zip·city·street"
  val id:        Long = 0L        // autoincrement
) extends {
   
  def encode()  = s"${name}^${organizer}^${orgDir}^${startDate}^${endDate}^${ident}^${typ}^${privat}^${contact}^${address}^${id}^_"
  def stringify = s"${name}^${organizer}^${orgDir}^${startDate}^${endDate}^${ident}^${typ}^${privat}^${contact}^${address}^${id}^_"
 
  def getAddrDescription: String = getMDStr(address,0);    def setAddrDescription(value: String) = { address = setMD(address,value,0) }
  def getAddrCountry: String     = getMDStr(address,1);    def setAddrCountry(value: String)     = { address = setMD(address,value,1) }
  def getAddrZIP: String         = getMDStr(address,2);    def setAddrZIP(value: String)         = { address = setMD(address,value,2) }
  def getAddrCity: String        = getMDStr(address,3);    def setAddrCity(value: String)        = { address = setMD(address,value,3) }
  def getAddrStreet: String      = getMDStr(address,4);    def setAddrStreet(value: String)      = { address = setMD(address,value,4) }

  def getContactLastname: String  = getMDStr(contact,0);   def setContactLastname(value: String)  = { contact = setMD(contact,value,0) }
  def getContactFirstname: String = getMDStr(contact,1);   def setContactFirstname(value: String) = { contact = setMD(contact,value,1) }
  def getContactPhone: String     = getMDStr(contact,2);   def setContactPhone(value: String)     = { contact = setMD(contact,value,2) }
  def getContactEmail: String     = getMDStr(contact,3);   def setContactEmail(value: String)     = { contact = setMD(contact,value,3) }
  def getContactName: String      = {
    val lname = getContactLastname
    val fname = getContactFirstname
    if (lname != "" & fname != "") {
      lname + ", " + fname 
    } else {
      lname + fname 
    }
  }

  def getStartDate(lang: String, fmt:Int=0): String = int2date(startDate, lang, fmt)
  def getEndDate(lang: String, fmt:Int=0): String = int2date(endDate, lang, fmt)

  def setContact(name: String, email: String, phone: String) = {
    val nArr1 = name.split(",")
    val nArr2 = name.split(" ")      
    val (lastname,firstname) = if (nArr1.length>=2) {
      (nArr1(0).trim, nArr1(1).trim)
    } else if (nArr2.length>=2) {
      (nArr2(0).trim, nArr2(1).trim) 
    } else {
      (name.trim,"")
    }
    contact = s"${lastname}·${firstname}·${phone}·${email}"
  }

  def setAddress(desc: String, country: String, zip: String, city: String, street: String) = {
    address = s"${desc}·${country}·${zip}·${city}·${street}"
  }

}  
 
 
object TournBase {
  // necessary workaround for slick
  val tupled = (this.apply _).tupled
  implicit def rw: RW[TournBase] = macroRW

  def dummy(organizer: String, orgDir: String, date: Int, typ: Int) 
    = new TournBase("", organizer, orgDir, date, 0, "", typ, true, "", "", 0L)

  def getInstance(name: String, organizer: String, orgDir: String, startDate: Int, typ: Int) 
    = new TournBase(name, organizer, orgDir, startDate, 0, "", typ, true, "", "", 0L)

  def obify(x: String) = objectify(x).getOrElse(TournBase("", "", "", 0, 0, "", 0, true, "", "", 0L))
  def objectify(x: String) : Option[TournBase] = {
    val tb = x.split("\\^")
    try { 
      Some(TournBase(tb(0), tb(1), tb(2), tb(3).toInt, tb(4).toInt, tb(5), tb(6).toInt, tb(7).toBoolean, tb(8), tb(9), tb(10).toLong))
    } catch { case _: Throwable => None }
  }
  def decode(x: String) : Either[Error, TournBase] = {
    val tb = x.split("\\^")
    try Right(TournBase(tb(0), tb(1), tb(2), tb(3).toInt, tb(4).toInt, tb(5), tb(6).toInt, tb(7).toBoolean, tb(8), tb(9), tb(10).toLong))
    catch { case _: Throwable => Left(Error("err0062.decode.TournBase", x)) }
  }

  def encSeq(tBs: Seq[TournBase]): String = {
    write[TournBases](TournBases(tBs.map(_.stringify)))
  }

  def decSeq(tbStr: String): Either[Error, Seq[TournBase]] = {
    try {
      val tBs = read[TournBases](tbStr)
      (tBs.list.map{ tb => TournBase.decode(tb) }).partitionMap(identity) match {
        case (Nil, rights)      => Right(rights.toSeq)
        case (firstErr :: _, _) => Left(firstErr)
      }
    } catch { case _: Throwable => Left(Error("err0144.decode.TournBases", tbStr.take(20))) }
  }

}  

case class TournBases (list: Seq[String])
object TournBases { implicit def rw: RW[TournBases] = macroRW }


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
class Tourney(var name: String, var organizer: String, val orgDir: String, 
              val startDate: Int, var endDate: Int, var ident: String, var typ: Int, 
              var privat: Boolean, var contact: Contact, var address: Address, 
              val id: Long) 
{

  def this(orgDir: String, startDate: Int, id: Long) {
    this("", "", orgDir, startDate, 0, "", 0, true, Contact("","","",""), Address("","","","",""), id)
  }

  def this(tb: TournBase) {
    this(tb.name, tb.organizer, tb.orgDir, tb.startDate, tb.endDate, tb.ident, tb.typ, tb.privat, 
         Contact(tb.getContactLastname,tb.getContactFirstname,tb.getContactPhone,tb.getContactEmail), 
         Address(tb.getAddrDescription,tb.getAddrCountry,tb.getAddrZIP,tb.getAddrCity,tb.getAddrStreet),
         tb.id)
  }

  def getToId(): Long = { this.id } 
  def getBase() = TournBase(name, organizer, orgDir, startDate, endDate, ident, typ, privat, contact.stringify, address.stringify, id)
  def isDummy()  = (orgDir == "dummy")
  def getStartDate(lang: String, fmt:Int=0): String = int2date(startDate, lang, fmt)
  
  /** copy to a new Tourney and overwrite/copy TournBase 
    * 
    * @param trnyBase
    * @param trny
    * @return new Tourney
    */
  def copy(trnyBase: TournBase): Tourney = {
    val nTrny         = new Tourney(trnyBase)
    nTrny.players     = this.players
    nTrny.comps       = this.comps
    nTrny.clubs       = this.clubs
    nTrny.pl2co       = this.pl2co
    nTrny.clName2id   = this.clName2id
    nTrny.coName2id   = this.coName2id
    nTrny.plNCY2id    = this.plNCY2id
    nTrny.plLIC2id    = this.plLIC2id

    nTrny.run         = this.run
    nTrny.playerIdMax = this.playerIdMax
    nTrny.clubIdMax   = this.clubIdMax
    nTrny.compIdMax   = this.compIdMax
    nTrny.accessTime  = this.accessTime
    nTrny.backupTime  = this.backupTime
    nTrny.writeTime   = this.writeTime
    nTrny
  }


  /*
   * tourney configuration data
   */
  // configuration of competition phases, mapping of (coId,coPh) to its Phase
  
  var players:     HashMap[Long, Player]                     = HashMap()
  var comps:       HashMap[Long, Competition]                = HashMap() 
  var clubs:       HashMap[Long, Club]                       = HashMap() 
  // entries for a player registered/playing in a competition
  var pl2co:       HashMap[(String, Long), Participant2Comp] = HashMap()  // key: (sno, coId)

  // inverse hashmaps for fast access to players and clubs
  // club -> id
  var clName2id:   HashMap[String, Long]                     = HashMap()
  // competition -> id
  var coName2id:   HashMap[String, Long]                     = HashMap()
  var plNCY2id:    HashMap[(String,String,String,Int), Long] = HashMap()
  var plLIC2id:    HashMap[String, Long]                     = HashMap()
 
  var run = new TournRun(0L)                                


  /*
   * tourney management data
   */
  var playerIdMax: Long = 0L
  var clubIdMax:   Long = 0L
  var compIdMax:   Long = 0L
  var accessTime:  Long = 0L
  var backupTime:  Long = 0L
  var writeTime:   Long = 0L     

  /*
   * methods for converting
   */

  def encode(): String = write[TourneyTx](this.toTx)
 
  def toTx(): TourneyTx = {
    val comps   = for ((k,v) <- this.comps) yield v.stringify
    val clubs   = for ((k,v) <- this.clubs) yield v.stringify
    val players = for ((k,v) <- this.players) yield v.stringify
    val pl2co   = for ((k,v) <- this.pl2co) yield v.stringify
    TourneyTx(TournBase(name, organizer, orgDir, startDate, endDate, ident, typ, privat, 
                        contact.stringify, address.stringify, id).stringify,
                        comps.toList, clubs.toList, players.toList, pl2co.toList)
  } 


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
      |""".stripMargin('|')
  } 


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
  def addSect(prevSecId: Int, coId: Long, name: String, secTyp: Int, winSec: Boolean=true): Either[Error, Int] = { 
    val newId = run.coSects.keys.filter(x => x._1 == coId).map(x => x._2).max + 1

    if (run.coSects.isDefinedAt((coId, newId))) {
      Left(Error("err0169.trny.addSect"))
    } else if (prevSecId == 0) {
      // new (first) entry without previous entry
      run.coSects((coId, newId)) = new CompSection(newId, prevSecId, coId, name, secTyp)
      Right(newId)
    } else if (!run.coSects.isDefinedAt((coId, prevSecId))) { 
      // now previous entry
      Left(Error("err0048.trny.addSect"))
    } else {
      // new entry with previous entry
      run.coSects((coId, newId)) = new CompSection(newId, prevSecId, coId, name, secTyp)
      if (winSec) {
        run.coSects((coId, prevSecId)).winId = newId
      } else {
        run.coSects((coId, prevSecId)).looId = newId
      }
      Right(newId)
    }
  }

  //setSectPlayer - sets the participants to competition section
  def setSectPlayer(coId: Long, secId: Int, pEntries: Array[ParticipantEntry]): Either[Error, Int] = { 
    import shared.utils.Constants._
    if (run.coSects.isDefinedAt((coId, secId))) {
      run.coSects((coId, secId)).pants = pEntries
      run.coSects((coId, secId)).noPlayer = pEntries.filter(p => p.sno != SNO_BYE).size
      run.coSects((coId, secId)).size = pEntries.size
      Right(run.coSects((coId, secId)).noPlayer) 
    } else {
      Left(Error("err0170.trny.setSectPlayer"))
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
}


object Tourney {
  def getDummy = new Tourney("dummy", 19700101, 0L)
  def init = new Tourney("dummy", 19700101, 0L)

  def decode(trnyStr: String): Either[Error, Tourney] = 
    if (trnyStr.length > 0 ){
      try fromTx(read[TourneyTx](trnyStr))
      // try fromTxOld(read[TourneyTx](trnyStr))
      catch { case _: Throwable => Left( Error("err0070.decode.Tourney", trnyStr.take(20), "", "Tourney.decode") ) }
    } else {
      Left(Error("err0070.decode.Tourney", "<empty input>", "", "Tourney.decode"))
    }

  def fromTx(tx: TourneyTx): Either[Error, Tourney] = {
    (for {
      tBase    <- TournBase.decode(tx.basis)
      clubs    <- Club.decSeq(tx.clubs) 
      players  <- Player.decSeq(tx.players) 
      comps    <- Competition.decSeq(tx.comps) 
      pl2co    <- Participant2Comp.decSeq(tx.pl2co) 
    } yield (tBase, comps, clubs, players, pl2co)) match {  
      case Left(err)  => Left(err)
      case Right(res) => {
        val tI = new Tourney(res._1)
        tI.comps   = collection.mutable.HashMap( res._2.map(p => p.id -> p) : _*)
        tI.clubs   = collection.mutable.HashMap( res._3.map(c => c.id -> c) : _*) 
        tI.players = collection.mutable.HashMap( res._4.map(p => p.id -> p) : _*)        
        tI.pl2co   = collection.mutable.HashMap( res._5.map(p => (p.sno,p.coId) -> p) : _*)

        // create inverse hashtables and maxId entries
        for(club <- tI.clubs.values) {   
          tI.clName2id(club.name) = club.id.toInt
          if (club.id > tI.clubIdMax) tI.clubIdMax = club.id.toInt
        }
        //logger.info(s"load(${tony.id}) step 4")
        for(pl <- tI.players.values) {   
          tI.plNCY2id((pl.lastname, pl.firstname,pl.clubName,pl.birthyear)) = pl.id.toInt
          if (pl.id > tI.playerIdMax) tI.playerIdMax = pl.id.toInt
        }
        //logger.info(s"load(${tony.id}) step 5")
        for(comp <- tI.comps.values) {   
          tI.coName2id(comp.hash) = comp.id
          if (comp.id > tI.compIdMax) tI.compIdMax = comp.id
        }
        Right(tI)
      }
    }
  }  

}

case class TourneyTx(basis: String, comps :Seq[String], clubs :Seq[String], players :Seq[String], pl2co :Seq[String])
object TourneyTx {  implicit def rw: RW[TourneyTx] = macroRW }


/*
**
**  TOURNEY RUN - Tourney Runtime Information
**
*/
class TournRun(val id: Long) 
{
  /*
   * tourney runtime data
   */
  // map coId, secId -> Competition Section
  var coSects:    HashMap[(Long, Int), CompSection] = HashMap() 
  // map coId, coPh  -> Competition Phase
  var cophs:      HashMap[(Long, Int), CompPhase] = HashMap() 
  var playfields: HashMap[Int, Playfield]         = HashMap() 
  
  /*
   * methods for converting
   */
  def encode(): String = write[TournRunTx](toTx(id))

  def toTx(id: Long): TournRunTx = {
    val pfs     = for ((k,v) <- this.playfields) yield v.stringify
    val cphs    = for ((k,v) <- this.cophs)      yield v.toTx
    val cSects  = for ((k,v) <- this.coSects)    yield v.toTx
    //val cSecs   = for (v <- this.coSects)        yield v.toTx 
    TournRunTx(id, cSects.toList, cphs.toList, pfs.toList) 
  }

  override def toString(): String = {
    def pfsStr() = {
      val str = new StringBuilder("-- PLAYFIELDSS\n")
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

    s"""\nTOURNEY RUN INFORMATION[${id}]
       |  ${pfsStr()}
       |  ${cphsStr()}
       |  ${coSecStr()}
       |""".stripMargin('|')
  }
}


object TournRun {

  def decode(trnyStr: String): Either[Error, TournRun] = 
    try Right(fromTx(read[TournRunTx](trnyStr)))
    catch { case _: Throwable => Left(Error("err0071.decode.TournRun", trnyStr.take(20))) }

  def fromTx(tx: TournRunTx): TournRun = {
    val tR         = new TournRun(tx.id)
    val plfs       = tx.playfields.map(x => Playfield.decode(x)).partitionMap(identity)._2
    //tR.coSects     = tx.coSects.map(x => CompSection.fromTx(x)).to(ArrayBuffer) 
    tR.coSects     = collection.mutable.HashMap( tx.coSects.map(c => (c.coId, c.id)-> CompSection.fromTx(c) ) : _*) 
    tR.cophs       = collection.mutable.HashMap( tx.cophs.map(c => (c.coId,c.coPh)-> CompPhase.fromTx(c) ) : _*) 
    tR.playfields  = collection.mutable.HashMap( plfs.map(p => p.nr -> p) : _*) 
    
    tR
  }
}

case class TournRunTx(id: Long, coSects: Seq[CompSectionTx], cophs: Seq[CompPhaseTx], playfields: Seq[String])
object TournRunTx { implicit def rw: RW[TournRunTx] = macroRW }