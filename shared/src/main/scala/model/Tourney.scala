package shared.model

//import shared.model.gamesystem.Match
import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Map, HashMap }
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
  var typ:        TourneyTyp.Value, 
  var privat:     Boolean                               = true, 
  var contact:    Contact                               = Contact("","","",""), 
  var address:    Address                               = Address("","","","",""), 
  var players:    Map[Long, Player]                     = Map().withDefaultValue(Player(0L, 0L, "","","", 0, "", SexTyp.UNKN, "_")),
  var comps:      Map[Long, Competition]                = Map(),
  var clubs:      Map[Long, Club]                       = Map(),  // clubs map with key (id) 
  var pl2co:      Map[(String, Long), Pant2Comp]        = Map(),  // registered player in a competition key: (sno, coId)
  var cophs:      Map[(Long, Int), CompPhase]           = Map(),  // map (coId, coPhId)  -> Competition Phase
  var playfields: Map[Int, Playfield]                   = Map(),  // map (playfieldNo)   -> Playfield
  var licenses:   Map[String, String]                   = Map().withDefaultValue("")
)
{
  // inverse hashmaps for fast access to players, clubs, ...  
  var club2id:     Map[Int, Long]   = Map().withDefaultValue(0L)  // club hash -> id
  var player2id:   Map[Int, Long]   = Map().withDefaultValue(0L)  // player hash -> id
  var comp2id:     Map[Int, Long]   = Map().withDefaultValue(0L)  // competition hash -> id
  
  /*
   * tourney management data
   */
  var playerIdMax: Long = 0L
  var clubIdMax:   Long = 0L
  var compIdMax:   Long = 0L
  var accessTime:  Long = 0L
  var backupTime:  Long = 0L
  var writeTime:   Long = 0L  
  var curCoId:     Long = 0L 
 
  def encode(): String =
    write[(Int, TourneyBaseData, List[Player], List[Competition], List[Club], List[Pant2Comp], List[CompPhaseTx], List[Playfield], Map[String,String])]  ((
      Tourney.defaultEncodingVersion, 
      TourneyBaseData(id, name, organizer, orgDir, startDate, endDate, ident, typ, privat, contact, address),
      players.values.toList, 
      comps.values.toList, 
      clubs.values.toList,
      pl2co.values.toList, 
      cophs.values.map(x => x.toTx()).toList,
      playfields.values.toList,
      licenses    
    ))


  def toJson(ident: Int): String = write[Tourney](this, ident)
  def isDummy() = (id == 0L)
  def getToId() = this.id
  def getBase() = TournBase(name, organizer, orgDir, startDate, endDate, ident, typ.id, privat, contact.encode, address.encode, id)
  def getStartDate(lang: String, fmt:Int=0): String = int2date(startDate, lang, fmt)

   
  /*
   * miscellanous tourney routines
   */
  // get all competitions with id and name
  def getNamesComp(): Seq[(Long, String)] = {
    (for { (k,co)  <- comps } yield {
      (co.id, co.name, co.getAgeGroup, co.getRatingRemark, co.typ, co.startDate)
    }).toSeq.sortWith(_._5 > _._5).sortWith(_._6 < _._6).map(x => (x._1,x._2))
  }

  // get all players with id and name of the competitions
  def getNamesClubPlayer(coId: Long): Seq[(String,String)] = {
    val res = for { p2c  <- pl2co.values } yield { 
      if (p2c.coId == coId)  {
        comps(coId).typ match {
          case CompTyp.SINGLE => ( players(p2c.getPlayerId).getClub(), players(p2c.getPlayerId).getName() )
          case CompTyp.DOUBLE => {
            val (id1,id2) = p2c.getDoubleId
            ( s"${players(id1).getClub()}/${players(id2).getClub()}", 
              s"${players(id1).lastname}/${players(id2).lastname}" ) 
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
    } else if (comp2id.isDefinedAt(co.hash)) {
      // idempotent routine 
      Right(comps(comp2id(co.hash)))
    } else {
      // ok add new one
      val coIdMax = compIdMax + 1
      compIdMax = coIdMax
      comps(coIdMax) = co.copy(id = coIdMax)     
      comp2id(co.hash) = coIdMax
      Right(comps(coIdMax))
    }
  }

  /** setComp - sets new values of existing competitions
   *            or adds new one
   */
  def setComp(co: Competition): Either[Error, Competition] = {
    if (co.id == 0)  {
      Left(Error("err0154.trny.setComp")) 
    } else if (!co.validateDate(startDate, endDate)) {
      Left(Error("err0015.trny.compDate", co.startDate)) 
    } else if (comp2id.isDefinedAt(co.hash)) {   
      // hash value exists, if it belongs to different id
      // don't allow it
      val coId = comp2id(co.hash)
      if (coId != co.id) {
        // do not allow to change a competition to an existing
        Left(Error("err0016.trny.compExistsAlready"))
      } else {
        // set new competition and return it
        comps(co.id) = co
        Right(co)
      }
    } else {
      // ok changes on name, type, .... which result in hash changes
      // thats ok, caveat hash changes remove old hash and add new one!
      comp2id = comp2id.filter(x => x._2 != co.id) 
      comp2id(co.hash) = co.id
      comps(co.id) = co
      Right(comps(co.id))
    }
  }  

  /** prtComp
    * 
    *
    * @param coId
    * @return
    */
  def prtComp(coId: Long, fun:(String, Seq[String])=>String): String = {

    if (!comps.contains(coId)) {
      s"Competition coId ${coId} does not exist" 
    } else {
      val co = comps(coId)
      val str = new StringBuilder(s"COMPETITION(${co.id}): ${co.name} \n  typ: ${co.typ} status: ${co.status}\n")
      var first=true   
      for ((k,v) <- cophs) {
        if (k._1 == coId && first)  { 
          str.append(s"  CompPhases: ${v.name}(${k._2}) status: ${v.status} player: ${v.noPlayers}"); first = false 
        } else {
          str.append(s"              ${v.name}(${k._2}) status: ${v.getStatusTxt} player: ${v.noPlayers}")
        }
      }
      str.toString
    }
  }

  /** getCompCnt - returns number of registered and active participants  
   *
   */ 
  def getCompCnt(co: Competition): (Int, Int) = co.typ match {    
    case CompTyp.SINGLE | CompTyp.DOUBLE => {
      val pl2cos   = pl2co.values.filter(_.coId==co.id).toSeq
      val cnt      = pl2cos.length
      val cntActiv = pl2cos.filter(_.status > PantStatus.REGI).length
      (cnt, cntActiv)
    }
    case _ => (0, 0)
  }


  /** delComp - delete competition with id
   * 
   */ 
  def delComp(coId: Long):Either[Error, Boolean] = {
    if (coId == 0) {
      Left(Error("err0014.trny.compNotFound", coId.toString)) 
    } else if (comps.isDefinedAt(coId)) {
      val co = comps(coId)
      if (comp2id.isDefinedAt(co.hash)) comp2id.remove(co.hash)
      comps.remove(coId)        
      Right(true)
    } else {  
      Left(Error("err0014.trny.compNotFound", coId.toString))
    }
  }

  def setCompStatus(coId: Long, status: CompStatus.Value): Either[Error, Unit] = 
    if (comps.isDefinedAt(coId)) { comps(coId).status = status; Right({}) }
    else Left(Error("err0014.trny.compNotFound", coId.toString))
  
  def updateCompStatus(coId: Long): Either[Error, CompStatus.Value] = {
    if (!comps.isDefinedAt(coId)) Left(Error("err0014.trny.compNotFound", coId.toString)) else {
      val statusList = (cophs.filter( _._1._1 == coId).values.map { _.status }).toList
      val sizeMap = statusList.groupBy(identity).mapValues(_.size)
      val size = statusList.length
      val result = if (size == 0)                                                                  CompStatus.READY
        else if (sizeMap.isDefinedAt(CompPhaseStatus.FIN) && sizeMap(CompPhaseStatus.FIN) == size) CompStatus.FIN
        else if (sizeMap.isDefinedAt(CompPhaseStatus.EIN) && sizeMap(CompPhaseStatus.EIN) > 0)     CompStatus.RUN
        else if (sizeMap.isDefinedAt(CompPhaseStatus.AUS) && sizeMap(CompPhaseStatus.AUS) > 0)     CompStatus.RUN
        else if (sizeMap.isDefinedAt(CompPhaseStatus.CFG) && sizeMap(CompPhaseStatus.CFG) > 0)     CompStatus.CFG
        else                                                                                       CompStatus.UNKN
      comps(coId).status = result
      Right(result)
    }
  }


  def regSingle(coId: Long, pl: Player, pStatus: PantStatus.Value): Either[Error, SNO] =
    addPlayer(pl) match {
      case Left(err)     => Left(err)
      case Right(player) => {
        val p2c = Pant2Comp.single(player.id, coId, pStatus) 
        pl2co((p2c.sno, p2c.coId)) = p2c
        Right(SNO(p2c.sno))
      }
    }

  def regSingle(coId: Long, pList: List[Player], pStatus: PantStatus.Value): Either[Error, List[SNO]] = 
    (for { x <- pList } yield regSingle(coId, x, pStatus)).partitionMap(identity) match {
      case (Nil, rights) => Right(rights)
      case (lefts, _)    => Left(lefts.head)
    }
  
  def regDouble(coId: Long, pls: (Long,Long), pStatus: PantStatus.Value): Either[Error, SNO] = 
    if(players.contains(pls._1) && players.contains(pls._2)) {
      val p2c = Pant2Comp.double(pls._1, pls._2, coId, pStatus) 
      pl2co((p2c.sno, p2c.coId)) = p2c
      Right(SNO(p2c.sno))
    } else {
      Left(Error(""))
    }


  def regDouble(coId: Long, ppList: List[(Long,Long)], pStatus: PantStatus.Value): Either[Error, List[SNO]] = 
    (for { x <- ppList } yield regDouble(coId, x, pStatus)).partitionMap(identity) match {
      case (Nil, rights) => Right(rights)
      case (lefts, _)    => Left(lefts.head)
    }

  /** addPlayer - adds new player if it does not exist already
   */
  def addPlayer(pl: Player): Either[Error, Player] =
    if (pl.id != 0) {
      Left(Error("err0155.svc.addPlayer", pl.id.toString))
    } else {
      // check whether an other player exists with same name ....
      val id = player2id.getOrElse(pl.hash, 0L)
      if (id > 0) {
        // same player exists => idempotent add 
        Right(players(id))
      } else {
        val club = addClub(pl.clubName)
        // critical path (lock?)
        val newId = playerIdMax + 1
        playerIdMax = newId
        player2id(pl.hash) = newId
        players(newId) = pl.copy(id = newId, clubId = club.id )
        Right(players(newId))
      }
    }


  /** setPlayer updates existing player 
   *  if necessary creates new club entry
   */
  def setPlayer(pl: Player): Either[Error, Player] =
    if (pl.id == 0) Left(Error("err0157.svc.setPlayer", pl.getName())) else {
      val plId = player2id(pl.hash)
      if (pl.hasLicense) {
        //allow only change of email!
        if (pl.copy(email="") != players(pl.id).copy(email="") ) {
          Left(Error("err0216.svc.setPlayer.invalidUpdate", pl.getName()))
        } else {
          players(pl.id).email = pl.email
          Right(players(pl.id))
        }
      } else if (plId == pl.id) {
        // change with same hash entry
        players(pl.id) = pl
        Right(players(pl.id))
      } else if (plId == 0) {
        // change with new hash entry
        // add clubname (idempotent)
        val club = addClub(pl.clubName)

        // remove old hash entry, change require new one
        player2id = player2id.filter(x => x._2 != pl.id) 
        player2id += (pl.hash -> pl.id)

        players(pl.id) = pl.copy(clubId = club.id)
        Right(players(pl.id))
      } else {
        // change with hash collision - not allowed
        Left(Error("err0158.svc.setPlayer", pl.getName()))
      }
    }


  def setPlayer(plId: Long, license: CttLicense): Either[shared.utils.Error, Player] = 
    if (!players.contains(plId)) Left(Error("err0212.svc.setPlayer.invalidPlayerId ", plId.toString)) 
    else if (license.value != "" && !licenses.contains(license.value)) Left(Error("err0213.svc.setPlayer.invalidLicense", plId.toString, license.value)) 
    else {
      val cttLicInfo = CttPersonCsv(licenses.getOrElse(license.value, ""))  
      // println(s"setPlayer: cttLicInfo->${cttLicInfo} license.value->${license.value}")
      cttLicInfo.get match {
        case Left(err)  => players(plId).delLicense
        case Right(ctp) => players(plId) = ctp.toPlayer(plId, addClub(ctp.clubName).id)
      }
      Right(players(plId))
    }

  def getPlayerRatingRange() = {
    typ match {
      case TourneyTyp.TT   => (100, 5000)
      case TourneyTyp.SK   => (0, 10000) 
      case TourneyTyp.ANY  => (0, 5000)
      case TourneyTyp.UNKN => (0, 5000)
    }       
  }


  /** setClub - adds new Club or merges to existing
   */
  def setClub(cl: Club, merge: Boolean = false): Either[Error, Club] = {
    val clId = club2id.getOrElse(cl.hash, 0L)
    if (clId == 0) {
      // no existing club with that name  
      if (cl.id == 0) {
        // new club not existing so far   
        Right(addClub(cl.name))
      } else {
        // name changed so far not cached 
        // keep clId, remove invalid hash value
        //club2id = club2id.filter(x=>x._2 != cl.id) 
        club2id(cl.hash) = cl.id
        clubs(cl.id) = cl
        Right(clubs(cl.id))       
      }
    } else {
      // club with this name already exists
      if (cl.id == 0) {
        // add existing club again (idempotent)
        clubs(clId) = cl.copy(id=clId)
        Right(clubs(clId))
      } else {
      // update existing club
        if (cl.id == clId) {
          clubs(clId) = cl
          Right(clubs(clId))
        } else {
          // there is an existing club with a different id
          // a name of an existing club was changed to existing
          // name, either error or merge
          // cl.id != clId AND cl.id != 0 AND clId != 0
          if (merge) {
            club2id = club2id.filter(x=>x._2 != cl.id) 
            clubs(clId) = cl.copy(id=clId)
            Right(clubs(clId))
          } else {
            // error
            Left(Error("err0187.trny.setClub"))
          }
        }
      }
    }
  }  

  def addClub(name: String): Club = {
    val clId = club2id.getOrElse(name.hashCode(), 0L)
    if (clId == 0) {
      // new club not existing so far   
      val nclId = clubIdMax + 1
      clubIdMax = nclId
      club2id(name.hashCode()) = nclId
      clubs(nclId) = Club(nclId, name, "") 
      clubs(nclId)
    } else {
      clubs(clId)
    }  
  }  

  //
  // PARTICIPANT ROUTINES
  //

  def setPantStatus(coId: Long, sno: SNO, status: PantStatus.Value): Either[Error, PantStatus.Value] = {
    if ( pl2co.isDefinedAt((sno.value, coId)) ) { 
      pl2co((sno.value,coId)).status = status
      Right(status)
    } else { 
      Left(Error("err0027.tourney.setParticipantStatus", sno.value, coId.toString)) 
    }      
  }

  def setPantBulkStatus(coId: Long, pantStatus: List[(String, PantStatus.Value)]):Either[Error, Int] = 
    seqEither(for (p <- pantStatus) yield setPantStatus(coId, SNO(p._1), p._2)) match {
      case Left(err)  => Left(err)
      case Right(res) => Right(pantStatus.filter(_._2>=PantStatus.REDY).length)
    }

  def getPant(coId: Long, sno: SNO): Pant = sno.getPant(coId)(this)

  def getPant(coTyp: CompTyp.Value, sno: SNO): Pant = coTyp match {
    case CompTyp.SINGLE => getSinglePlayer(sno) match {
      case Left(err)       => Pant(sno.value, "", "", 0, "", (0,0)) 
      case Right(p)        => Pant(sno.value, p.getName(), p.clubName, p.getRating, "", (0,0))
    }
    case CompTyp.DOUBLE => getDoublePlayers(sno) match {
      case Left(err)       => Pant(sno.value, "", "", 0, "", (0,0)) 
      case Right((p1, p2)) => Pant(sno.value, p1.getDoubleName(p2), p1.getDoubleClub(p2), p1.getDoubleRating(p2), "", (0,0))
    }
    case _                 => Pant(sno.value, "", "", 0, "", (0,0))    
  }

  def getSinglePlayer(sno: SNO): Either[Error, Player] = 
    try Right(players(sno.value.toLong))
    catch { case _: Throwable => Left(Error("err0173.trny.getSinglePlayer", sno.value)) }
 
  def getDoublePlayers(sno: SNO): Either[Error, (Player, Player)] = 
    try { val ids = getMDLongArr(sno.value); Right( (players(ids(0)), players(ids(1))) ) }  
    catch { case _: Throwable => Left(Error("err0174.trny.getDoublePlayer", sno.value)) }


  //
  // Competition Phase Routines
  //

  /** getCoPhStatus returns status of the competition phase 
   *  otherwise undefined status
   */ 
  def getCoPhStatus(coId: Long, coPhId: Int): CompPhaseStatus.Value = 
    if (cophs.isDefinedAt((coId,coPhId))) { cophs((coId,coPhId)).status } else { CompPhaseStatus.UNKN } 


  def setCompPhase(coph: CompPhase): Either[Error, Unit] = 
    if (coph.coId == 0 || coph.coPhId == 0 || !cophs.contains((coph.coId, coph.coPhId))) {
      Left(Error("err0242.setCompPhase", coph.coId.toString, coph.coPhId.toString))
    } else {
      Right(cophs((coph.coId, coph.coPhId)) = coph)
    }

  def addCompPhase(coId: Long, name: String, preCoPhId:Option[Int] = None): Either[Error, CompPhase] = { 
    val coPhIds = cophs.filter(x => x._1._1 == coId).values.map(x => x.coPhId).toList
    val coPhNames = cophs.filter(x => x._1._1 == coId).values.map(x => x.name).toList
    val coPhId = if (coPhIds.isEmpty) 1 else coPhIds.max + 1
    if (coPhNames.contains(name)) {
      Left(Error("err0234.coph.already.exists", name))
    } else {
      val coph = CompPhase(name, coId, coPhId, CompPhaseCfg.CFG, CompPhaseStatus.CFG, false, 0, 0, 0, preCoPhId)
      cophs((coId, coph.coPhId)) = coph
      println(s"addCompPhase ${cophs.mkString(":")}")
      Right(coph)
    }
  }

  def getCoPh(coId: Long, coPhId: Int): Either[Error, CompPhase] = 
    if ( coId==0 || coPhId == 0 | !cophs.isDefinedAt((coId,coPhId))) {
      println(s"ERROR: getCoPh(${coId},${coPhId}) doesn't exist")
      Left(Error("err0237.getCoPh.notFound", coId.toString, coPhId.toString))
    } else {
      Right(cophs((coId,coPhId))) 
    }  


  def getCoPhList(coId: Long, coPhId: Int): ArrayBuffer[CompPhase] = 
    if (!cophs.isDefinedAt((coId, coPhId))) ArrayBuffer[CompPhase]() else cophs((coId,coPhId)).baseCoPhId match {
      case None           => ArrayBuffer[CompPhase]()
      case Some(bCoPhId)  => if ((bCoPhId != 0) && cophs.isDefinedAt((coId, bCoPhId))) {
        ArrayBuffer(cophs((coId, bCoPhId))) ++ getCoPhList(coId, bCoPhId)
      } else { ArrayBuffer[CompPhase]() }
    }

  def delCompPhase(coId: Long, coPhId: Int) = if (cophs.isDefinedAt((coId, coPhId))) cophs.remove((coId, coPhId))

  def delCompPhases(coId: Long, coPhIds: List[Int]) = coPhIds.foreach { coPhId => if (cophs.isDefinedAt((coId, coPhId))) cophs.remove((coId, coPhId)) }

  def getCompPhaseFollowing(coId: Long, coPhId: Int): List[Int] = {
    var result = scala.collection.mutable.ListBuffer[Int]()
    if (coPhId != 0 & coId != 0) {
      if (cophs.isDefinedAt((coId, coPhId*2)))   result.append(coPhId*2) ++ getCompPhaseFollowing(coId, coPhId*2)
      if (cophs.isDefinedAt((coId, coPhId*2+1))) result.append(coPhId*2+1) ++ getCompPhaseFollowing(coId, coPhId*2+1) 
    }
    result.toList
  }  

  def getCompPhaseNames(coId: Long, coPhIds: List[Int]): List[String] = {
    for (coPhId <- coPhIds) yield { cophs((coId, coPhId)).name }  
  } 

  def getCompPhaseName(coId: Long, coPhId: Int): String = {
    if (cophs.isDefinedAt((coId, coPhId))) { cophs((coId, coPhId)).name }
    else { println(s"ERROR: getCompPhaseName(${coId},${coPhId}) doesn't exist");  "" }
  }

  def getCompPhaseMatches(coId: Long, coPhId: Int): String = {
    val sno2clickTTId = pl2co.filter( _._1._2 == coId).map (elt => elt._2.sno -> elt._2.ident)
    if (cophs.isDefinedAt((coId, coPhId))) cophs((coId, coPhId)).getMatchesXML(sno2clickTTId) else ""
  }

  def getCompName(coId: Long=0L, fmt:Int=0) = {
    val effCoId = if (coId == 0L) curCoId else coId
    //println(s"getCompName ${effCoId} ${coId}  ${fmt}")
    fmt match {
      case 1 => if (comps.isDefinedAt(effCoId) && effCoId != 0L) s"[${comps(effCoId).name}]" else ""
      case _ => if (comps.isDefinedAt(effCoId) && effCoId != 0L) comps(effCoId).name else ""
    }
  } 

  //def getCompName(coId: Long=0L, fmt:Int=0) = "ffffffff"


  def getCompMatches(coId: Long) = {
    val result = new StringBuilder("<matches>")
    cophs.filter(_._1._1==coId).foreach { elem => result.append(getCompPhaseMatches(elem._1._1, elem._1._2)) }
    result.append("</matches>")
    result.toString()
  }

  //
  //  Mgmt Routines
  // 
  /* getCurCoId - take stored value when available, otherwise select a good choice: 
  **              competition with running phases/rounds
  **              first configured competition
  */
  def getCurCoId: Long = { 
    if (curCoId == 0L) {
      val cophStarted = cophs.keys.filter(x => x._2 == 1).toList.sortBy(_._1) 
      if (cophStarted.length >= 1) {
        setCurCoId(cophStarted.head._1)
        if (comps(curCoId).getCurCoPhId == 0) comps(curCoId).setCurCoPhId(1)
      } else {
        if (comps.size > 0) setCurCoId(comps.head._1)
      }
    }
    curCoId
  } 

  def setCurCoId(value: Long) = { curCoId = value}
  def getCurCoPhId: Int = {
    if (comps.isDefinedAt(curCoId) && curCoId !=0 ) {
      val curCoPhId = comps(curCoId).getCurCoPhId
      if (curCoPhId != 0) curCoPhId else { if (cophs.isDefinedAt((curCoId,1))) { comps(curCoId).setCurCoPhId(1); 1 } else 0 } 
    } else 0
  }


  //
  // print readable tourney - for debug purposes
  //
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
      |  ${pfsStr()}
      |""".stripMargin('|')
  } 
}


object Tourney {
  implicit val tourneyTypReadWrite: upickle.default.ReadWriter[TourneyTyp.Value] =
    upickle.default.readwriter[Int].bimap[TourneyTyp.Value](x => x.id, TourneyTyp(_))

  implicit def rw: RW[Tourney] = macroRW
  def init                   = Tourney(0L, "", "", "dummy", 19700101, 19700101, "", TourneyTyp.UNKN)
  def init(tBase: TournBase) = Tourney(tBase.id, tBase.name, tBase.organizer, tBase.orgDir, tBase.startDate, tBase.endDate, tBase.ident, TourneyTyp(tBase.typ))

  val defaultEncodingVersion: Int = 2

  def decode(trnyStr: String): Either[Error, Tourney] = {
    val trnyStrStart = trnyStr.take(8)
    val start = trnyStrStart.indexOf("[")
    val end = trnyStrStart.indexOf(",")
    val version = if (start >=0 & end >= 2 & start<end) trnyStrStart.slice(start+1,end).trim.toIntOption.getOrElse(0) else 0
    //println(s"Tourney.decode => version: ${version}" )
    version match {
      case 1 => decodeV1(trnyStr)
      case _ => decodeDefault(trnyStr) // val defaultEncodingVersion: Int = 2
    }
  }


  def decodeV1(trnyStr: String): Either[Error, Tourney] = try {
    val (version, tBD, players, comps, clubs, pl2co, cophTx, playfields, lics) =
      read[(Int, TourneyBaseData, List[Player], List[Competition], List[Club], List[Pant2Comp], List[CompPhaseTx1], List[Playfield], Map[String, String])](trnyStr)

    val trny = Tourney(tBD.id, tBD.name, tBD.organizer, tBD.orgDir, tBD.startDate, tBD.endDate, 
                        tBD.ident, tBD.typ, tBD.privat, tBD.contact, tBD.address)

    trny.players = collection.mutable.Map(players.map(x => (x.id, x)): _*)
    trny.comps = collection.mutable.Map(comps.map(x => (x.id, x)): _*)
    trny.clubs = collection.mutable.Map(clubs.map(x => (x.id, x)): _*)
    trny.pl2co = collection.mutable.Map(pl2co.map(x => ((x.sno, x.coId), x)): _*)
    trny.cophs = collection.mutable.Map(cophTx.map(x => CompPhase.fromTx1(x)).map(coph => ((coph.coId, coph.coPhId),coph)): _*)
    trny.playfields = collection.mutable.Map(playfields.map(x => (x.nr, x)): _*)
    trny.licenses = lics

    // for(license <- licenses) { 
    //   val key = getMDStr(license, 0)
    //   if (!trny.licenses.contains(key)) trny.licenses(key) = license
    // }

    trny.club2id   = Map().withDefaultValue(0L)  // club hash -> id
    trny.player2id = Map().withDefaultValue(0L)  // player hash -> id
    trny.comp2id   = Map().withDefaultValue(0L)  // competition hash -> id
    // create inverse hashtables and maxId entries
    for(club <- trny.clubs.values) {   
      trny.club2id(club.hash) = club.id
      if (club.id > trny.clubIdMax) trny.clubIdMax = club.id.toInt
    }
    for(pl <- trny.players.values) {   
      trny.player2id(pl.hash) = pl.id
      if (pl.id > trny.playerIdMax) trny.playerIdMax = pl.id
    }
    for(comp <- trny.comps.values) {   
      trny.comp2id(comp.hash) = comp.id
      if (comp.id > trny.compIdMax) trny.compIdMax = comp.id
    }   
    Right(trny)
  }
  catch { case _: Throwable => Left( Error("err0070.decode.Tourney", trnyStr.take(20), "", "Tourney.decode") ) }


  def decodeDefault(trnyStr: String): Either[Error, Tourney] = { 
    var decodeStep = 0
    try {
      val (version, tBD, players, comps, clubs, pl2co, cophTx, playfields, lics) =
        read[(Int, TourneyBaseData, List[Player], List[Competition], List[Club], List[Pant2Comp], List[CompPhaseTx], List[Playfield], Map[String, String])](trnyStr)

      decodeStep = 1  
      val trny = Tourney(tBD.id, tBD.name, tBD.organizer, tBD.orgDir, tBD.startDate, tBD.endDate, 
                          tBD.ident, tBD.typ, tBD.privat, tBD.contact, tBD.address)


      trny.players = collection.mutable.Map(players.map(x => (x.id, x)): _*);        decodeStep += 1  
      trny.comps = collection.mutable.Map(comps.map(x => (x.id, x)): _*);            decodeStep += 1
      trny.clubs = collection.mutable.Map(clubs.map(x => (x.id, x)): _*);            decodeStep += 1
      trny.pl2co = collection.mutable.Map(pl2co.map(x => ((x.sno, x.coId), x)): _*); decodeStep += 1
      trny.cophs = collection.mutable.Map(cophTx.map(x => CompPhase.fromTx(x) ).map(coph => ((coph.coId, coph.coPhId),coph)): _*); decodeStep += 1
      trny.playfields = collection.mutable.Map(playfields.map(x => (x.nr, x)): _*);  decodeStep += 1
      trny.licenses = lics

      trny.club2id   = Map().withDefaultValue(0L)  // club hash -> id
      trny.player2id = Map().withDefaultValue(0L)  // player hash -> id
      trny.comp2id   = Map().withDefaultValue(0L)  // competition hash -> id
      // create inverse hashtables and maxId entries
      for(club <- trny.clubs.values) {   
        trny.club2id(club.hash) = club.id
        if (club.id > trny.clubIdMax) trny.clubIdMax = club.id.toInt
      }
      decodeStep += 1

      for(pl <- trny.players.values) {   
        trny.player2id(pl.hash) = pl.id
        if (pl.id > trny.playerIdMax) trny.playerIdMax = pl.id
      }
      decodeStep += 1
      for(comp <- trny.comps.values) {   
        trny.comp2id(comp.hash) = comp.id
        if (comp.id > trny.compIdMax) trny.compIdMax = comp.id
      }   
      Right(trny)
    }
    catch { case _: Throwable => Left( Error("err0070.decode.Tourney", decodeStep.toString, trnyStr.take(20), "Tourney.decode") ) }
  }

}


object TourneyTyp extends Enumeration {
  val UNKN = Value(0,  "UNKN")
  val TT   = Value(1,  "Tabletennis")  // table tennis
  val SK   = Value(2,  "Shephead")     // Schafkopf
  val ANY  = Value(99, "ANY")          // waiting list
}

