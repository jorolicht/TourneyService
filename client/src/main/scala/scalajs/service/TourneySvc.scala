package scalajs.service

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent._
import scala.util.{ Either, Success, Failure}

import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.concurrent.duration._

import scala.scalajs._
import scala.scalajs.js.URIUtils

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

import shared.model.tabletennis.ResultEntry
import shared.utils._
import shared.utils.Routines._
import shared.model._
import shared.model.Competition._
import scalajs.usecase.Helper
import scalajs._
import org.xml.sax.ErrorHandler

trait TourneySvc extends WrapperSvc 
  with AppHelperSvc
{
  
  // export database
  def ping(toId: Long, msg: String): Future[Either[Error, String]] = getAction("ping", toId, s"msg=${msg}")

  // export database
  def expDatabase(toId: Long, club: String, date: Int, etyp: String): Future[Either[Error, String]] = 
    getAction("expDatabase", toId, s"club=${club}&date=${date.toString}&etyp=${etyp}")

  // getContent - either error or file content
  def getContent(path: String): Future[Either[Error, String]] = {
    val absPath = if (path.startsWith("/")) path else s"/${path}"
    Helper.info("getContent", s"path: ${absPath}")

    Ajax.get(absPath).map(_.responseText)
      .map(content => Right(content))
      .recover({
        case dom.ext.AjaxException(req) => Left( 
          Error(" err0034.ajax.getRequest", s"response: ${req.responseText.take(20)} status: ${req.statusText}", absPath, "getContent") 
        )
        case _: Throwable => Left( Error("err0033.svc.getContent", absPath, "", "getContent")) 
      })
  }


  // getCfgFile - either error or file content
  def getCfgFile(orgDir: String, startDate: Integer, uldType: String): Future[Either[Error, String]] = {
    val path = s"/service/getCfgFile?orgDir=${orgDir}&startDate=${startDate}&fileType=${uldType}" 
    //Helper.info("getCfgFile", s"path: ${path}")
    Ajax.get(path).map(_.responseText)
      .map(content => Right(content))
      .recover({
        case dom.ext.AjaxException(req) => Left( 
          Error(" err0034.ajax.getRequest ", s"response: ${req.responseText.take(20)} status: ${req.statusText}", path, "getCfgFile") 
        )
        case _: Throwable               => Left(Error("err0035.svc.getCfgFile", path, "", "getCfgFile")) 
    })
  }

  
  // provide certificate template for printout (deprecated)
  def provideCertTemplFile(orgDir: String): Future[Boolean] = {
    postJson("/service/provideCertTemplFile", s"orgDir=${orgDir}").map {
      case Left(err)  => false
      case Right(res) => true
    }
  }

  def genCertFile(orgDir: String, coId: Long, sno: String, certHtml: String): Future[Boolean] = {
    postJson("/service/genCertFile", s"orgDir=${orgDir}&toId=${App.tourney.id}&coId=${coId}&sno=${sno}", certHtml).map {
      case Left(err)  => false
      case Right(res) => true
    }
  }

  /** printCert - print a certificate for a user of a competition
   *
   * @param tourney
   * @param sno - start number of user
   * @param coId - competition identifier
   */
  def printCert(tourney: Tourney, sno: String, coId: Long): Unit = {
    import shared.utils.Constants._
    val coName     = tourney.comps(coId).name
    val place      = tourney.pl2co((sno,coId)).getPlaceDesc(AppEnv.getMessage _)
    val orgDir     = tourney.orgDir
    val locdate    = AppEnv.getMessage("certificate.locationdate", tourney.address.city, int2date(tourney.startDate, AppEnv.lang))

    val (plName, clName) = tourney.comps(coId).typ match {
      case CT_SINGLE => {
        val plId = tourney.pl2co((sno,coId)).getPlayerId
        (s"${tourney.players(plId).firstname} ${tourney.players(plId).lastname}", s"${tourney.players(plId).clubName}")
      }

      case CT_DOUBLE | CT_MIXED => {
        val plId1 = tourney.pl2co((sno,coId)).getPlayerId1
        val plId2 = tourney.pl2co((sno,coId)).getPlayerId2
        //debug("printCert DOUBLE", s"${plId1} ${plId2}")
        (s"${tourney.players(plId1).lastname}/${tourney.players(plId2).lastname}", s"${tourney.players(plId1).clubName}/${tourney.players(plId2).clubName}")
      }
      case _ => ("?","?")
    }

    val ctext = clientviews.info.html.CertificatePage(
      orgDir, AppEnv.getMessage("certificate.title", plName), tourney.name, coName, place, plName, clName, locdate, AppEnv.lang
    ).toString

    genCertFile(orgDir, coId, sno, ctext).map { x => {
       dom.window.location.href = s"/content/clubs/${orgDir}/certs/Certificate_${tourney.id}_${coId}_${sno}.html"

       //Home.link(s"/content/clubs/${orgDir}/certs/Certificate_${getToId}_${coId}_${sno}.html")
       Helper.debug("genCertFile", s"for coId: ${coId} sno: ${sno}")
    }} 
  }  


  def downloadFile(dType: Int): Unit = {
    val path = s"/service/downloadFile?toId=${App.tourney.id}&dType=${dType}"
    Helper.debug("downloadFile", s"path: ${path}")
    Ajax.get(path).map( _.getAllResponseHeaders).map( content => Helper.debug("downloadFile", s"${content.take(20)}"))
  }

  def uploadFile(formData: dom.FormData): Future[Either[Error, Boolean]] = 
    postForm("/service/uploadFile", formData).map {
      case Left(err)  => Left(err.add("uploadFile"))
      case Right(res) => Return.decode2Boolean(res, "uploadFile")
    }

  // file exists
  def fileExists(filePath: Array[String]): Future[Either[Error, Boolean]] = {
    val fPath = filePath.mkString(":") 
    getAction("fileExists", App.tourney.id, s"filePath=${fPath}" ).map {
      case Left(err)  => Left(err.add("fileExists"))
      case Right(res) => Return.decode2Boolean(res, "fileExists")
    }
  }  


  // 
  // TOURNEY Interface
  // 

  // add tourney return tourney identifier
  def addTourney(trny: Tourney): Future[Either[Error, Long]] =
    postAction("addTourney", App.tourney.id, "", trny.encode()).map {
      case Left(err)     => Left(err.add("addTourney"))
      case Right(result) => Return.decode2Long(result, "addTourney")
    }

  // add tourney base return complete tourney (with identifier)
  def addTournBase(tb: TournBase): Future[Either[Error, Tourney]] =
    postAction("addTournBase", App.tourney.id, "", tb.encode()).map {
      case Left(err)     => Left(err.add("addTournBase"))
      case Right(trnyTx) => Tourney.decode(trnyTx)
    }


  // delete tourney basis information
  def delTourney(toId: Long): Future[Either[Error, Boolean]] = 
    postAction("delTourney", toId, "", "", true).map {
      case Left(err)  => Left(err.add("delTourney"))
      case Right(res) => Return.decode2Boolean(res, "delTourney")
    }

  // update tourney basis information
  def setTournBase(tournbase: TournBase): Future[Either[Error, Tourney]] =
    postAction("setTournBase", App.tourney.id, "", tournbase.encode()).map {
      case Left(err)     => Left(err.add("setTournBase"))
      case Right(trnyTx) => Tourney.decode(trnyTx)
    }

  // add tourney based on with click TT configuration 
  def addTournCTT(cttData: String, sDate: Int, eDate: Int): Future[Either[Error, Tourney]] = {
    Helper.debug("addTournCTT", s"${cttData.take(20)}")
    postAction("addTournCTT", App.tourney.id, s"sDate=${sDate}&eDate=${eDate}", cttData).map {
      case Left(err)     => Left(err.add("addTournCTT"))
      case Right(trnyTx) => Tourney.decode(trnyTx)
    }
  }  


  // save tourney to disc on server side (no sync)
  def saveTourney(toId: Long): Future[Either[Error, Boolean]] =
    postAction("saveTourney", toId, "", "").map {
      case Left(err)  => Left(err.add("saveTourney"))
      case Right(res) => Return.decode2Boolean(res, "saveTourney")
    }

  // copy loal tourney to server and save to disc
  def syncTourney(toId: Long): Future[Either[Error, Boolean]] =
    postAction("syncTourney", toId, "", App.tourney.encode()).map {
      case Left(err)  => Left(err.add("syncTourney"))
      case Right(res) => Return.decode2Boolean(res, "syncTourney")
    }


  // get full tourney configuration information
  def getTourney(toId: Long): Future[Either[Error, Tourney]] = {
    // println(s"getSubResult1: ${scala.scalajs.js.Date.now()}")
    getAction("getTourney", toId).map {
      case Left(err)     => Left(err.add("getTourney"))
      case Right(trnyTx) => Tourney.decode(trnyTx)
    }
  }


  // get all players of tourney 
  def getTournPlayers(toId: Long): Future[Either[Error, Seq[Player]]] = 
    getAction("getTournPlayers", toId).map { 
      case Left(err)      => Left(err.add("getTournPlayers"))
      case Right(players) => Player.decSeq(players)
    }


  // get all clubs of tourney
  def getTournClubs(toId: Long):  Future[Either[Error, Seq[Club]]] = 
    getAction("getTournClubs", toId).map { 
      case Left(err)    => Left(err.add("getTournClubs"))
      case Right(clubs) => Club.decSeq(clubs)
    }



  // find tourney basis informations by Name, Year and Type
  def findTournBases(search: String, toTyp: Int=0, toYear: Int=0): Future[Either[Error, Seq[TournBase]]] = 
    getAction("findTournBases", App.tourney.id, s"search=${search}&toTyp=${toTyp.toString}&toYear=${toYear.toString}").map { 
      case Left(err)     => Left(err.add("findTournBases"))
      case Right(tBases) => TournBase.decSeq(tBases)
    }


  // get all tourney basis informations
  def getTournBases(orgDir: String):  Future[Either[Error, Seq[TournBase]]] = 
    getAction("getTournBases", App.tourney.id, s"orgDir=${orgDir}").map { 
      case Left(err)  => Left(err.add("getTournBases"))
      case Right(tBs) => TournBase.decSeq(tBs)
    }


  // get tourney basis information
  def getTournBase(toId: Long):  Future[Either[Error, TournBase]] = 
    getAction("getTournBase", toId).map { 
      case Left(err) => Left(err.add("getTournBase"))
      case Right(tB) => TournBase.decode(tB)
    }


  // 
  // COMPETITION Interface
  // 

  // set competition status
  def setCompStatus(coId: Long, status: Int): Future[Either[Error, Boolean]] = 
    postAction("setCompStatus", App.tourney.id, s"coId=${coId}&status=${status.toInt}", "", true).map { 
      case Left(err)  => Left(err.add("setCompStatus"))
      case Right(res) => Return.decode2Boolean(res, "setCompStatus")
    }


  // set whole competition
  def setComp(co: Competition): Future[Either[Error, Competition]] =
    postAction("setComp", App.tourney.id, "", co.encode, true).map { 
      case Left(err)   => Left(err.add("setComp"))
      case Right(coTx) => Competition.decode(coTx)
    }

  // set whole competition
  def addComp(co: Competition): Future[Either[Error, Competition]] =
    postAction("addComp", App.tourney.id, "", co.encode, true).map { 
      case Left(err)   => Left(err.add("addComp"))
      case Right(coTx) => Competition.decode(coTx)
    }


  // get competitions based on tourney Id
  def getComps(toId: Long): Future[Either[Error, Seq[Competition]]] = 
    getAction("getComps", toId).map { 
      case Left(err)    => Left(err.add("getComps"))
      case Right(comps) => Competition.decSeq(comps)
    }


  // get a competition based on tourney Id and competition Id
  def getComp(coId: Long): Future[Either[Error, Competition]] = 
    getAction("getComp", App.tourney.id, s"coId=${coId}").map { 
      case Left(err)  => Left(err.add("getComp"))
      case Right(res) => Competition.decode(res)
    }


  // set whole competition
  def delComp(coId: Long): Future[Either[Error, Boolean]] = {
    postAction("delComp", App.tourney.id, s"coId=${coId}", "", true).map {
      case Left(err)  => Left(err.add("delComp"))
      case Right(res) => Return.decode2Boolean(res, "delComp")
    }
  }


  //
  // Register Interface (Single/Double)
  //
  /* regSingle register single player */  
  def regSingle(coId: Long, pl: Player, status: Int): Future[Either[Error, Long]] = 
    postAction("regSingle", App.tourney.id, s"coId=${coId}&status=${status}", s"player=${enc(pl.encode)}", true).map {
      case Left(err)     => Left(err.add("regSingle"))
      case Right(result) => Return.decode2Long(result, "reqSingle")
    }

  /* regDouble register double player */  
  def regDouble(coId: Long, pl1: Player, pl2: Player): Future[Either[Error, (Long, Long)]] = 
    postAction("regDouble", App.tourney.id, s"coId=${coId}", s"player1=${enc(pl1.encode)}&player2=${enc(pl2.encode)}", true).map {
      case Left(err)     => Left(err.add("regDouble"))
      case Right(result) => {
        try Right( read[(Long, Long)](result) )  
        catch { case _:Throwable => Left(Error("err0057.call.regDouble", result.take(10),"","regDouble")) }
      }
    }


  //
  // Participant Interface (participant could be Single,Double or Team (future) 
  // 
  //? def setParticipant2Comp(p2c: Participant2Comp)(implicit tse :TournSVCEnv):Future[Either[Error, Participant2Comp]]
  //? def setParticipant2Comps(p2cs: Seq[Participant2Comp])(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  //? def delParticipant2Comps(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  // def getParticipant2Comps(toId: Long): Future[Either[Error, Participant2Comps]]
  // def getParticipantPlace(toId: Long, coId: Long, sno: String): Future[Either[Error, String]]
  // def setParticipantPlace(coId: Long, sno: String, place: String)(implicit tse :TournSVCEnv): Future[Either[Error, Placement]]
  // def setParticipantStatus(coId: Long, sno: String, status: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]

  // get tourney player 2 comp 
  def getParticipant2Comps(toId: Long):  Future[Either[Error, Seq[Participant2Comp]]] =
    getAction("getParticipant2Comps", toId).map { 
      case Left(err)     => Left(err.add("getParticipant2Comps"))
      case Right(pa2cos) => Participant2Comp.decSeq(pa2cos)
    }

  // get the player placement
  def getParticipantPlace(coId: Long, sno: String): Future[Either[Error, Placement]] =
    getAction("getParticipantPlace", App.tourney.id, s"coId=${coId}&sno=${sno}").map {
      case Left(err)    => Left(err.add("getParticipantPlace"))
      case Right(place) => Placement.decode(place)
    }

  // set participant to competition
  // setParticipant2Comp(p2c: Participant2Comp): Future[Either[Error, Participant2Comp]]
  def setParticipant2Comp(p2c: Participant2Comp): Future[Either[Error, Participant2Comp]] = {
    postAction("setParticipant2Comp", App.tourney.id, "", p2c.encode, true).map {
      case Left(err)  => Left(err.add("setParticipant2Comp"))
      case Right(res) => Participant2Comp.decode(res)
    } 
  }   

  // delete participant entry
  def delParticipant2Comp(coId: Long, sno: String): Future[Either[Error, Int]] = {
    postAction("delParticipant2Comp", App.tourney.id, s"coId=${coId}&sno=${sno}", "", true) map {
      case Left(err)  => Left(err.add("delParticipant2Comp"))
      case Right(res) => Return.decode2Int(res, "delParticipant2Comp")
    } 
  }     

  // set the player placement
  // setParticipantPlace(coId: Long, sno: String, place: String): Future[Either[Error, Placement]]
  def setParticipantPlace(coId: Long, sno: String, place: Placement): Future[Either[Error, Placement]] = 
    postAction("setParticipantPlace", App.tourney.id, s"coId=${coId}&sno=${sno}&place=${Placement.encode(place)}", "", true).map {
      case Left(err)  => Left(err.add("setParticipantPlace"))
      case Right(res) => Placement.decode(res)
    }

  // setParticipantStatus set the status of a participants in a competition
  // def setParticipantStatus(coId: Long, sno: String, status: Int): Future[Either[Error, Int]]
  def setParticipantStatus(coId: Long, sno: String, status: Int): Future[Either[Error, Int]] = 
    postAction("setParticipantStatus", App.tourney.id, s"coId=${coId}&sno=${sno}&status=${status.toString}","",true).map {
      case Left(err)  => Left(err.add("setParticipantStatus"))
      case Right(res) => Return.decode2Int(res, "setParticipantStatus")
    }

  // setPantBulkStatus set the status of all participants in a competition
  // returns number of updated entries
  // def setPantBulkStatus(coId: Long, List[(String, Int)]): Future[Either[Error, Int]]
  def setPantBulkStatus(coId: Long, pantStatus: List[(String, Int)]): Future[Either[Error, Int]] = 
    postAction("setPantBulkStatus", App.tourney.id, s"coId=${coId}", write(pantStatus),true).map {
      case Left(err)  => Left(err.add("setPantBulkStatus"))
      case Right(res) => Return.decode2Int(res, "setParticipantStatus")
    }    


  //
  //  PLAYFIELD Interface
  //

  // set playfield
  def setPlayfield(playfield: Playfield): Future[Either[Error, Playfield]] = 
    postAction("setPlayfield", App.tourney.id, "", playfield.encode, true).map {
      case Left(err)  => Left(err)
      case Right(res) => Playfield.decode(res)
    }

  // get playfield
  def getPlayfield(pfNo: Int): Future[Either[Error, Playfield]] = 
    getAction("getPlayfield", App.tourney.id, s"pfNo=${pfNo}").map {
      case Left(err)     => Left(err.add("getPlayfield"))
      case Right(pfield) => Playfield.decode(pfield)
    }

  // get all playfields of tourney 
  def getPlayfields(toId: Long): Future[Either[Error, Seq[Playfield]]] = 
    getAction("getPlayfields", toId).map {
      case Left(err)  => Left(err.add("getPlayfields"))
      case Right(pfs) => Playfield.decSeq(pfs)
    }

  // delete playfield with certain code, return number of deleted entry
  def delPlayfield(code: String): Future[Either[Error, Int]] = 
    postAction("delPlayfield", App.tourney.id, s"code=${code}","",true).map {
      case Left(err)  => Left(err.add("delPlayfield"))
      case Right(cnt) => Return.decode2Int(cnt, "delPlayfield")
    }


  //
  //  PLAYER Interface
  //

  // addPlayer
  def addPlayer(player: Player): Future[Either[Error, Player]] = 
    postAction("addPlayer", App.tourney.id, "", player.encode, true).map {
      case Left(err) => Left(err)
      case Right(pl) => Player.decode(pl)
    }


  //
  // MATCH Interface
  //
  
  // getMatchKo - return sequence of result entries of ko round
  def getMatchKo(coId: Long, coPh: Int):  Future[Either[Error, Seq[ResultEntry]]] =
    getAction("getMatchKo", App.tourney.id, s"coId=${coId.toString}&coPh=${coPh.toString}").map {
      case Left(err)  => Left(err.add("getMatchKo"))
      case Right(res) => ResultEntry.decSeq(res)
    }

  // getMatchGr - return sequence of result entries of a group  
  def getMatchGr(coId: Long, coPh: Int, grId: Int):  Future[Either[Error, Seq[ResultEntry]]] = 
    getAction("getMatchGr", App.tourney.id, s"coId=${coId}&coPh=${coPh}&grId=${grId}").map {
      case Left(err)  => Left(err.add("getMatchGr"))
      case Right(res) => ResultEntry.decSeq(res)
    }

  // hasMatch - returns true if game results for player are available
  def hasResult():  Future[Either[Error, Boolean]] = 
    getAction("hasResult", App.tourney.id).map {
      case Left(err)  => Left(err.add("hasResult"))
      case Right(res) => Return.decode2Boolean(res, "hasResult")
    } 



  //
  //  COMPETITION SECTION Interface
  //

  // def setSectPlayer(coId: Long, secId: Int, pEntries: ArrayBuffer[ParticipantEntry]): Future[Either[Error, Int]] = { 


  // }  

  // def addSect(prevSecId: Int, coId: Long, name: String, secTyp: Int, winSec: Boolean=true): Future[Either[Error, Int]] = { 


  // }  


}  