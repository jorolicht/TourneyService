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

import shared.utils._
import shared.utils.Routines._
import shared.utils.Constants._
import shared.model._
import shared.model.Competition._
import scalajs._
import org.xml.sax.ErrorHandler

trait TourneySvc extends WrapperSvc 
{
  
  // ping server
  def ping(toId: Long, msg: String): Future[Either[Error, String]] = getAction("ping", toId, s"msg=${msg}")

  // get ip address of server
  def getIpAddress(): Future[Either[Error, String]] = {
    Ajax.get("/getIpAddress").map(_.responseText)
      .map(content => Right(content))
      .recover({
        case dom.ext.AjaxException(req) => Left(Error("err0034.ajax.getRequest", s"response: ${req.responseText.take(20)} status: ${req.statusText}", "getIpAddress"))
        case _: Throwable               => Left(Error("err0220.getIpAddress", "getIpAddress")) 
      })
  }

  // export database
  def expDatabase(toId: Long, club: String, date: Int, etyp: String): Future[Either[Error, String]] = 
    getAction("expDatabase", toId, s"club=${club}&date=${date.toString}&etyp=${etyp}")

  // getContent - either error or file content
  def getContent(path: String): Future[Either[Error, String]] = {
    val absPath = if (path.startsWith("/")) path else s"/${path}"
    AppEnv.info("getContent", s"path: ${absPath}")

    Ajax.get(absPath).map(_.responseText)
      .map(content => Right(content))
      .recover({
        case dom.ext.AjaxException(req) => Left( 
          Error(" err0034.ajax.getRequest", s"response: ${req.responseText.take(20)} status: ${req.statusText}", absPath, "getContent") 
        )
        case _: Throwable => Left( Error("err0033.svc.getContent", absPath, "", "getContent")) 
      })
  }


  // getInvitation - either error or file content
  def getInvitation(orgDir: String, startDate: Integer): Future[Either[Error, String]] = {
    val path = s"/service/getInvitation?orgDir=${orgDir}&startDate=${startDate}" 
    Ajax.get(path).map(_.responseText)
      .map(content => Right(content))
      .recover({
        case dom.ext.AjaxException(req) => Left(Error("err0034.ajax.getRequest", s"response: ${req.responseText.take(20)} status: ${req.statusText}", path, "getCfgFile"))
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
      case CompTyp.SINGLE => {
        val plId = tourney.pl2co((sno,coId)).getPlayerId
        (s"${tourney.players(plId).firstname} ${tourney.players(plId).lastname}", s"${tourney.players(plId).clubName}")
      }

      case CompTyp.DOUBLE | CompTyp.MIXED => tourney.pl2co((sno, coId)).getDoubleId match {
        case Left(err)  => println(s"ERROR: invalid double sno ${sno}"); ("?","?")
        case Right(id)  =>  (s"${tourney.players(id._1).lastname}/${tourney.players(id._2).lastname}", s"${tourney.players(id._1).clubName}/${tourney.players(id._2).clubName}")           
      }
      case _ => ("?","?")
    }

    val ctext = clientviews.info.html.CertificatePage(
      orgDir, AppEnv.getMessage("certificate.title", plName), tourney.name, coName, place, plName, clName, locdate, AppEnv.lang
    ).toString

    genCertFile(orgDir, coId, sno, ctext).map { x => {
       dom.window.location.href = s"/content/clubs/${orgDir}/certs/Certificate_${tourney.id}_${coId}_${sno}.html"

       //Home.link(s"/content/clubs/${orgDir}/certs/Certificate_${getToId}_${coId}_${sno}.html")
       AppEnv.debug("genCertFile", s"for coId: ${coId} sno: ${sno}")
    }} 
  }  


  def downloadFile(dloType: DownloadType.Value): Future[Either[Error, (String,String)]] = {
    val path  = genPath("/service/downloadFile", s"toId=${App.tourney.id}&dloType=${dloType.id}")
    AppEnv.debug("downloadFile", s"path: ${path}")
    Ajax.get(path).map( response => { 
      val respText = response.responseText 
      val contDisp = getHdrParam(response.getResponseHeader("content-disposition"))
      val fName = contDisp("filename")
      Right((fName,respText))
    }).recover({
        // Recover from a failed error code into a successful future
        case dom.ext.AjaxException(req) => Left(Error.decodeWithDefault(Error("err0000.communication.error"), req.responseText, s"${req.statusText} / ${req.responseText}", "downloadFile"))
        case _: Throwable               => Left(Error.decodeWithDefault(Error("err0000.communication.error"), "", "request status and text unknown", "downloadFile"))
    })
  }

  def uploadFile(toId: Long, sDate: Int, uplType: UploadType.Value, formData: dom.FormData): Future[Either[Error, Long]] = 
    postForm("/service/uploadFile", s"toId=${toId}&sDate=${sDate}&uplType=${uplType.id}", formData).map {
      case Left(err)  => Left(err.add("uploadFile"))
      case Right(res) => Return.decode2Long(res, "uploadFile")
    }

  //Either[Error,Seq[(Long, Either[Error, Int])]]  
  def updCttFile(toId: Long, sDate: Int, fData: dom.FormData): Future[Either[Error,Seq[(Long, Int)]]] = {
    import upickle.default._
    postForm("/service/sendCttFile", s"toId=${toId}&sDate=${sDate}&uplMode=${UploadMode.Update.id}",fData).map {
      case Left(err)  => Left(err.add("updCttFile"))
      case Right(res) => Right(read[Seq[(Long, Int)]](res))
    }
  }  

  def newCttFile(toId: Long, sDate: Int, fData: dom.FormData): Future[Either[Error, (Long,String)]] = 
    postForm("/service/sendCttFile", s"toId=${toId}&sDate=${sDate}&uplMode=${UploadMode.New.id}",fData).map {
      case Left(err)  => Left(err.add("newCttFile"))
      case Right(res) => try {
        Right(read[(Long,String)](res))
      } catch { case _: Throwable => Left(Error("err0246.decode.sendCttFile")) }
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
  def delTourney(toId: Long): Future[Either[Error, String]] = 
    postAction("delTourney", toId, "", "", true).map {
      case Left(err)  => Left(err.add("delTourney"))
      case Right(res) => Return.decode2String(res, "delTourney")
    }

  // update tourney basis information
  def setTournBase(tournbase: TournBase): Future[Either[Error, Tourney]] =
    postAction("setTournBase", App.tourney.id, "", tournbase.encode()).map {
      case Left(err)     => Left(err.add("setTournBase"))
      case Right(trnyTx) => Tourney.decode(trnyTx)
    }

  // add tourney based on with click TT configuration 
  def addTournCTT(cttData: String, sDate: Int, eDate: Int): Future[Either[Error, Tourney]] = {
    AppEnv.debug("addTournCTT", s"${cttData.take(20)}")
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

  // update competition status
  def updateCompStatus(coId: Long): Future[Either[Error, Boolean]] = 
    if (!App.tourney.comps.isDefinedAt(coId)) Future(Left(Error("err0014.trny.compNotFound", coId))) else {
      App.tourney.updateCompStatus(coId) match {
        case Left(err)      => Future(Left(err))
        case Right(changed) => if (!changed) Future(Right(changed)) else 
          postAction("setCompStatus", App.tourney.id, s"coId=${coId}&status=${App.tourney.comps(coId).status.id}", "", true).map { 
            case Left(err)  => Left(err)
            case Right(res) => Right(true)
        }
      }
    }

  // set whole competition
  def setComp(co: Competition): Future[Either[Error, Competition]] =
    App.tourney.setComp(co) match {
      case Left(err)      => Future(Left(err))
      case Right(locComp) => postAction("setComp", App.tourney.id, "", co.encode, true).map {    
        case Left(err)      => Left(err.add("setComp"))
        case Right(coTx)    => Competition.decode(coTx) match {
          case Left(err)      => Left(err)
          case Right(remComp) => if (locComp == remComp) Right(remComp) else Left(Error("err0215.sync")) 
        }
      }  
    }

  // set whole competition
  def addComp(co: Competition): Future[Either[Error, Competition]] =
    App.tourney.addComp(co: Competition) match {
      case Left(err)      => Future(Left(err))
      case Right(locComp) => postAction("addComp", App.tourney.id, "", co.encode, true).map { 
        case Left(err)   => Left(err.add("addComp"))
        case Right(coTx) => Competition.decode(coTx) match {
          case Left(err)      => Left(err)
          case Right(remComp) => if (locComp.id == remComp.id) Right(remComp) else Left(Error("err0215.sync")) 
        }        

      }
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
    App.tourney.delComp(coId) match {
      case Left(err)  => Future(Left(err))
      case Right(res1) => postAction("delComp", App.tourney.id, s"coId=${coId}", "", true).map {
        case Left(err)   => Left(err)
        case Right(res2) => Return.decode2Boolean(res2, "delComp") match {
          case Left(err)   => Left(err)
          case Right(res3) => if (res1 == res3) Right(res3) else Left(Error("err0215.sync")) 
        }
      }  
    }
  }


  //
  // Register Interface (Single/Double)
  //
  /* regSingle register single player */  
  def regSingle(coId: Long, pl: Player, status: PantStatus.Value): Future[Either[Error, SNO]] = {
    App.tourney.regSingle(coId, pl, status) match {
      case Left(err)      => Future(Left(err))
      case Right(sno1) => {
        postAction("regSingle", App.tourney.id, s"coId=${coId}&status=${status.id}&bulk=false", write[Player](pl), true).map {
          case Left(err)     => Left(err)
          case Right(snoEnc) => try {
            val sno2 = read[SNO](snoEnc)
            if (sno1 == sno2) Right(sno1) else Left(Error("err0215.sync")) 
          } catch { case _: Throwable => Left(Error(""))}
        }     
      } 
    }
  }

  /* regSingle register List of single players */  
  def regSingle(coId: Long, pList: List[Player], pStatus: PantStatus.Value): Future[Either[Error, List[SNO]]] = 
    App.tourney.regSingle(coId, pList, pStatus) match {
      case Left(err)    => Future(Left(err))
      case Right(lList) => postAction("regSingle", App.tourney.id, s"coId=${coId}&status=${pStatus.id}&bulk=true", write[List[Player]](pList), true).map {
        case Left(err)         => Left(err)
        case Right(encList) => try {
          val rList = read[List[SNO]](encList)
          if (lList == rList) Right(lList) else Left(Error("err0215.sync")) 
        } catch { case _: Throwable =>  Left(Error("err0240.svc.regSingle")) }
      }
    }    

  /* regDouble register double player */  
  def regDouble(coId: Long, pls: (Long, Long), pStatus: PantStatus.Value): Future[Either[Error, SNO]] = 
    App.tourney.regDouble(coId, pls, pStatus) match {
      case Left(err)      => Future(Left(err))
      case Right(result1) => postAction("regDouble", App.tourney.id, s"coId=${coId}&status=${pStatus.id}&bulk=false", write[(Long,Long)](pls), true).map {
        case Left(err)      => Left(err)
        case Right(result2) => try {
          val result3 = read[SNO](result2)
          if (result3 == result1) Right(result1) else Left(Error("err0215.sync")) 
        } catch { case _:Throwable => Left(Error("err0057.call.regDouble", result2.take(10),"","regDouble")) }
      }
    }


  /* regDouble register List of double players */  
  def regDouble(coId: Long, ppList: List[(Long, Long)], pStatus: PantStatus.Value): Future[Either[Error, List[SNO]]] = 
    App.tourney.regDouble(coId, ppList, pStatus) match {
      case Left(err)   => Future(Left(err))
      case Right(res1) => postAction("regDouble", App.tourney.id, s"coId=${coId}&status=${pStatus.code}&bulk=true", write[List[(Long,Long)]](ppList), true).map {
        case Left(err)    => Left(err)
        case Right(res2)  => try {
          val res3 = read[List[SNO]](res2)
          if (res3.length == res1.length) Right(res1) else Left(Error("err0215.sync")) 
        } catch { case _:Throwable => Left(Error("err0057.call.regDouble", res2.take(10),"","regDouble")) }
      }  
    }

  //
  // Participant Interface (participant could be Single,Double or Team (future) 
  // 
  // def setPant2Comp(p2c: Pant2Comp)(implicit tse :TournSVCEnv):Future[Either[Error, Pant2Comp]]
  // def setPant2Comps(p2cs: Seq[Pant2Comp])(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  // def delPant2Comps(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
  // def getPant2Comps(toId: Long): Future[Either[Error, Pant2Comps]]
  // def getPantPlace(toId: Long, coId: Long, sno: String): Future[Either[Error, String]]
  // def setPantPlace(coId: Long, sno: String, place: String)(implicit tse :TournSVCEnv): Future[Either[Error, Placement]]
  // def setPantStatus(coId: Long, sno: String, status: Int)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]

  // get tourney player 2 comp 
  def getPant2Comps(toId: Long):  Future[Either[Error, Seq[Pant2Comp]]] =
    getAction("getPant2Comps", toId).map { 
      case Left(err)     => Left(err.add("getPant2Comps"))
      case Right(pa2cos) => Pant2Comp.decSeq(pa2cos)
    }

  // get the player placement
  def getParticipantPlace(coId: Long, sno: String): Future[Either[Error, Placement]] =
    getAction("getPantPlace", App.tourney.id, s"coId=${coId}&sno=${sno}").map {
      case Left(err)    => Left(err.add("getPantPlace"))
      case Right(place) => Placement.decode(place)
    }


  // delete participant entry
  def delPant2Comp(coId: Long, sno: String): Future[Either[Error, Int]] = {
    postAction("delPant2Comp", App.tourney.id, s"coId=${coId}&sno=${sno}", "", true) map {
      case Left(err)  => Left(err.add("delPant2Comp"))
      case Right(res) => Return.decode2Int(res, "delPant2Comp")
    } 
  }     

  // set the participant placement
  def setPantPlace(coId: Long, sno: String, place: Placement): Future[Either[Error, Placement]] = 
    postAction("setPantPlace", App.tourney.id, s"coId=${coId}&sno=${sno}&place=${Placement.encode(place)}", "", true).map {
      case Left(err)  => Left(err.add("setPantPlace"))
      case Right(res) => Placement.decode(res)
    }

  // setPantStatus set the status of a participants in a competition
  def setPantStatus(coId: Long, sno: SNO, status: PantStatus.Value): Future[Either[Error, PantStatus.Value]] = 
    postAction("setPantStatus", App.tourney.id, s"coId=${coId}&sno=${sno.value}&status=${status.id}","",true).map {
      case Left(err)  => Left(err.add("setPantStatus"))
      case Right(res) => Return.decode2Int(res, "setPantStatus") match {
        case Left(err)     => Left(err)
        case Right(sResult) => { 
          App.tourneyUpdate = true
          App.tourney.setPantStatus(coId, sno, PantStatus(sResult)) 
        }
      }
    }

  // setPantBulkStatus set the status of all participants in a competition
  // returns number of updated entries
  // def setPantBulkStatus(coId: Long, List[(String, Int)]): Future[Either[Error, Int]]
  def setPantBulkStatus(coId: Long, pantStatus: List[(String, PantStatus.Value)]): Future[Either[Error, Int]] = {
    implicit val pStatusReadWrite: upickle.default.ReadWriter[PantStatus.Value] =
      upickle.default.readwriter[Int].bimap[PantStatus.Value](x => x.id, PantStatus(_))
   
    postAction("setPantBulkStatus", App.tourney.id, s"coId=${coId}", write(pantStatus), true).map {
      case Left(err)  => Left(err.add("setPantBulkStatus"))
      case Right(res) => Return.decode2Int(res, "setPantStatus") match {
        case Left(err)  => Left(err)
        case Right(err) => { App.tourneyUpdate = true; App.tourney.setPantBulkStatus(coId, pantStatus) }
      }  
    }
  }  


  //
  //  PLAYFIELD Interface
  //
  // set playfield
  def setPlayfield(pf: Playfield): Future[Either[Error, Unit]] = {
    App.tourney.setPlayfield(pf)
    postAction("setPlayfield", App.tourney.id, "", pf.encode, true).map {
      case Left(err)  => Left(err)
      case Right(res) => Right({})
    }
  }  

  def setPlayfield(coId: Long, coPhId: Int, game: Int, startTime: String):  Future[Either[Error, Unit]] = {
    App.tourney.setPlayfield(coId, coPhId, game, startTime)
    postAction("setPlayfield", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}&game=${game}&startTime=${startTime}", "", true).map {
      case Left(err)  => Left(err)
      case Right(res) => Right({})
    }
  }

  // delete playfield with certain code, return number of deleted entry
  def delPlayfield(coId: Long, coPhId: Int, game: Int): Future[Either[Error, Boolean]] = 
    if (!App.tourney.delPlayfield(coId, coPhId, game)) Future(Right(false)) else {
      postAction("delPlayfield", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}&game=${game}","",true).map {
        case Left(err)  => Left(err.add("delPlayfield"))
        case Right(res) => Return.decode2Boolean(res, "delPlayfield")
      }
    }

  // delete playfield with certain code, return number of deleted entry
  def delPlayfield(pfNo: String): Future[Either[Error, Boolean]] = 
    if (!App.tourney.delPlayfield(pfNo)) Future(Right(false)) else {
      postAction("delPlayfield", App.tourney.id, s"pfNo=${pfNo}", "", true).map {
        case Left(err)  => Left(err.add("delPlayfield"))
        case Right(res) => Return.decode2Boolean(res, "delPlayfield")
      }
    }

  // sync playfield entries
  def syncPlayfields: Future[Either[Error, Unit]] = 
    getAction("getPlayfields", App.tourney.id, "").map {
      case Left(err)     => Left(err)
      case Right(pfSeq)  => {
        try {
          val pfS = read[Seq[Playfield]](pfSeq)
          App.tourney.playfields = collection.mutable.HashMap( pfS.map { p => { p.nr -> p }} : _*)
          Right({})
        } catch { case _:Throwable => Left(Error("???"))}
      }
    }

    
  //
  //  PLAYER Interface
  //

  // addPlayer
  def addPlayer(newPlayer: Player): Future[Either[Error, Player]] = 
    App.tourney.addPlayer(newPlayer) match {
      case Left(err)  => Future(Left(err))
      case Right(lPl) => postAction("addPlayer", App.tourney.id, "", newPlayer.encode, true).map {
        case Left(err)    => AppEnv.info("addPlayer", s" ${newPlayer}"); Left(err)
        case Right(rPlCo) => Player.decode(rPlCo) match {
          case Left(err)     => Left(err)
          case Right(rPl)    => if (rPl == lPl) Right(lPl) else Left(Error("err0215.sync"))
        }  
      }  
    }


  /** setPlayer updates existing player 
   *  if necessary creates new club entry
   */
  def setPlayer(player: Player): Future[Either[Error, Player]] = 
    App.tourney.setPlayer(player) match {
      case Left(err) => Future(Left(err))
      case Right(lPl) => postAction("setPlayer", App.tourney.id, "", player.encode, true).map {
        case Left(err) => AppEnv.info("setPlayer", s" ${player}"); Left(err)
        case Right(rPlCo) => Player.decode(rPlCo) match {
          case Left(err)     => Left(err)
          case Right(rPl)    => if (rPl == lPl) Right(lPl) else Left(Error("err0215.sync"))  
        }
      }  
    } 


  def setPlayer(plId: Long, license: CttLicense): Future[Either[Error, Player]] = 
    App.tourney.setPlayer(plId, license) match {
      case Left(err) => Future(Left(err))
      case Right(localPlayer) => postAction("setPlayer", App.tourney.id, s"plId=${plId}&license=${license.value}", "", true).map {
        case Left(err)             => Left(err)
        case Right(remoPlayerCode) => Player.decode(remoPlayerCode) match {
          case Left(err)               => Left(err)
          case Right(remoPlayer)       => if (remoPlayer == localPlayer) Right(localPlayer) else Left(Error("err0215.sync"))  
        }
      }  
    }


  //
  // MATCH Interface
  //
  // hasMatch - returns true if game results for player are available
  def hasResult():  Future[Either[Error, Boolean]] = 
    getAction("hasResult", App.tourney.id).map {
      case Left(err)  => Left(err.add("hasResult"))
      case Right(res) => Return.decode2Boolean(res, "hasResult")
    } 


  // inputMatch - input match result, returns affected game numbers 
  def inputMatch(coId: Long, coPhId: Int, gameNo: Int, sets: (Int,Int), balls: String, info: String, playfield: String, timeStamp: String): Future[Either[Error, List[Int]]] = 
    if (App.tourney.isDummy) Future(Left(Error("err0227.tourney.isDummy"))) else {
      App.tourney.inputMatch(coId, coPhId, gameNo, sets, balls, info, playfield, timeStamp) match {
        case Left(err)     => Future(Left(err))
        case Right(result) => if (App.serverLess) Future(Right(result)) else {
          postAction("inputMatch", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}&gameNo=${gameNo}&timeStamp=${timeStamp}", 
                     write[( (Int, Int), String, String, String) ]((sets, balls, info, playfield)), true).map {
            case Left(err)  => Left(err.add("postAction->inputMatch"))
            case Right(res) => {
              try Right(read[List[Int]](res))  
              catch { case _:Throwable => Left(Error("err0223.svc.inputMatch.decodeResult", "", "", "inputMatch")) }    
            } 
          }
        }
      }
    }   

  // resetMatch - reset a match locally result, returns affected game numbers 
  def resetMatch(coId: Long, coPhId: Int, gameNo: Int, rPantA: Boolean=false, rPantB: Boolean=false): Future[Either[Error, List[Int]]] = 
    if (App.tourney.isDummy) Future(Left(Error("err0227.tourney.isDummy"))) else {
      App.tourney.resetMatch(coId, coPhId, gameNo, rPantA, rPantB) match {
        case Left(err)     => Future(Left(err))
        case Right(result) => if (App.serverLess) Future(Right(result)) else {
          postAction("resetMatch", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}&gameNo=${gameNo}&resetPantA=${rPantA}&resetPantB=${rPantB}", "", true).map {
            case Left(err)  => Left(err.add("postAction->resetMatch"))
            case Right(res) => {
              try Right(read[List[Int]](res))  
              catch { case _:Throwable => Left(Error("err0228.svc.resetMatch.decodeResult", "", "", "resetMatch")) }    
            } 
          }  
        }
      }
    }       



  // inputReferee - input match result (no overwrite), returns affected game numbers 
  def inputReferee(toId: Long, coId: Long, coPhId: Int, gameNo: Int, 
                 sets: (Int,Int), balls: String, info: String, playfield: String): Future[Either[Error, List[Int]]] = 
    postAction("inputMatch", toId, s"coId=${coId}&coPhId=${coPhId}&gameNo=${gameNo}",
      write[( (Int, Int), String, String, String) ]((sets, balls, info, playfield)), true).map {
        case Left(err)  => Left(err.add("postAction/inputReferee"))
        case Right(res) => {
          try Right(read[List[Int]](res))  
          catch { case _:Throwable => Left(Error("err0231.svc.inputReferee.decodeResult", "", "", "inputReferee")) }    
          } 
      }
  

    

  // getMatch - get a match 
  def getMatch(toId: Long, coId: Long, coPhId: Int, gameNo: Int): Future[Either[Error, MEntry]] = {
    import cats.data.EitherT
    import cats.implicits._ 

    if   (App.tourney.isDummy) Future(Left(Error("err0227.tourney.isDummy"))) 
    else try   Future(Right(App.tourney.cophs((coId, coPhId)).getMatch(gameNo)))
         catch { case _: Throwable => Future(Left(Error("err0232.svc.getMatch.error", gameNo.toString)))}
  }



  // resetMatchRemote - reset a match locally result, returns affected game numbers 
  def resetMatchRemote(toId: Long, coId: Long, coPhId: Int, gameNo: Int,  
                 rPantA: Boolean=false, rPantB: Boolean=false): Future[Either[Error, List[Int]]] = 
    if (App.serverLess) Future(Right(List())) else {
      postAction("resetMatch", toId, s"coId=${coId}&coPhId=${coPhId}&gameNo=${gameNo}&resetPantA=${rPantA}&resetPantB=${rPantB}", "", true).map {
          case Left(err)  => Left(err.add("postAction/reseetMatch"))
          case Right(res) => {
            try Right(read[List[Int]](res))  
            catch { case _:Throwable => Left(Error("err0228.svc.resetMatch.decodeResult", "", "", "resetMatch")) }    
            } 
        }
    }  

  // resetMatch - reset a match locally result, returns affected game numbers 
  def resetMatches(coId: Long, coPhId: Int): Future[Either[Error, List[Int]]] = 
    if (App.tourney.isDummy) Future(Left(Error("err0227.tourney.isDummy"))) else {
      App.tourney.resetMatches(coId, coPhId) match {
        case Left(err)  => Future(Left(err))
        case Right(res) => postAction("resetMatches", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}", "", true).map {
          case Left(err)  => Left(err.add("postAction/resetMatches"))
          case Right(res) => {
            try Right(read[List[Int]](res))  
            catch { case _:Throwable => Left(Error("err0228.svc.resetMatch.decodeResult", "", "", "resetMatch")) }    
          }  
        }           
      }
    }    




  //
  //  COMPETITION PHASE Interface
  //
  def addCompPhase(coId: Long, name: String): Future[Either[Error, CompPhase]] = 
    App.tourney.addCompPhase(coId, name) match {
      case Left(err)  => Future(Left(err))
      case Right(lCoPh ) => postAction("addCompPhase", App.tourney.id, s"coId=${coId}&name=${name}", "", true).map {
        case Left(err)      => Left(err)
        case Right(rCoPhE)  => CompPhase.decode(rCoPhE) match {
          case Left(err)       => Left(err)
          case Right(rCoPh)    => if (lCoPh.coPhId == rCoPh.coPhId) Right(lCoPh) else Left(Error("err0215.sync")) 
        }    
      }     
    }


  def updateCompPhaseStatus(coId: Long, coPhId: Int, status: CompPhaseStatus.Value): Future[Either[Error, Boolean]] = 
    App.tourney.updateCompPhaseStatus(coId, coPhId, status) match {
      case Left(err)  => Future(Left(err))
      case Right(res) => if (!res) Future(Right(res)) else {
        postAction("updateCompPhaseStatus", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}&status=${status.id}", "", true).map {
          case Left(err)  => Left(err)
          case Right(res) => Return.decode2Boolean(res, "updateCompPhaseStatus") 
        }
      }
    }  


  def saveCompPhase(coph: CompPhase): Future[Either[Error, Unit]] = {
    //println(s"saveCompPhase -> coId: ${coph.coId} coPhId: ${coph.coPhId}")
    postAction("saveCompPhase", App.tourney.id, "", coph.encode(), true).map {
      case Left(err)  => Left(err)
      case Right(res) => Right({})  
    }
  } 

  // delCompPhase - delete competition phase if it exists (and possible)
  def delCompPhase(coId: Long, coPhId: Int): Future[Either[Error, Unit]] = 
    App.tourney.delCompPhase(coId, coPhId) match {
      case Left(err)  => Future(Left(err))
      case Right(res) => postAction("delCompPhase", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}", "", true).map {
        case Left(err)  => Left(err)
        case Right(res) => Right({})  
    }}

  // getCompPhase - fetches whole competition phase from server
  def getCompPhase(coId: Long, coPhId: Int): Future[Either[Error, CompPhase]] = 
    postAction("getCompPhase", App.tourney.id, s"coId=${coId}&coPhId=${coPhId}", "", false).map {
      case Left(err)      => Left(err)
      case Right(coPhEnc) => CompPhase.decode(coPhEnc) match {
        case Left(err)       => Left(err)
        case Right(result)   => Right(result)
      }      
    }


  //
  //  ClickTT Interface
  //
  def genCttResult(): Future[Either[Error, Array[ (Long, Either[Error, (List[String],List[String]) ]) ] ]] = {
    postAction("genCttResult", App.tourney.id, "", "").map {
      case Left(err)     => Left(err.add("genCttResult"))
      case Right(result) => {
        try {
          val decodeResult = read[Array[ (Long, Either[String, (List[String], List[String])]) ]] (result)
          val finalResult= decodeResult.map { entry => entry._2 match {
            case Left(errString) => (entry._1, Left(Error.decode(errString, "", "genCttResult")))
            case Right(snoList)  => (entry._1, Right(snoList)) 
          }}
          Right( finalResult ) 
        }   
        catch { case _:Throwable => {
          println(s"ERROR: genCttResult decoding result not possible: ${result.take(10)}")
          Left(Error("err0207.genCttResult", "","","genCttResult")) 
        }}        
      }
    }
  }





}  