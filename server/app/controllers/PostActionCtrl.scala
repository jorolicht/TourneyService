package controllers

import java.io.File
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.nio.charset.StandardCharsets._
import javax.inject._

// imports for Silhouette
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.mohiva.play.silhouette.api.{ LogoutEvent, Silhouette }
import utils.auth.DefaultEnv

//import SQLiteProfile to connect to sqlite database
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent._
import scala.util._
import sys.process._

import play.api.{ Environment, Configuration, Logging }
import play.api.inject.ApplicationLifecycle
import play.api.http.ContentTypes
import play.api.libs.EventSource
import play.api.libs.mailer._
import play.api.libs.json._
import play.api.mvc._
import play.api.i18n._

import models.daos.{ TourneyDAO, LicenseDAO }
import models.User
import shared.model._
import shared.utils.Routines._
import shared.utils._
import tourn.services._

/** Controller for services regarding Competitions, Players, Playfields, ...
  */
@Singleton
class PostActionCtrl @Inject()
( 
  coco:       ControllerComponents,
  lc:         ApplicationLifecycle,
  licDao:     LicenseDAO,
  mailer:     MailerClient,
  langs:      Langs,
  silhouette: Silhouette[DefaultEnv]
)(
  implicit
  ec:           ExecutionContext,
  env:          Environment,
  cfg:          Configuration,
  tsv:          TourneyService,
  tourneyDao:   TourneyDAO,
  assetsFinder: AssetsFinder
)    
  extends AbstractController(coco) with I18nSupport with Logging
{

  // 
  // Service based on postAction wrapper 
  // 
  def postAction(params: String) = silhouette.UserAwareAction.async { implicit request: Request[AnyContent] =>
    import tourn.services.Crypto._
    import upickle.default._
    import cats.data.EitherT

    def chkAccess(ctx: shared.utils.Session): Boolean = (ctx.orgId > 0) | !Crypto.accessCtrl

    val msgs: Messages = messagesApi.preferred(request)
    val ctx     = getSessionFromCookie(request.cookies.get("TuSe"), messagesApi.preferred(request)) 
    val param   = encParam(params)
    val reqData = request.body.asText.getOrElse("")

    // verify necessary parameters
    val (pMap, cmd, toId, callId, trigger, valid) = (for {
      pM    <- encEParam(params)
      cmd   <- getEParam(pM, "cmd")
      toId  <- getEParam(pM, "toId", 0L)
      caId  <- getEParam(pM, "caId")
      trig  <- getEParam(pM, "trigger", false)
    } yield { (pM, cmd, toId, caId, trig) }) match {
      case Left(err)  => (new scala.collection.mutable.HashMap[String, String], "error", 0L, "", false, false)
      case Right(res) => (res._1, res._2, res._3, res._4, res._5, true)  
    }

    logger.info(s"postAction -> ${pMap}") 

    if (!valid) logger.error(s"postAction invalid arguments") 
    implicit val caId = CallerIdent(callId)
    implicit val tse  = TournSVCEnv(toId, ctx.orgDir, trigger, callId)
      
    cmd match {
      //
      // Register Action Routines (Single/Double)
      //

      // def setPant2Comp(coId: Long, plId: Player): Future[Either[Error, Pant2Comp]]
      // registers a player with a competition, returns Pant2Comp entry
      case "setPant2Comp" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights", "", "", "setPant2Comp").encode)) else {
          Pant2Comp.decode(reqData) match {
            case Left(err)  => Future(BadRequest(err.add("setPant2Comp").encode))
            case Right(p2c) => tsv.setPant2Comp(p2c).map {
              case Left(err)     => BadRequest(err.add("setPant2Comp").encode)
              case Right(p2cRes) => Ok(p2cRes.encode) 
            }
          } 
        }
      }   
 

      // def delPant2Comp(coId: Long, sno: String): Future[Either[Error, Int]]
      // removes a player from the competition
      case "delPant2Comp" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights", "", "", "delPant2Comp").encode)) else {
          tsv.delPant2Comp(getParam(pMap, "coId", -1L), getParam(pMap, "sno", "XXXXX")).map {
            case Left(err)     => BadRequest(err.add("delPant2Comp").encode)
            case Right(result) => Ok(Return(result).encode) 
          }
        }
      } 


      // def regSingle(coId: Long, pl: Player): Future[Either[Error, String]]
      // registers a player with a competition, returns playerId
      case "regSingle" => {
        (for {
          data    <- EitherT(Future( encEParam(reqData)))
          player  <- EitherT(Future( Player.decode(data("player")) ))
          pl      <- EitherT(tsv.addPlayer(player))
          result  <- EitherT(tsv.setPant2Comp(Pant2Comp.single(pl.id, getParam(pMap, "coId", 0L), getParam(pMap, "status", -1))))
        } yield { (pl, result) }).value.map {
          case Left(err)  => BadRequest(err.add("regSingle").encode)
          case Right(res) => Ok(Return(res._1.id).encode) 
        }
      }

      //def regDouble(coId: Long, pl1: Player, pl2: Player): Future[Either[Error, String]]
      case "regDouble" => {
        (for {
          data <- EitherT(Future( encEParam(reqData)))
          pl1  <- EitherT(Future( Player.decode(data("player1")) ))
          pl2  <- EitherT(Future( Player.decode(data("player2")) ))
          p1   <- EitherT(tsv.setPlayer(pl1))
          p2   <- EitherT(tsv.setPlayer(pl2))
          res  <- EitherT(tsv.setPant2Comp(Pant2Comp.double(p1.id, p2.id, getParam(pMap, "coId", 0L), getParam(pMap, "status", -1))) )
        } yield { (p1.id, p2.id) }).value.map { 
          case Left(err)  => BadRequest(err.add("regDouble").encode)
          case Right(res) => Ok(write(res)) 
        }
      }


      //
      // Pant Action Routines (Pant could be Single,Double or Team (future) 
      // 

      // setPantStatus sets the status of a Pant within a competition, returns status
      // def setPantStatus(coId: Long, sno: String, status: Int): Future[Either[Error, Int]]
      case "setPantStatus" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights","","","setPantStatus").encode)) else {
          tsv.setPantStatus(getParam(pMap, "coId", 0L), getParam(pMap, "sno"), getParam(pMap, "status", 0)).map {
            case Left(err)  => BadRequest(err.encode)
            case Right(res) => Ok(Return(res).encode)
          }
        }
      }  

      // setPantBulkStatus sets the status of a participant within a competition, returns number of affected
      // def setPantBulkStatus(coId: Long, List[(String, Int)]): Future[Either[Error, Int]]
      case "setPantBulkStatus" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights","","","setPantBulkStatus").encode)) else {
          val pantStatus = read[List[(String,Int)]](reqData)
          logger.warn(s"setPantBulkStatus => pants status: ${pantStatus}") 
          tsv.setPantBulkStatus(getParam(pMap, "coId", 0L), pantStatus).map {
            case Left(err)  => BadRequest(err.encode)
            case Right(res) => Ok(Return(res).encode)
          }
        }
      }  



      
      // setPantPlace sets the place of a participant within a competition, returns placement
      // setPantPlace(coId: Long, sno: String, place: String): Future[Either[Error, Placement]]
      case "setPantPlace" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights","","","setPantPlace").encode)) else {
          tsv.setPantPlace(getParam(pMap, "coId", 0L), getParam(pMap, "sno"), getParam(pMap, "place")).map {
            case Left(err)     => BadRequest(err.encode)
            case Right(result) => Ok(Placement.encode(result))          
          }  
        }
      }  


      //
      // Playfield Action Routines
      //

      /** setPlayfield
       *  def setPlayfield(pf: Playfield): Future[Either[Error, Int]]
       */
      case "setPlayfield"    => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights","","","setPlayfield").encode)) else {
          Playfield.decode(reqData) match {
            case Left(err)   => Future( BadRequest(err.encode) )
            case Right(pf)   => tsv.setPlayfield(pf).map {
              case Left(err)   => BadRequest(err.encode)
              case Right(pfNr) => Ok(Return(pfNr).encode)
            }  
          }
        }
      }    
      
      
      /** setPfieldInfo
       *  setPfieldInfo(pfi: PfieldInfo)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
       */
      case "setPfieldInfo"    => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights","","","setPfieldInfo").encode)) else {
          PfieldInfo.decode(reqData) match {
            case Left(err)    => Future(BadRequest(err.encode))
            case Right(pfi)   => tsv.setPfieldInfo(pfi)(tse).map {
              case Left(err)     => BadRequest(err.encode)
              case Right(res)    => Ok(Return(res).encode)
            }
          }
        } 
      }  

      // delete a playfield with code, returns number of deleted fields
      // def delPlayfield(code: String)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
      case "delPlayfield"    => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
          tsv.delPlayfield(getParam(param, "no", -1), getParam(param, "code"), getParam(param, "verify", false) ).map {
            case Left(err)    => BadRequest(err.encode)
            case Right(pfNo)  => Ok(Return(pfNo).encode)
          }
        }  
      }  

      //
      // Tourney POST-Action Routines
      //  

      /** addTourney adds a tourney to the database
        * addTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]
        */
      case "addTourney" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights","","","addTourney").encode)) else {
          Tourney.decode(reqData) match {
            case Left(err)    => Future(BadRequest(err.encode))
            case Right(trny)  => tsv.addTourney(trny)(tse).map {
              case Left(err)     => BadRequest(err.encode)
              case Right(trnyId) => Ok(Return(trnyId).encode)
            }
          }
        }  
      }  


      /** delTourney deletes a tourney
       * addTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]
       */
      case "delTourney" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights","","","delTourney").encode)) else {
          tsv.delTourney(toId)(tse).map {
            case Left(err)     => BadRequest(err.add("delTourney").encode)
            case Right(result) => Ok(Return(result).encode)
          }
        } 
      }


      // addTournBase adds a tourney (from a tournBase) to the database
      case "addTournBase" => if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
        TournBase.decode(reqData) match {
          case Left(err)       => logger.error(s"addTournBase: ${err.msgCode}"); Future(BadRequest(err.encode))
          case Right(trnyBase) => tsv.addTournBase(trnyBase)(tse).map {
            case Left(err)        => logger.error(s"addTournBase: ${err.msgCode}"); BadRequest(err.add("addTournBase").encode)
            case Right(trny)      => Ok(trny.encode())
          }
        }
      }  
    

      // sync (overwrite) tourney and save to disk
      case "syncTourney" => if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
        Tourney.decode(reqData) match {
          case Left(err)   => logger.error(s"syncTourney: ${err.msgCode}"); Future(BadRequest(err.encode))
          case Right(trny) => tsv.syncTourney(trny)(tse).map {
            case Left(err)     => logger.error(s"syncTourney: ${err.msgCode}"); BadRequest(err.add("syncTourney").encode)
            case Right(res)    => Ok(Return(res).encode)
          }
        }
      }  

      
      // save tourney to disk
      case "saveTourney" => if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
        tsv.saveTourney(toId) match {
          case Left(err)  => Future(BadRequest(err.encode))
          case Right(res) => Future(Ok(Return(res).encode))
        }
      }
  

      // setTournBase sets a tourney (from a tournBase) to the database
      case "setTournBase" => if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
        TournBase.decode(reqData) match {
          case Left(err)   => Future( BadRequest(err.encode) ) 
          case Right(tb)   => tsv.setTournBase(tb)(tse).map {
              case Left(err)   => BadRequest(err.add("setTournBase").encode) 
              case Right(trny) => Ok(trny.encode())
          }
        }
      }
 

      // addTournBase adds tourney based on click TT data
      case "addTournCTT" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
          val orgDir    = ctx.orgDir
          val organizer = ctx.organizer

          // verify necessary parameters
          val (sDate, eDate, valid) = (for {
            start  <- getEParam(pMap, "sDate", 0)
            end    <- getEParam(pMap, "eDate", 0)
          } yield { (start, end) }) match {
            case Left(err)  => (0, 0, false)
            case Right(res) => (res._1, res._2, true)  
          }
          if (!valid) Future(BadRequest(Error("err0047.post.addTournCTT").encode)) else {
            CttService.load("", reqData) match { 
              case Left(err)      => Future(BadRequest(err.add("addTournCTT").encode)) 
              case Right(cttTrny) => tsv.addTournCTT(cttTrny, orgDir, organizer).map { 
                case Left(err)   => BadRequest(err.add("addTournCTT").encode) 
                case Right(trny) => tsv.saveTourney(trny.id) match {
                  case Left(err)  => BadRequest(err.encode)
                  case Right(res) => Ok(trny.encode())
                }
              } 
            }       
          }
        }     
      } 


      //
      // Competition Action Routines
      //    

      /** setComp updates a competition, returns either error or the competition
        * calls setComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Competition]]        
        */
      case "setComp"   => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {       
          Competition.decode(reqData) match {
            case Left(err)   => Future(BadRequest(err.encode))
            case Right(co)   => tsv.setComp(co)(msgs, tse).map { 
              case Left(err)    => { logger.error(s"setComp: ${err.encode}" ); BadRequest(err.add("setComp").encode) }  
              case Right(newCo) => Ok(newCo.encode) 
            }
          }
        }
      }      

      /** addComp creates  a competition, returns either error or the competition
        * calls addComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Competition]]        
        */
      case "addComp"   => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
          Competition.decode(reqData) match {
            case Left(err)   => Future(BadRequest(err.encode))
            case Right(co)   => tsv.addComp(co)(msgs, tse).map { 
              case Left(err)    => { logger.error(s"addComp: ${err.encode}"); BadRequest(err.add("addComp").encode) }  
              case Right(newCo) => { Ok(newCo.encode) }
            }
          }
        }
      } 


      /** setCompStatus sets the competition status, returns true if status is set, otherwise false
        * calls setCompStatus(coId: Long, status: Int)(implicit tse: TournSVCEnv): Future[Either[Error, Boolean]]        
        */
      case "setCompStatus"  => {
        import shared.model.Competition._
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
          tsv.setCompStatus(getParam(pMap, "coId", -1L), getParam(pMap, "status", CS_UNKN) ).map {
            case Left(err)   => { logger.error(s"setComp: ${err.encode}" ); BadRequest(err.add("setCompStatus").encode) }
            case Right(res)  => Ok(Return(res).encode)
          } 
        }
      }     
      
      
      /** delComp deletes a competition
        * calls delComp(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]      
        */
      case "delComp"  => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
          tsv.delComp(getParam(pMap, "coId", -1L)).map { 
            case Left(err)   => BadRequest(err.encode)
            case Right(res)  => Ok(Return(res).encode)
          }           
        }
      }    


      //
      // PLAYER Routines
      //
      // addPlayer adds a player to the database
      case "addPlayer" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights", "", "", "addPlayer").encode)) else {
          Player.decode(reqData) match {
            case Left(err)      => Future(BadRequest(err.encode))
            case Right(player)  => tsv.addPlayer(player)(tse).map {
              case Left(err)    => BadRequest(err.encode)
              case Right(pl)    => { logger.info(s"addPlayer: RETURN" );  Ok(pl.encode) }
            }
          }
        }  
      }  

      // setPlayerLicence sets licence of player
      case "setPlayerLicence" => {
        if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights", "", "", "setPlayerLicence").encode)) else {
          tsv.setPlayerLicence(getParam(pMap, "plId", -1L),getParam(pMap, "licence", "")).map {
            case Left(err)    => BadRequest(err.encode)
            case Right(pl)    => Ok(pl.encode)
          }
        }  
      } 


      //
      // Click TT Action Routines
      //
      case "genCttResult" => if (!chkAccess(ctx)) Future(BadRequest(Error("err0080.access.invalidRights").encode)) else {
        import scala.collection.mutable.ArrayBuffer
        import scala.xml.transform.RewriteRule
        import scala.xml.transform.RuleTransformer

        tsv.getTourney(toId).map {
          case Left(err)   => { logger.error(s"genCttResult: ${err.encode}" ); BadRequest(err.add("genCttResult").encode) }
          case Right(trny) => {
            val tourneyDir = s"${env.rootPath}${File.separator}db${File.separator}Tourneys${File.separator}${ctx.orgDir}"
            val cttFN    = s"${tourneyDir}/${trny.startDate}_${toId}_participants.xml"
            val cttResFN = s"${tourneyDir}/${trny.startDate}_${toId}_result.xml"

            val clickTTNode = MyXML.loadFile(cttFN)
            val cttTrny = CttService.readCttTourney(clickTTNode)
            var resClickTTNode = clickTTNode.head
            val coPlMapping = genCttMapping(trny, cttTrny)
            coPlMapping.foreach { elem => {
              elem._2 match {
                case Left(err)     => logger.info(s"genCttResult -> coId ${elem._1} ${err}") 
                case Right(plList) => {
                  val co = trny.comps(elem._1)
                  val matches = scala.xml.XML.loadString(trny.getCompMatches(co.id))   
                  val rwRule = new tourn.services.AddMatches(matches, co.getAgeGroup, co.typ, co.getRatingLowLevel, co.getRatingUpperLevel, co.getRatingRemark, co.startDate)
                  val addRule = new scala.xml.transform.RuleTransformer(rwRule)
                  resClickTTNode = addRule.transform(resClickTTNode).head
                } 
              }
            }}
            MyXML.saveClickTT(resClickTTNode, cttResFN)
            Ok( write[ Array[(Long,   Either[String, (List[String], List[String]) ])] ] (coPlMapping) )
          }
        }
      }  
 

      //
      // Miscellaneous Action Routines
      //
      case "error" => {
        logger.error(s"postAction params: ${params}") 
        Future(BadRequest(Error("err0022.post.decode", params,"","error").encode))
      }

      case "test" => {
        logger.info(s"postAction test params: ${params}" ) 
        Future(Ok(s"Result cmd: ${cmd} toId: ${toId} reqData: ${reqData}"))
      }  

      case _ => Future(BadRequest(Error("err0063.post.unknown", cmd, "", "<unknown>").encode))
    }
  }

  
  // generate Ctt Mapping
  // returns for every competiion either error or two list (tupel) of mapped and not mapped players

  def genCttMapping(trny: Tourney, cttTrny: CttTournament): Array[(Long, Either[String, (List[String], List[String])])] = {

    import scala.collection.mutable.{ HashMap, ArrayBuffer }      
    val result = new ArrayBuffer[(Long, Either[String, (List[String], List[String])])]

    // reset ident settings
    trny.pl2co.foreach(_._2.ident = "") 

    for ((coId, comp) <- trny.comps) {
      // get approbriate competition
      val cttCompList = cttTrny.competitions.filter( _.matchWith(comp.getAgeGroup, comp.typ, comp.getRatingLowLevel, comp.getRatingUpperLevel, comp.getRatingRemark, comp.startDate) )
      if (cttCompList.size == 0) { 
        result += ( (coId, Left(Error("err0208.genCttResult.noMatchingComp").encode)) )
      } else if (cttCompList.size > 1) {
        result += ( (coId, Left(Error("err0209.genCttResult.moreMatchingComps").encode)) )
      } else {  
        val cttComp = cttCompList.head
        val name2person    = new HashMap[String, ArrayBuffer[CttPerson]]()
        val licence2player = new HashMap[String, String]()

        // generate licence2player 
        for (pl <- cttComp.players) {
          val lic = pl.persons.map(p => p.licenceNr).mkString("·")
          licence2player(lic) = pl.id 
        }

        // generate name2person(licence)
        for (pl <- cttComp.players; pe <- pl.persons) {
          val name = s"${pe.lastname}·${pe.firstname}"
          if (!name2person.isDefinedAt(pe.lastname)) {
            name2person(pe.lastname) = ArrayBuffer(pe) 
          } else {
            name2person(pe.lastname) += (pe)
          }          
        }
        
        // update pl2co database
        comp.typ match {
          case Competition.CT_SINGLE =>
            trny.pl2co.filter(_._1._2 == coId).foreach { case (key, entry) => {
              val plId    = entry.getSingleId
              val lic     = trny.players(plId).getLicenceNr
              entry.ident = licence2player(lic)
            }}

          case Competition.CT_DOUBLE =>
            trny.pl2co.filter(_._1._2 == coId).foreach { case (key, entry) => {
              val plIds  = entry.getDoubleId
              val lic1   = trny.players(plIds._1).getLicenceNr
              val lic2   = trny.players(plIds._2).getLicenceNr

              entry.ident = 
                if (licence2player.isDefinedAt(s"${lic1}·${lic2}")) {
                  licence2player(s"${lic1}·${lic2}")
                } else if (licence2player.isDefinedAt(s"${lic2}·${lic1}")) {
                  licence2player(s"${lic2}·${lic1}")
                } else { ""}  
            }}

          case _ => logger.info(s"coId: ${coId} with competition typ: ${comp.typ} not supported" ) 
        }

        val okMissList = trny.pl2co.filter(_._1._2==coId).partition( _._2.ident!="") 
        val okList   = okMissList._1.map( _._2.sno).toList
        val missList = okMissList._2.map( _._2.sno).toList

        result += ((coId, Right( (okList, missList) )))
      } // valid coId
    } 

    // trny.pl2co.foreach { case (key, entry) => {
    //   logger.info(s"coId: ${entry.coId} sno: ${entry.sno} ident: ${entry.ident}" ) 
    // }}
    result.to(Array)
    //Array( (1L, Left(Error("Competition not found").encode)), (2L, Right(List(177L, 188L))) )
  }

}