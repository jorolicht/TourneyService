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

    def accessError(cmd: String) = Error("err0080.access.invalidRights", "", "", cmd).encode
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
            
      // def regSingle(coId: Long, pl: Player): Future[Either[Error, String]]
      // registers a player with a competition, returns playerId
      case "regSingle" => {
        val pStatus = PantStatus(getParam(pMap, "status", PantStatus.UNKN.id))
        val coId = getParam(pMap, "coId", 0L)        
        val bulk = getParam(pMap, "bulk", false)    
        if (bulk) {
          try tsv.regSingle(coId, read[List[Player]](reqData), pStatus).map {
            case Left(err)      => BadRequest(err.add("regSingle").encode)
            case Right(snoList) => Ok(write[List[SNO]](snoList))
          } catch { case _: Throwable => Future(BadRequest(Error("err0240.svc.regSingle").encode)) }
        } else {
          try tsv.regSingle(coId, read[Player](reqData), pStatus).map {
            case Left(err) => BadRequest(err.add("regSingle").encode)
            case Right(sno) => Ok(write[SNO](sno))
          } catch { case _: Throwable => Future(BadRequest(Error("err0240.svc.regSingle").encode)) }
        }
      }  


      //def regDouble(coId: Long, pl1: Player, pl2: Player): Future[Either[Error, String]]
      case "regDouble" => {
        val pStatus = PantStatus(getParam(pMap, "status", PantStatus.UNKN.id))
        val coId = getParam(pMap, "coId", 0L)        
        val bulk = getParam(pMap, "bulk", false)    
        if (bulk) {
          try tsv.regDouble(coId, read[List[(Long,Long)]](reqData), pStatus).map {
            case Left(err)     => BadRequest(err.add("regDouble").encode)
            case Right(snoList) => Ok(write[List[SNO]](snoList))
          } catch { case _: Throwable => Future(BadRequest(Error("err0241.svc.regDouble").encode)) }
        } else {
          try tsv.regDouble(coId, read[(Long,Long)](reqData), pStatus).map {
            case Left(err)  => BadRequest(err.add("regDouble").encode)
            case Right(sno) => Ok(write[SNO](sno))
          } catch { case _: Throwable => Future(BadRequest(Error("err0241.svc.regDouble").encode)) }
        }
      } 

      //
      // Pant Action Routines (Pant could be Single,Double or Team (future) 
      // 

      // def setPant2Comp(coId: Long, plId: Player): Future[Either[Error, Pant2Comp]]
      // registers a player with a competition, returns Pant2Comp entry
      case "setPant2Comp" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        Pant2Comp.decode(reqData) match {
          case Left(err)  => Future(BadRequest(err.add("setPant2Comp").encode))
          case Right(p2c) => tsv.setPant2Comp(p2c).map {
            case Left(err)     => BadRequest(err.add("setPant2Comp").encode)
            case Right(p2cRes) => Ok(p2cRes.encode) 
          }
        } 
      } 
 

      // def delPant2Comp(coId: Long, sno: String): Future[Either[Error, Int]]
      // removes a player from the competition
      case "delPant2Comp" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.delPant2Comp(getParam(pMap, "coId", -1L), getParam(pMap, "sno", "XXXXX")).map {
          case Left(err)     => BadRequest(err.add("delPant2Comp").encode)
          case Right(result) => Ok(Return(result).encode) 
        }
      }      

      // setPantStatus sets the status of a Pant within a competition, returns status
      // def setPantStatus(coId: Long, sno: String, status: Int): Future[Either[Error, Int]]
      case "setPantStatus" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.setPantStatus(getParam(pMap, "coId", 0L), SNO(getParam(pMap, "sno")), PantStatus(getParam(pMap, "status", PantStatus.UNKN.id))).map {
          case Left(err)  => BadRequest(err.encode)
          case Right(res) => Ok(Return(res.id).encode)
        }
      } 

      // setPantBulkStatus sets the status of a participant within a competition, returns number of affected
      // def setPantBulkStatus(coId: Long, List[(String, Int)]): Future[Either[Error, Int]]
      case "setPantBulkStatus" => {
        implicit val pStatusReadWrite: upickle.default.ReadWriter[PantStatus.Value] =
          upickle.default.readwriter[Int].bimap[PantStatus.Value](x => x.id, PantStatus(_))

        if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
          val pantStatus = read[List[(String, PantStatus.Value)]](reqData)
          logger.warn(s"setPantBulkStatus => pants status: ${pantStatus}") 
          tsv.setPantBulkStatus(getParam(pMap, "coId", 0L), pantStatus).map {
            case Left(err)  => BadRequest(err.encode)
            case Right(res) => Ok(Return(res).encode)
          }
        }
      }  


      
      // setPantPlace sets the place of a participant within a competition, returns placement
      // setPantPlace(coId: Long, sno: String, place: String): Future[Either[Error, Placement]]
      case "setPantPlace" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.setPantPlace(getParam(pMap, "coId", 0L), SNO(getParam(pMap, "sno")), getParam(pMap, "place")).map {
          case Left(err)     => BadRequest(err.encode)
          case Right(result) => Ok(Placement.encode(result))          
        }  
      } 


      //
      // Playfield Action Routines
      //

      /** setPlayfield
       *  def setPlayfield(pf: Playfield): Future[Either[Error, Int]]
       */
      case "setPlayfield"    => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        Playfield.decode(reqData) match {
          case Left(err)   => Future( BadRequest(err.encode) )
          case Right(pf)   => tsv.setPlayfield(pf).map {
            case Left(err)   => BadRequest(err.encode)
            case Right(pfNr) => Ok(Return(pfNr).encode)
          }  
        }
      }   
      
      
      /** setPfieldInfo
       *  setPfieldInfo(pfi: PfieldInfo)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
       */
      case "setPfieldInfo"    => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        PfieldInfo.decode(reqData) match {
          case Left(err)    => Future(BadRequest(err.encode))
          case Right(pfi)   => tsv.setPfieldInfo(pfi)(tse).map {
            case Left(err)     => BadRequest(err.encode)
            case Right(res)    => Ok(Return(res).encode)
          }
        }
      } 

      // delete a playfield with code, returns number of deleted fields
      // def delPlayfield(code: String)(implicit tse :TournSVCEnv): Future[Either[Error, Int]]
      case "delPlayfield"    => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.delPlayfield(getParam(param, "no", -1), getParam(param, "code"), getParam(param, "verify", false) ).map {
          case Left(err)    => BadRequest(err.encode)
          case Right(pfNo)  => Ok(Return(pfNo).encode)
        }
      }  

      //
      // Tourney POST-Action Routines
      //  

      /** addTourney adds a tourney to the database
        * addTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]
        */
      case "addTourney" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        Tourney.decode(reqData) match {
          case Left(err)    => Future(BadRequest(err.encode))
          case Right(trny)  => tsv.addTourney(trny)(tse).map {
            case Left(err)     => BadRequest(err.encode)
            case Right(trnyId) => Ok(Return(trnyId).encode)
          }
        }
      }  


      /** delTourney deletes a tourney
       * addTourney(trny: Tourney)(implicit tse :TournSVCEnv): Future[Either[Error, Long]]
       */
      case "delTourney" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.delTourney(toId)(tse).map {
          case Left(err)     => BadRequest(err.add("delTourney").encode)
          case Right(result) => Ok(Return(result).encode)
        }
      } 


      // addTournBase adds a tourney (from a tournBase) to the database
      case "addTournBase" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        TournBase.decode(reqData) match {
          case Left(err)       => logger.error(s"addTournBase: ${err.msgCode}"); Future(BadRequest(err.encode))
          case Right(trnyBase) => tsv.addTournBase(trnyBase)(tse).map {
            case Left(err)        => logger.error(s"addTournBase: ${err.msgCode}"); BadRequest(err.add("addTournBase").encode)
            case Right(trny)      => Ok(trny.encode())
          }
        }
      }  
    

      // sync (overwrite) tourney and save to disk
      case "syncTourney" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        Tourney.decode(reqData) match {
          case Left(err)   => logger.error(s"syncTourney: ${err.msgCode}"); Future(BadRequest(err.encode))
          case Right(trny) => tsv.syncTourney(trny)(tse).map {
            case Left(err)     => logger.error(s"syncTourney: ${err.msgCode}"); BadRequest(err.add("syncTourney").encode)
            case Right(res)    => Ok(Return(res).encode)
          }
        }
      }  

      
      // save tourney to disk
      case "saveTourney" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.saveTourney(toId) match {
          case Left(err)  => Future(BadRequest(err.encode))
          case Right(res) => Future(Ok(Return(res).encode))
        }
      }
  

      // setTournBase sets a tourney (from a tournBase) to the database
      case "setTournBase" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        TournBase.decode(reqData) match {
          case Left(err)   => Future( BadRequest(err.encode) ) 
          case Right(tb)   => tsv.setTournBase(tb)(tse).map {
              case Left(err)   => BadRequest(err.add("setTournBase").encode) 
              case Right(trny) => Ok(trny.encode())
          }
        }
      }
 

      // addTournBase adds tourney based on click TT data
      case "addTournCTT" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
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



      //
      // Competition Action Routines
      //
      /** setComp updates a competition, returns either error or the competition
        * calls setComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Competition]]        
        */
      case "setComp"   => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else {    
        Competition.decode(reqData) match {
          case Left(err)   => Future(BadRequest(err.encode))
          case Right(co)   => tsv.setComp(co)(msgs, tse).map { 
            case Left(err)    => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.add(s"${cmd}").encode)  
            case Right(newCo) => logger.info(s"${cmd}: execution Ok");    Ok(newCo.encode) 
          }
        }
      }
     

      /** addComp creates  a competition, returns either error or the competition
        * calls addComp(co: Competition)(implicit msgs: Messages, tse :TournSVCEnv):Future[Either[Error, Competition]]        
        */
      case "addComp"   => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        Competition.decode(reqData) match {
          case Left(err)   => Future(BadRequest(err.encode))
          case Right(co)   => tsv.addComp(co)(msgs, tse).map { 
            case Left(err)    => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.add(s"${cmd}").encode) 
            case Right(newCo) => logger.info(s"${cmd}: execution Ok");    Ok(newCo.encode)
          }
        }
      } 


      /** setCompStatus sets the competition status, returns true if status is set, otherwise false
        * calls setCompStatus(coId: Long, status: Int)(implicit tse: TournSVCEnv): Future[Either[Error, Boolean]]        
        */
      case "setCompStatus"  => {
        import shared.model.Competition._
        if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
          tsv.setCompStatus(getParam(pMap, "coId", -1L), CompStatus(getParam(pMap, "status", CompStatus.UNKN.id)) ).map {
            case Left(err)   => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.add(s"${cmd}").encode)
            case Right(res)  => logger.info(s"${cmd}: execution Ok"); Ok("")
          } 
        }
      }     
      
      
      /** delComp deletes a competition
        * calls delComp(coId: Long)(implicit tse :TournSVCEnv): Future[Either[Error, Boolean]]      
        */
      case "delComp"  => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.delComp(getParam(pMap, "coId", -1L)).map { 
          case Left(err)   => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
          case Right(res)  => logger.info(s"${cmd}: execution Ok");    Ok(Return(res).encode)
        }           
      }

      case "addCompPhase"   => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        val name    = getParam(pMap, "name", "")
        val coPhCfg = CompPhaseCfg(getParam(pMap, "coPhCfg", CompPhaseCfg.UNKN.id))
        tsv.addCompPhase(getParam(pMap, "coId", -1L), name).map { 
            case Left(err)      => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
            case Right(newCoPh) => logger.info(s"${cmd}: execution Ok"); Ok(newCoPh.encode())
        }
      }
      
      case "delCompPhase"   => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        tsv.delCompPhase(getParam(pMap, "coId", -1L), getParam(pMap, "coPhId", -1)).map { 
            case Left(err)  => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
            case Right(res) => logger.info(s"${cmd}: execution Ok"); Ok("")
        }
      }

      case "saveCompPhase"   => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else {
        CompPhase.decode(reqData) match {
          case Left(err)  => Future(BadRequest(err.encode))
          case Right(coph) => tsv.setCompPhase(coph).map { 
            case Left(err)  => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
            case Right(res) => logger.info(s"${cmd}: execution Ok"); Ok("")
          }
        }
      }  


      //
      // PLAYER Routines
      //
      // addPlayer adds a player to the database
      case "addPlayer" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        Player.decode(reqData) match {
          case Left(err)      => Future(BadRequest(err.encode))
          case Right(player)  => tsv.addPlayer(player)(tse).map {
            case Left(err)    => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
            case Right(pl)    => logger.info(s"${cmd}: execution Ok");    Ok(pl.encode)
          }
        }
      }  
  

      // setPlayer set player
      case "setPlayer" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else {                            
        getParam(pMap, "plId", -1L) match {

          case plId if (plId > (-1L)) => tsv.setPlayer(plId, CttLicense(getParam(pMap, "license", ""))).map {
            case Left(err)    => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
            case Right(pl)    => logger.info(s"${cmd}: execution Ok");    Ok(pl.encode)
          }

          case _      => Player.decode(reqData) match {
            case Left(err)      => Future(BadRequest(err.encode))
            case Right(player)  => tsv.setPlayer(player)(tse).map {
              case Left(err)    => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
              case Right(pl)    => logger.info(s"${cmd}: execution Ok");    Ok(pl.encode)
            }
          }    

        }
      }

      // resetMatch
      // def resetMatch(coId: Long, coPhId:Int, gameNo: Int)(implicit tse :TournSVCEnv): Future[Either[Error, List[Int]]] 
      case "resetMatch" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        val coId   = getParam(pMap, "coId", 0L)
        val coPhId = getParam(pMap, "coPhId", 0)
        val gameNo = getParam(pMap, "gameNo", 0)
        val resetPantA = getParam(pMap, "resetPantA", false)
        val resetPantB = getParam(pMap, "resetPantB", false)        

        tsv.resetMatch(coId, coPhId, gameNo, resetPantA, resetPantB)(tse).map {
          case Left(err)    => BadRequest(err.encode)
          case Right(gList) => Ok( write[List[Int]](gList) )
        }
      } 

      // resetMatches
      // def resetMatches(coId: Long, coPhId:Int)(implicit tse :TournSVCEnv): Future[Either[Error,List[Int]]]  
      case "resetMatches" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        val coId   = getParam(pMap, "coId", 0L)
        val coPhId = getParam(pMap, "coPhId", 0)     

        tsv.resetMatches(coId, coPhId)(tse).map {
          case Left(err)    => BadRequest(err.encode)
          case Right(gList) => Ok( write[List[Int]](gList) )
        }
      } 


      // inputReferee = inputMatch without overwrite functionality
      case "inputReferee" => {
        val coId   = getParam(pMap, "coId", 0L)
        val coPhId = getParam(pMap, "coPhId", 0)
        val gameNo = getParam(pMap, "gameNo", 0)
        
        var data: ((Int,Int),String,String,String) =((0,0),"","","")
        try   data = read[( (Int, Int), String, String, String) ](reqData)
        catch { case _ :Throwable => data = ((0,0),"","","") }
  
        val sets      = data._1
        val result    = data._2
        val info      = data._3
        val playfield = data._4

        logger.info(s"${cmd}: param: ${sets} ${result} ${info} ${playfield}")

        tsv.inputMatch(toId, coId, coPhId, gameNo, sets, result, info, playfield, false).map {
          case Left(err)    => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
          case Right(gList) => logger.info(s"${cmd}: execution Ok");    Ok( write[List[Int]](gList) )
        }
      }

      // inputMatch
      case "inputMatch" => {
        val coId      = getParam(pMap, "coId", 0L)
        val coPhId    = getParam(pMap, "coPhId", 0)
        val gameNo    = getParam(pMap, "gameNo", 0)
        val overwrite = getParam(pMap, "overwrite", false)
        
        var data: ((Int,Int),String,String,String) =((0,0),"","","")
        try   data = read[( (Int, Int), String, String, String) ](reqData)
        catch { case _ :Throwable => data = ((0,0),"","","") }
  
        val sets      = data._1
        val result    = data._2
        val info      = data._3
        val playfield = data._4

        logger.info(s"${cmd}: param: ${sets} ${result} ${info} ${playfield}")

        tsv.inputMatch(toId, coId, coPhId, gameNo, sets, result, info, playfield, overwrite).map {
          case Left(err)    => logger.error(s"${cmd}: ${err.encode}" ); BadRequest(err.encode)
          case Right(gList) => logger.info(s"${cmd}: execution Ok");    Ok( write[List[Int]](gList) )
        }
      }

      


      //
      // Click TT Action Routines
      //
      case "genCttResult" => if (!chkAccess(ctx)) Future(BadRequest(accessError(cmd))) else { 
        import scala.collection.mutable.ArrayBuffer
        import scala.xml.transform.RewriteRule
        import scala.xml.transform.RuleTransformer

        tsv.getTourney(toId).map {
          case Left(err)   => { logger.error(s"genCttResult: ${err.encode}" ); BadRequest(err.add("genCttResult").encode) }
          case Right(trny) => {
            val tourneyDir = s"${env.rootPath}${File.separator}db${File.separator}Tourneys${File.separator}${ctx.orgDir}"
            val cttFN    = s"${tourneyDir}${File.separator}${trny.startDate}_${toId}_participants.xml"
            val cttResFN = s"${tourneyDir}${File.separator}${trny.startDate}_${toId}_result.xml"

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
          case CompTyp.SINGLE =>
            trny.pl2co.filter(_._1._2 == coId).foreach { case (key, entry) => {
              val plId    = entry.getSingleId
              val lic     = trny.players(plId).getLicense.value
              entry.ident = licence2player(lic)
            }}

          case CompTyp.DOUBLE =>
            trny.pl2co.filter(_._1._2 == coId).foreach { case (key, entry) => {

              val plIds  = entry.getDoubleId match {
                case Left(err) => println(s"ERROR: ${err.toString}"); (0L,0L)
                case Right(id) => id
              }
              val lic1   = trny.players(plIds._1).getLicense.value
              val lic2   = trny.players(plIds._2).getLicense.value

              entry.ident = 
                if (licence2player.isDefinedAt(s"${lic1}·${lic2}")) {
                  licence2player(s"${lic1}·${lic2}")
                } else if (licence2player.isDefinedAt(s"${lic2}·${lic1}")) {
                  licence2player(s"${lic2}·${lic1}")
                } else { "" }  
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