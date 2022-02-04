package scalajs.service

import scala.concurrent.duration._
import scala.concurrent._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom.ext._             // import stmt sequence is important
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"

import shared.model._
import shared.utils._
import shared.utils.Constants._


import scalajs.{ App, AppEnv }
import scalajs.usecase.Helper


trait ViewServices {


 /** genSingleTblData
   *  returns Sequence of Single Participants (sno, lastname, firstname, clubname, birthyear, status) 
   */
  def genSingleTblData(coId: Long): Seq[(String, String, String, String, String, Int, Int)] = {
    val tourney = App.tourney
    val p2cs = if (coId > 0) tourney.pl2co.values.filter(_.coId == coId).toSeq else tourney.pl2co.values.toSeq
    (for { p2ce <- p2cs } yield {
      val plId = p2ce.getPlayerId
      if (tourney.players.contains(plId)) {
        val player   = tourney.players(plId)
        (p2ce.sno, player.lastname, player.firstname, player.getTTR, player.clubName, player.birthyear, p2ce.status)
      } else {
        ("0", "", "", "", "", 0, 0)
      }
    }).toSeq.sortBy(s => (s._2,s._3))
  }

  /** genDoubleTblData
   *  returns Sequence of Double Participants (sno, firstname1, firstname2, clubname1, clubname2, status) 
   */
  def genDoubleTblData(coId: Long): Seq[(String, String, String, String, String, Int)] = {
    val tourney = App.tourney
    val p2cs = if (coId > 0) tourney.pl2co.values.filter(_.coId == coId).toSeq else tourney.pl2co.values.toSeq
    (for {
      p2ce <- p2cs
    } yield {
       val plId1 = p2ce.getPlayerId1
       val plId2 = p2ce.getPlayerId2
       if (tourney.players.contains(plId1) & tourney.players.contains(plId2)) {
        (p2ce.sno, 
        tourney.players(plId1).lastname, 
        tourney.players(plId2).lastname, 
        tourney.players(plId1).clubName,
        tourney.players(plId2).clubName, p2ce.status)
       } else {
        ("0·0", "", "", "", "", 0)
       }
    }).toSeq.sortBy(s => (s._2,s._3))
  }
    

 /** view4PlayerRegister
   *  returns Sequence of Player (id, name, clubname, status, coName, coId, coTyp, coStatus, email) 
   */
  def view4PlayerRegister(): Seq[(Long, String, String, String, Int, String, Long, Int, Int, String)] = {
    val tourney = App.tourney
    
    // list for single players
    val pList = (for {
      p2ce  <- tourney.pl2co.values.toSeq
      comp   = tourney.comps(p2ce.coId) 
    } yield {
      if (comp.typ == 1) {
        val pl1 = tourney.players(p2ce.getPlayerId)
        (pl1.id, pl1.id.toString, 
         s"${pl1.lastname} , ${pl1.firstname}", 
         pl1.clubName, p2ce.status, 
         comp.name, comp.id, comp.typ, comp.status, pl1.email)
      } else {
        val pl1 = tourney.players(p2ce.getPlayerId1)
        val pl2 = tourney.players(p2ce.getPlayerId2)
        (pl1.id * 1000000 + pl2.id, s"${pl1.id}/${pl2.id}", 
         s"${pl1.lastname}/${pl2.lastname}", 
         s"${pl1.clubName}/${pl2.clubName}", p2ce.status,
         comp.name, comp.id, comp.typ, comp.status, pl1.email)
      }
    }).toSeq
    pList
  }


  /** list all competition and the corresponding players
   *  @return: Seq(CoId, CoName, Seq(Position, PlaceString, PlayerName, ClubName, PlayerIdStr))            
   */
  def viewPlacements(): Seq[(Long, String, Seq[(Int, String, String, String, String)])] = {

    def getPlacements(coId: Long, cTyp: Int, tourney: Tourney): Seq[(Int, String, String, String, String)] = {
      (for {
        (sno_coId, pa2co) <- App.tourney.pl2co
      } yield {
        //debug(s"viewPlacments: ${sno_coId} ${tourney.comps(sno_coId._2).name}")
        if (sno_coId._2 == coId) {
          cTyp match {
             case CT_SINGLE => {
               val plId = pa2co.getSingleId
               /*
               debug(s"viewPlacments0: ${plId.toString}")
               debug(s"viewPlacments1: ${pa2co.getPlace()}")
               debug(s"viewPlacments2: ${pa2co.getPlaceDesc(App.getMessage _)}")
               debug(s"viewPlacments3: ${tourney.players(plId).getName()}")
               debug(s"viewPlacments4: ${tourney.players(plId).clubName}")
               debug(s"viewPlacments5: ${pa2co.sno}")
               */
               (pa2co.getPlace()._1, pa2co.getPlaceDesc(AppEnv.getMessage _), 
                tourney.players(plId).getName(),
                tourney.players(plId).clubName,
                pa2co.sno)
             }
             case CT_DOUBLE | CT_MIXED => {
               val (plId1, plId2) = pa2co.getDoubleId

               Helper.debug("getPlacements", s"double: ${plId1.toString}  ${plId2.toString}")
               (pa2co.getPlace()._1, pa2co.getPlaceDesc(AppEnv.getMessage _), 
                s"${tourney.players(plId1).lastname}/${tourney.players(plId2).lastname}",
                s"${tourney.players(plId1).clubName}/${tourney.players(plId2).clubName}",
                pa2co.sno)
             }
             case CT_TEAM => (0, "0", "", "", "0")
             case _       => (0, "0", "", "", "0")
          }
        } else {
          (0, "0", "", "", "0")
        }
      }).toSeq.filter(_._1 > 0).sortBy(_._1)
    }


    (for {  
      (coId, comp) <- App.tourney.comps
    } yield { 
      if (comp.status == CS_ERFIN) {
        (coId, comp.name, getPlacements(coId, comp.typ, App.tourney))
      } else {
        (coId, comp.name, Seq())
      }
    }).toSeq.filter(_._3.length>0)
  }


  /** genPlayerOption generate player option with value id
   *  @return: html input option         
   */
  def genPlayerOption(name: String, trny: Tourney, coId: Long, setId: Long, skipId: Long): String = {
    import shared.utils.Routines._

    // list all players minus players which are already registered and remove also player with skipId (if existing)
    val plIds = trny.players.keySet
    val pl2co = trny.pl2co.keySet.filter( ((x)) => x._2==coId ).map( ((x)) => getMDLongArr(x._1).toSeq).flatten
    val diffSet = plIds.diff(pl2co + skipId)
    Helper.info("genPlayerOption", s"plIds: ${plIds} pl2co: ${pl2co} diffSet: ${diffSet}     ")

    "<option value='0'>---</option>"  + (for ((id) <- diffSet) yield s"""<option value="${id}">${trny.players(id).getName(1)} (${trny.players(id).getClub(0)})</option>""").mkString(" ")
  }

  def freePlayers(trny: Tourney, coId: Long, skipId: Long): Seq[Long] = {
    import shared.utils.Routines._
    // list all players minus players which are already registered and remove also player with skipId (if existing)
    val plIds = trny.players.keySet
    val pl2co = trny.pl2co.keySet.filter( ((x)) => x._2==coId ).map( ((x)) => getMDLongArr(x._1).toSeq).flatten
    plIds.diff(pl2co + skipId).toSeq
  }  


}