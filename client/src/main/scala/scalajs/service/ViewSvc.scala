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
import shared.model.Competition._
import scalajs.usecase.component.BasicHtml
import shared.utils._

import scalajs.{ App, AppEnv }

trait ViewServices extends BasicHtml {


 /** genSingleTblData
   *  returns Sequence of Single Participants (sno, lastname, firstname, ttr, clubname, birthyear, status) 
   */
  def genSingleTblData(trny: Tourney, coId: Long): Seq[(String, String, String, String, String, Int, Int)] = {
    val p2cs = if (coId > 0) trny.pl2co.values.filter(_.coId == coId).toSeq else trny.pl2co.values.toSeq
    (for { p2ce <- p2cs } yield {
      val plId = p2ce.getPlayerId
      if (trny.players.contains(plId)) {
        val player   = trny.players(plId)
        (p2ce.sno, player.lastname, player.firstname, player.getTTR, player.clubName, player.birthyear, p2ce.status.id)
      } else {
        ("0", "", "", "", "", 0, 0)
      }
    }).toSeq.sortBy(s => (s._2,s._3))
  }

  /** genDoubleTblData
   *  returns Sequence of Double Participants (sno, firstname1, firstname2, clubname1, clubname2, status) 
   */
  def genDoubleTblData(trny: Tourney, coId: Long): Seq[(String, String, String, String, String, Int)] = {
    val p2cs = if (coId > 0) trny.pl2co.values.filter(_.coId == coId).toSeq else trny.pl2co.values.toSeq
    (for {
      p2ce <- p2cs
    } yield {
       p2ce.getDoubleId match {
         case Left(err) => println(s"ERROR: ${err.toString()}"); ("0·0", "", "", "", "", 0)
         case Right(id) => if (trny.players.contains(id._1) & trny.players.contains(id._2)) {
           (p2ce.sno, trny.players(id._1).lastname, trny.players(id._2).lastname, trny.players(id._1).clubName, trny.players(id._2).clubName, p2ce.status.id)
         } else { println(s"ERROR: invalid double sno ${id}"); ("0·0", "", "", "", "", 0) }
       }
    }).toSeq.sortBy(s => (s._2,s._3))
  }


  /** list all competition and the corresponding players
   *  @return: Seq(CoId, CoName, Seq(Position, PlaceString, PlayerName, ClubName, PlayerIdStr))            
   */
  def viewPlacements(tourney: Tourney): Seq[(Long, String, Seq[(Int, String, String, String, String)])] = {

    def getPlacements(coId: Long, cTyp: CompTyp.Value, tourney: Tourney): Seq[(Int, String, String, String, String)] = {
      (for {
        (sno_coId, pa2co) <- tourney.pl2co
      } yield {
        //debug(s"viewPlacments: ${sno_coId} ${tourney.comps(sno_coId._2).name}")
        if (sno_coId._2 == coId) {
          cTyp match {
             case CompTyp.SINGLE => {
               val plId = pa2co.getSingleId
               val pl = pa2co.getPlace()._1
               if (pl != 0) (pl, tourney.getPantPlace(pa2co.placement), tourney.players(plId).getName(), tourney.players(plId).clubName, pa2co.sno)
               else (0, "0", "", "", "0")

             }
             case CompTyp.DOUBLE | CompTyp.MIXED => {
               pa2co.getDoubleId match {
                 case Left(err) => (0, "0", "", "", "0")
                 case Right(id) => {
                  (pa2co.getPlace()._1, tourney.getPantPlace(pa2co.placement), 
                    s"${tourney.players(id._1).lastname}/${tourney.players(id._2).lastname}",
                    s"${tourney.players(id._1).clubName}/${tourney.players(id._2).clubName}",
                    pa2co.sno)
                 } 
               }
             }
             case _            => (0, "0", "", "", "0")
          }
        } else {
          (0, "0", "", "", "0")
        }
      }).toSeq.filter(_._1 > 0).sortBy(_._1)
    }


    (for {  
      (coId, comp) <- App.tourney.comps
    } yield { 
      if (comp.getCertCoPhId != None) (coId, comp.name, getPlacements(coId, comp.typ, tourney)) else (coId, comp.name, Seq())
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
    AppEnv.info("genPlayerOption", s"plIds: ${plIds} pl2co: ${pl2co} diffSet: ${diffSet}     ")

    "<option value='0'>---</option>"  + (for ((id) <- diffSet) yield s"""<option value="${id}">${trny.players(id).getName(1)} (${trny.players(id).getClub(0)})</option>""").mkString(" ")
  }

  def freePlayers(trny: Tourney, coId: Long, skipId: Long): Seq[Long] = {
    import shared.utils.Routines._
    // list all players minus players which are already registered and remove also player with skipId (if existing)
    val plIds = trny.players.keySet
    val pl2co = trny.pl2co.keySet.filter( ((x)) => x._2==coId ).map( ((x)) => getMDLongArr(x._1).toSeq).flatten
    plIds.diff(pl2co + skipId).toSeq
  }  



  // show results of a group
  def showGrResult(coId: Long, coPhId: Int, group: Group) = {
    //debug("showGrResult", s"APP__GrRound_${coId}_${coPhId} for Group: ${group.grId}")
    for(i <- 0 until group.size) {
      for(j <- 0 until group.size) {
        if(i!=j){
          val res = if(group.results(i)(j).valid) { group.results(i)(j).sets._1.toString + ":" + group.results(i)(j).sets._2.toString } else { "&nbsp;" }
          setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Set_${group.grId}_${i+1}_${j+1}"), res)
          //debug("showGrResult",s"APP__GrRound_${coId}_${coPhId}_Set_${group.grId}_${i+1}_${j+1} -> ${res}")
        }
      }

      val balls  = group.balls(i)._1.toString + ":" + group.balls(i)._2.toString  
      val sets   = group.sets(i)._1.toString + ":" + group.sets(i)._2.toString  
      val points = group.points(i)._1.toString + ":" + group.points(i)._2.toString
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Balls_${group.grId}_${i}"), balls)
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Sets_${group.grId}_${i}"), sets)
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Points_${group.grId}_${i}"), points)
      setHtml(gE(s"APP__GrRound_${coId}_${coPhId}_Places_${group.grId}_${i}"), group.pants(i).place._1.toString)
    } 
  } 


  // show results of ko round
  def showKoResult(coId: Long, coPhId: Int, koRound: KoRound) = {
    //debug("showKoResult", s"coId: ${coId} coPhId: ${coPhId}")
    for (index <- 0 to koRound.size-2) {
      val result     = koRound.results(index) 
      val rnd        = result.pos._1
      val game       = result.pos._2
      val playerName = {
        if      (result.sets._1 == koRound.noWinSets) { koRound.getPlayerKoViewName(result.sno._1) } 
        else if (result.sets._2 == koRound.noWinSets) { koRound.getPlayerKoViewName(result.sno._2)} 
        else                                          { "&nbsp;" } 
      }

      // propagate result to next position of player
      if (result.valid) {
        //debug("showKoResult",s"valid ${index}: APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game} -> ${playerName}")

        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game}"), playerName)

        // print result only if it's not against a dummy player 
        if (result.sno._1 != SNO.BYE & result.sno._2 != SNO.BYE) {
          setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetA_${rnd}_${game}"), result.sets._1.toString)
          setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetB_${rnd}_${game}"), result.sets._2.toString)
          val balls = result.balls.mkString(",")
          setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Balls_${rnd}_${game}"), if (balls != "") s"(${balls})" else "&nbsp;" )
        }
      } else {
        val (rnd,game) = koRound.getRndManoFromIndex(index)
        //debug("showKoResult",s"invalid ${index}: APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game}")

        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Winner_${rnd}_${game}"), "&nbsp;")
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetA_${rnd}_${game}"), "&nbsp;")
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_SetB_${rnd}_${game}"), "&nbsp;")
        setHtml(gE(s"APP__KoRound_${coId}_${coPhId}_Balls_${rnd}_${game}"), "&nbsp;")      
      } 
    }  
  }  

  // showRrResult - show round robin result
  //                for size <= 12 it's like a group result
  //                for size >  12 it's a kind of (sorted) list view
  def showRrResult(coId: Long, coPhId: Int, group: Group) = {
    if (group.size <= 12) showGrResult(coId, coPhId, group) else {
      (for(i<-0 until group.size) yield {
        (group.pants(i).place._1, group.pants(i).name, group.pants(i).club, 
         group.balls(i)._1.toString + ":" + group.balls(i)._2.toString, 
         group.sets(i)._1.toString + ":" + group.sets(i)._2.toString,
         group.points(i)._1.toString + ":" + group.points(i)._2.toString)
      }).sortBy(_._1).zipWithIndex.map { e => {
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Places"), e._1._1.toString)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Name"),   e._1._2)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Club"),   e._1._3)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Balls"),  e._1._4)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Sets"),   e._1._5)
         setHtml(gE(s"RRView_${coId}_${coPhId}_${e._2}_Points"), e._1._6)
      }}
    }
  }  



}