package scalajs.usecase.organize

// Start TestCases in Javascript Console
// Start.testOrgCompDraw("<toId>")

import scala.collection.mutable.{ ArrayBuffer }
import scala.concurrent._
import scala.util.{Success, Failure }
import scala.util.matching

import scala.scalajs._
import scala.scalajs.js.annotation._
import scala.scalajs.js.Dynamic.global
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import org.querki.jquery._               // from "org.querki" %%% "jquery-facade" % "1.2"
import org.scalajs.dom                   // from "org.scala-js" %%% "scalajs-dom" % "0.9.3"
import upickle.default._

import shared.model._
import shared.model.CompPhase._ 
import shared.utils.Validation._ 
import shared.utils.Routines._
import shared.utils.Constants._
import shared.utils._

import scalajs.usecase.dialog.{ DlgBox }
import scalajs.usecase.component._
import scalajs.service._
import scalajs._


// ***
// Organize Competition Draw
// ***
@JSExportTopLevel("OrganizeCompetitionDraw")
object OrganizeCompetitionDraw extends UseCase("OrganizeCompetitionDraw")  
  with TourneySvc
{
  import org.scalajs.dom.raw.HTMLElement
  import scala.collection.mutable.ListBuffer

  def render(param: String = "", ucInfo: String = "", reload: Boolean=false) = {
    OrganizeCompetitionTab.render("Draw")
  }


  @JSExport 
  override def actionEvent(key: String, elem: dom.raw.HTMLElement, event: dom.Event) = {
    debug("actionEvent", s"key: ${key}")
    key match {
      case "DrawRefresh"   => {
        debug("Refresh", s"key: ${key}")

        // set coId if a new is available
        val (coId, coPhId)  = (getData(elem, "coId", 0L), getData(elem, "coPhId", 0))
        val drawElements = getElemById_(s"Draw_${coId}_${coPhId}").querySelectorAll("td[data-drawPos]")
        val asign = (for ( i <- 0 to drawElements.length-1) yield {
          val elem = drawElements.item(i).asInstanceOf[HTMLElement]
          (elem.getAttribute("data-drawPos").toInt, elem.innerText.toIntOption.getOrElse(0))
        }).unzip
        val diff = asign._1.toSet.diff(asign._2.toSet)
        if      (diff.size == 0) reassignDraw(App.tourney.cophs(coId, coPhId), asign._1.zip(asign._2).toMap)
        else if (diff.size == 1) DlgBox.showStd(getMsg("change.hdr"), getMsg("change.msg", diff.head.toString), Seq("ok"))
        else                     DlgBox.showStd(getMsg("change.hdr"), getMsg("changex.msg", diff.mkString(",")), Seq("ok"))
      } 
    }
  }


  //update view for draw, input player list with (pos, SNO, Name, Club, TTR)
  def update(coId: Long, coPhId: Int)(implicit trny: Tourney) = {
    // first get the base element identified by coId and coPhId
    val base = getElemById_(s"Draw_${coId}_${coPhId}").asInstanceOf[HTMLElement]

    // generate draw frame
    trny.cophs(coId, coPhId).coPhTyp match {
      case CPT_GR => updateGrView(base, trny.cophs(coId, coPhId).groups)  
      case CPT_KO => updateKoView(base, trny.cophs(coId, coPhId).ko)
      case CPT_SW => {}
      case _      => {}
    }
  }


  // init frame for a competition, coId != 0 and coPhId != 0
  def init(coId: Long, coPhId: Int)(implicit trny: Tourney): Unit = {
    debug("init", s"coId: ${coId} coPhId: ${coPhId}")
    if (!exists_(s"Draw_${coId}_${coPhId}")) {
      val elem    = getElemById_(s"DrawContent_${coId}").querySelector(s"[data-coPhId='${coPhId}']")
      val size    = trny.cophs(coId, coPhId).size
      val coPhTyp = trny.cophs(coId, coPhId).coPhTyp
      // generate draw frame
      coPhTyp match {
        case CPT_GR => elem.innerHTML = clientviews.organize.competition.draw.html.GroupCard(coId, coPhId, trny.cophs(coId, coPhId).groups).toString
        case CPT_KO => elem.innerHTML = clientviews.organize.competition.draw.html.KOCard(coId, coPhId, trny.cophs(coId, coPhId).ko).toString
        case CPT_SW => elem.innerHTML = clientviews.organize.competition.draw.html.SwitzCard(coId, coPhId, size).toString
        case _      => elem.innerHTML = showAlert(getMsg("invalidSection"))
      }
    }
  }

  def setDrawPosition(elem: HTMLElement, pant: ParticipantEntry, pantPos: String="") = try {
    setData(elem, "sno", pant.sno)
    elem.querySelector(s"[data-name]").asInstanceOf[HTMLElement].innerHTML = pant.name
    elem.querySelector(s"[data-club]").asInstanceOf[HTMLElement].innerHTML = pant.club
    elem.querySelector(s"[data-rating]").asInstanceOf[HTMLElement].innerHTML = pant.getRating

    // reset drawpos to original value    
    val drawPosElem = elem.querySelector(s"[data-drawPos]").asInstanceOf[HTMLElement]
    drawPosElem.innerHTML = getData(drawPosElem, "drawPos")
  } catch { case _: Throwable => error("setDrawPosition ", s"Pos: ${pantPos} Pant: ${pant.sno} ${pant.name} [${pant.club}]") }


  def updateGrView(base: HTMLElement, groups: ArrayBuffer[Group]) =
    for (g <- groups) {
      g.pants.zipWithIndex.foreach { case (pant, index) => {
        val pantBase = base.querySelector(s"[data-pantPos='${g.grId}_${index}']").asInstanceOf[HTMLElement]
        setDrawPosition(pantBase, pant, s"${g.grId}_${index}")
      }}
    } 

  def updateKoView(base: HTMLElement, ko: KoRound) =
    ko.pants.zipWithIndex.foreach { case (pant, index) => {
      val pantBase = base.querySelector(s"[data-pantPos='${index+1}']").asInstanceOf[HTMLElement]
      setDrawPosition(pantBase, pant, s"${index+1}")
    }}


  // reassingDraw set new draw
  def reassignDraw(coph: CompPhase, reassign: Map[Int,Int])= {
    val pants = Array.fill[ParticipantEntry](coph.size)(ParticipantEntry("0", "", "", 0, (0,0)))
    val base = getElemById_(s"Draw_${coph.coId}_${coph.coPhId}").asInstanceOf[HTMLElement]   
    coph.coPhTyp match {
      case CPT_GR => {
        coph.groups.foreach { g => 
          g.pants.zipWithIndex.foreach { case (pant, index) => pants(reassign(g.drawPos + index) - 1) = pant }
        }     
        coph.groups.foreach { g => pants.slice(g.drawPos - 1, g.drawPos + g.size - 1).copyToArray(g.pants) }
        updateGrView(base, coph.groups) 
        //pants.zipWithIndex.foreach { case (pant, index) => println(s"[${index}] ${pant.name} ${pant.club} ${pant.getRating}") }
      }
      case CPT_KO => {} 
      case _      => {}
    }
  }


  def initDraw(coph: CompPhase, pants: ArrayBuffer[SNO])(implicit trny: Tourney): Future[Either[Error, Boolean]] = {
    coph.coPhCfg match {
      case CPC_GRPS3 | CPC_GRPS34 | CPC_GRPS4 | CPC_GRPS45 | CPC_GRPS5 | CPC_GRPS56 | CPC_GRPS6 => { 
        initGrDraw(coph, pants.map(_.getPantEntry(coph.coId)), Group.genGrpConfig(coph.coPhCfg, pants.size).toList) match {
          case Left(err)  => Future(Left(err))
          case Right(res) => initGrMatches(coph, trny.comps(coph.coId).typ) match {
            case Left(err)  => Future(Left(err))
            case Right(res) => coph.setStatus(CPS_AUS); Future(Right(res))
          }
        }
      }
      case CPC_KO | CPC_SW  => { 
        //coph.init(pants.map(_.getPantEntry(comps(coId).typ)(this))) 
        Future(Left(Error("???")))
      }  
      case _          => Future(Left(Error("???")))   
    }
  }


  // iniGrDraw - initialie draw for group configurations 
  def initGrDraw(coph: CompPhase, pants: ArrayBuffer[ParticipantEntry], grpCfg: List[GroupConfig]): Either[Error, Boolean] = {
    // generate groups
    coph.groups = ArrayBuffer[Group]() 
    for (gEntry <- grpCfg) { coph.groups = coph.groups :+ new Group(gEntry.id, gEntry.size, gEntry.quali, gEntry.name, coph.noWinSets) } 
    val noGroups = coph.groups.size

    // calculate average pant rating
    val (sum, cnt, maxRating) = pants.foldLeft((0,0,0))((a, e) => if (e.rating == 0) (a._1, a._2, a._3) else (a._1 + e.rating, a._2+1, e.rating.max(a._3) ) )
    val avgPantRating = sum/cnt

    // Step 0 - init effective rating, pant with no rating get average rating
    for (i <- 0 until pants.size) { pants(i).effRating = if (pants(i).rating == 0) avgPantRating else pants(i).rating   }

    // Step 1  - init club name occurence in pants
    val clubOccuMap = scala.collection.mutable.Map[String, Int]().withDefaultValue(0) 
    pants.map(pant => { if (pant.club != "") clubOccuMap(pant.club) = clubOccuMap(pant.club) + 1 } )
    for (i <- 0 until pants.size) { pants(i).occu = clubOccuMap(pants(i).club) }

    // Step 2 - position the best players, one in each group  (take given rating)
    val (pantsS1, pantsS2) = pants.sortBy(_.rating).reverse.splitAt(noGroups)
    for (i <- 0 until pantsS1.size) {  coph.groups(i).addPant(pantsS1(i), avgPantRating) }

    // Step 3 - position players with highest occurence and ascending rating
    //val (pantsS3, pantsS4) = pantsS2.sortBy(x => (pants.size + 1 - x.occu) * maxRating + x.effRating).splitAt(noGroups)
    //val pantsS3 = pantsS2.sortBy(x => (pants.size + 1 - x.occu) * maxRating + x.effRating)    
    val pantsS3 = pantsS2.sortBy(_.rating)

    for (i <- 0 until pantsS3.size) {
      val ratings = getMinOccBestAvg(pantsS3(i), coph.groups, pants.size, MAX_RATING, noGroups, avgPantRating)  
      // get index of biggest element
      val bestRatingPos = ratings.zipWithIndex.maxBy(_._1)._2
      coph.groups(bestRatingPos).addPant(pantsS3(i), avgPantRating)
    }

    // Step 3: sort group according to pant rating
    for (i <- 0 until noGroups) { coph.groups(i).pants.sortInPlaceBy(x => - x.rating) } 
    val lastPos = coph.groups.foldLeft(1){ (pos, g) =>  g.drawPos = pos; pos + g.size }
    Right(true)
  }


  // target function for descrete optimization
  def getMinOccBestAvg(pant: ParticipantEntry, grps: ArrayBuffer[Group], pantSize: Int, maxRating: Int, maxGrpSize: Int, pantAvgRating: Int): ArrayBuffer[Long] = {
    val result = ArrayBuffer.fill[Long](grps.size)(0)

    for (i <- 0 until grps.size) {
      result(i) = {
        if (grps(i).fillCnt == grps(i).size) {
          0L 
        } else {
          // calculate improvement of average rating 
          val curDiffRating = if (grps(i).avgRating==0) 0 else (pantAvgRating - grps(i).avgRating).abs 
          val newDiffRating = (pantAvgRating - ((grps(i).avgRating * grps(i).fillCnt + pant.effRating) / (grps(i).fillCnt+1))).abs
          val improveRating = maxRating + (curDiffRating - newDiffRating)

          // calculate free level
          val freeGrpLevel = (grps(i).size - grps(i).fillCnt)
          //val freeGrpLevel = (maxGrpSize - grps(i).cnt)

          // calculate occu level
          val occuLevel    = (pantSize - grps(i).occu(pant.club)) 
          println(s"Pant: ${pant} improvementRating: ${improveRating} freeGrpLevel: ${freeGrpLevel } occuLevel: ${occuLevel}")

          ((1000*occuLevel) + freeGrpLevel) * (2 * maxRating) + improveRating 
        }
      }
    }
    result
  }


  def initGrMatches(coph: CompPhase, coTyp: Int): Either[Error, Boolean] = {
    import shared.utils.GamePlan
    coph.matches = ArrayBuffer[MEntry]()

    try { coph.groups.foreach { g =>
      val gPE = GamePlan.Group(g.size)
      for (rnd <-1 to gPE.noRounds) { gPE.rounds(rnd-1).foreach { wgw =>
        coph.matches += MEntryGr.init(coph.coId, coTyp, coph.coPhId, coph.coPhTyp, 0, g.pants(wgw._1-1).sno, g.pants(wgw._2-1).sno, rnd, g.grId, wgw, coph.noWinSets)
      }}  
    }} catch { case _: Throwable => error("initGrMatches ", s"${coph.toString}"); Left(Error("??? initGrMatches")) }

    coph.matches = coph.matches.sortBy(r => (r.round, r.asInstanceOf[MEntryGr].grId))
    for (i <- 0 until coph.matches.size) { coph.matches(i).setGameNo(i+1) } 
    coph.genGrMatchDependencies() match {
      case Left(err)  => Left(err)
      case Right(res) => {
        for (i <- 0 until coph.matches.size) { if (coph.matches(i).asInstanceOf[MEntryGr].hasDepend) { coph.matches(i).setStatus(MEntry.MS_BLOCK)} } 
        Right(res)
      }
    }
 

  } 

}