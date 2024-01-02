package shared.model

import scala.util.matching
import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}

import shared.utils._
import shared.utils.Constants._
import shared.utils.Routines._

/** CompPhase describes a phase of a competition like first round
 *  intermediate round or final round. Every competition has at least
 *  a final round. Currently only two typs are supported ("group" or "ko") 
 *  number of players always less or equal size
 * 
 *  baseCoPh - Round is based on previous Round (eg. preliminary round)
 *  quali    - take as pre selection either winner-1, looser-2 or all-0 from previous round
 * 
 */

case class CompPhase(val name: String, val coId: Long, val coPhId: Int, var coPhCfg: CompPhaseCfg.Value, 
                     var status: CompPhaseStatus.Value, var demo: Boolean, var size: Int, var noPlayers: Int, 
                     var noWinSets: Int=0, var baseCoPhId: Option[Int]=None, var quali: QualifyTyp.Value = QualifyTyp.ALL) {
  import CompPhase._
  
  var candidates   = ArrayBuffer[(Pant, Boolean)]() 
  var candInfo     = ""
  var matches      = ArrayBuffer[MEntry]()
  var mFinished    = 0
  var mFix         = 0
  var mTotal       = 0
    
  var groups       = ArrayBuffer[Group]()   // groups of the competition (only gr rounds)
  var ko           = new KoRound(0, "", 0)  // ko games of ghe competition (only ko rounds)


  def getQualiPants(quali: QualifyTyp.Value): ArrayBuffer[(Pant, Boolean)] = coPhCfg match {
    case CompPhaseCfg.GRPS3  | CompPhaseCfg.GRPS34 | CompPhaseCfg.GRPS4 | CompPhaseCfg.GRPS45 | CompPhaseCfg.GRPS5 | CompPhaseCfg.GRPS56 | CompPhaseCfg.GRPS6 => {
      val pInfo = ArrayBuffer[(Pant, Boolean)]()
      for (grp <- groups) for (pant <- grp.pants) {
        val checked = quali match {
          case QualifyTyp.ALL => true
          case QualifyTyp.WIN => pant.place._1 <= grp.quali
          case QualifyTyp.LOO => pant.place._1 > grp.quali   
        }
        pInfo += ((pant.copy(qInfo=s"${grp.name} [${pant.place._1}]"), checked))
      } 
      pInfo
    }
    case _ => { 
      ArrayBuffer[(Pant, Boolean)]()
    }  
  }
  

  def getQualiInfo: String = coPhCfg match {
    case CompPhaseCfg.GRPS3  | CompPhaseCfg.GRPS34 | CompPhaseCfg.GRPS4 |
         CompPhaseCfg.GRPS45 | CompPhaseCfg.GRPS5 | CompPhaseCfg.GRPS56 | CompPhaseCfg.GRPS6 => "Group Pos"
    case _ => ""
  }

  // reset competition phase to status
  def reset(toStatus: CompPhaseStatus.Value) = getTyp match {
    case CompPhaseTyp.RR | CompPhaseTyp.SW => toStatus match {
      case CompPhaseStatus.CFG  => { // back to configuration
        mFinished = 0; mFix = 0; mTotal = 0
        matches   = ArrayBuffer[MEntry]()
        groups    = ArrayBuffer[Group]()
        status    = CompPhaseStatus.CFG
      }
      case CompPhaseStatus.AUS  => if (status == CompPhaseStatus.EIN) { status = CompPhaseStatus.AUS; resetMatches() }  
      case CompPhaseStatus.EIN  => if (status == CompPhaseStatus.EIN) resetMatches()
    }  

    case CompPhaseTyp.GR => toStatus match {
      case CompPhaseStatus.CFG  => { // back to configuration
        mFinished = 0; mFix = 0; mTotal = 0
        matches   = ArrayBuffer[MEntry]()
        groups    = ArrayBuffer[Group]()
        status    = CompPhaseStatus.CFG
      }
      case CompPhaseStatus.AUS  => if (status == CompPhaseStatus.EIN) { status = CompPhaseStatus.AUS; resetMatches() }  
      case CompPhaseStatus.EIN  => if (status == CompPhaseStatus.EIN) resetMatches()
    }  
    case CompPhaseTyp.KO => toStatus match {
      case CompPhaseStatus.CFG  => { // back to configuration
        mFinished = 0; mFix = 0; mTotal = 0
        matches   = ArrayBuffer[MEntry]()
        ko        = new KoRound(0, "", 0)
        status    = CompPhaseStatus.CFG
      }
      case CompPhaseStatus.AUS  => if (status == CompPhaseStatus.EIN) { status = CompPhaseStatus.AUS; resetMatches() }  
      case CompPhaseStatus.EIN  => if (status == CompPhaseStatus.EIN) resetMatches()
    }
    case _               => println(s"ERROR: CompPhase.reset -> invalid CompPhaseTyp" ) 
  }    


  //*****************************************************************************
  // Status Routines
  //*****************************************************************************
  def updateStatus():Unit = { 
    mFinished = matches.foldLeft(0) ((cnt, m) => if (m.finished) cnt + 1 else cnt)
    
    status match {
      case CompPhaseStatus.CFG  => println(s"ERROR: CompPhase.setStatus -> status=${status}" ) 
      case CompPhaseStatus.AUS  => if (mFinished == mTotal) println(s"ERROR: CompPhase.setStatus -> status=${status}" )   
      case CompPhaseStatus.EIN  => if (mFinished == mTotal) setStatus(CompPhaseStatus.FIN)
      case CompPhaseStatus.FIN  => if (mFinished < mTotal)  setStatus(CompPhaseStatus.EIN)   
    }
  }

  def setStatus(value: CompPhaseStatus.Value):Unit = { status = value }
  def getStatus             = status

  // getStatusText - only for debug purposes
  def getStatusTxt = status match {
      case CompPhaseStatus.UNKN  => "UNKNOWN"
      case CompPhaseStatus.CFG   => "CONFIG"
      case CompPhaseStatus.AUS   => "DRAW"  
      case CompPhaseStatus.EIN   => "INPUT"
      case CompPhaseStatus.FIN   => "FINISHED"  
    }

  //*****************************************************************************
  // Draw/Init Routines
  //*****************************************************************************
  def draw(compTyp: CompTyp.Value, baseCoPhs: ArrayBuffer[CompPhase], rnd: Int=0, option: Int=0): Either[Error, Int] = {

    val pants = candidates.filter(_._2).map { _._1 } // map to selected candidates
    noPlayers = pants.size

    coPhCfg match {
      // group system selected, for now don't care about previous rounds
      // intermediary round !?
      case CompPhaseCfg.GRPS3  | CompPhaseCfg.GRPS34 | CompPhaseCfg.GRPS4 |
           CompPhaseCfg.GRPS45 | CompPhaseCfg.GRPS5 | CompPhaseCfg.GRPS56 | 
           CompPhaseCfg.GRPS6 => drawGr(pants, compTyp, baseCoPhs)

      case CompPhaseCfg.KO    => drawKO(pants, compTyp, baseCoPhs)

      case CompPhaseCfg.RR    => drawRR(pants, compTyp, baseCoPhs)

      case CompPhaseCfg.SW    => drawSW(pants, compTyp, rnd, option)

      case _                  => Left(Error("")) 

    }
  }



  def reassignDraw(reassign: scala.collection.immutable.Map[Int,Int], compTyp: CompTyp.Value): Either[Error, Int] = {
    val pants = ArrayBuffer.fill[Pant](size)(Pant("0", "", "", 0, "", (0,0)))
    getTyp match {
      case CompPhaseTyp.GR | CompPhaseTyp.RR => {
        groups.foreach { g => g.pants.zipWithIndex.foreach { case (pant, index) => pants(reassign(g.drawPos + index) - 1) = pant }}     
        groups.foreach { g => pants.slice(g.drawPos - 1, g.drawPos + g.size - 1).copyToArray(g.pants) }
        initGrMatches(compTyp) match {
          case Left(err)  => Left(err)
          case Right(res) => setStatus(CompPhaseStatus.AUS); mTotal = matches.size; Right(mTotal) 
        }  
      }

      case CompPhaseTyp.KO => {
        ko.pants.zipWithIndex.foreach { case (pant, index) => pants(reassign(index+1)-1) = pant } 
        ko.pants = pants
        ko.sno2pos = scala.collection.mutable.Map[String, Int]()
        for (i <- 0 to size-1) ko.sno2pos += (ko.pants(i).sno -> i) 
        initKoMatches(compTyp) match {
          case Left(err)  => println(s"ERROR: initKoMatches ${err}"); Left(err)
          case Right(res) => setStatus(CompPhaseStatus.AUS);  mTotal = matches.size; mFix = res; Right(mTotal)
        } 
      } 
      case _      => Left(Error("??? invalid Typ"))
    }
  }  


  /** drawSW  - draw Switzer System based on RR/Group system
    *
    * @pants pants       Array of confirmed all possible pants (candidates)
    * @param compTyp     e.g. Single or Double
    * @param rnd         round starting at 1
    * @param option      swiss system options
    * @return            Number of games
    */
  def drawSW(confirmedPants: ArrayBuffer[Pant], compTyp: CompTyp.Value, rnd:Int, option: Int): Either[Error, Int] = {
    val pants = confirmedPants.sortBy(x => -x.rating)
    if (pants.size % 2 == 1) pants += Pant.bye()

    size = noPlayers
    groups = ArrayBuffer[Group]()

    if (size > Tourney.MaxSizeSW) Left(Error("invalid size")) else {
      println("drawSW")
      groups = groups :+ new Group(1, size, size/2, "", noWinSets)
      val noGroups = 1
      
      for (i <- 0 until noGroups) { groups(i).pants.sortInPlaceBy(x => - x.rating) } 
      groups(0).drawPos = 1
      val lastPos = groups(0).drawPos + groups(0).size

      for (i <- 0 until pants.size) {  groups(0).addPant(pants(i), 0) }
      initSwMatches(compTyp, rnd, option) match {
        case Left(err)  => Left(err)
        case Right(res) => {
          setStatus(CompPhaseStatus.AUS) 
          mTotal = matches.size 
          Right(mTotal) 
        }  
      }
    }
  }  


  /** drawKO  - draw KO Round based on previous rounds
    *
    * @pants pants       Array of confirmed all possible pants (candidates)
    * @param compTyp
    * @param baseCoPhs   Array of previous/base competition phase
    * @return            Number of games
    */
  def drawKO(pants: ArrayBuffer[Pant], compTyp: CompTyp.Value, baseCoPhs: ArrayBuffer[CompPhase]): Either[Error, Int] = {

    def drawKO_Grp(confirmedPants: ArrayBuffer[Pant], bCoPh: CompPhase, compTyp: CompTyp.Value) = {
      val confirmedSNOs = confirmedPants.map { _.sno }
      // println(s"drawKO_Group -> basis: ${bCoPh.name}" )
      val pantsInfo = (for(i <- 0 to bCoPh.groups.size-1; j <- 0 to bCoPh.groups(i).size-1) yield {
        val pant     = bCoPh.groups(i).pants(j)
        val grpName  = bCoPh.groups(i).name
        val grpId    = i + 1
        val pos      = pant.place._1
        val sets     = bCoPh.groups(i).sets(j)
        val points   = bCoPh.groups(i).points(j)
        val rank:Int = (50 + sets._1 - sets._2)*1000 + (50 + points._1 - points._2)
        (pant, (grpName, grpId, pos, rank))
      }).to(ArrayBuffer).filter(x => confirmedSNOs.contains(x._1.sno))

      // sortieren aufsteigend nach Position und absteigend nach Ranking
      // niedrigste Position (e.g. 1) mit höchsten Ranking zuerst.
      // danach Aufteilung in pants und drawInfo
      val (pants, grpInfo) = pantsInfo.sortBy(x=>(x._2._3,-x._2._4)).unzip(x=> (x._1, x._2))

      ko.initDraw_Grp(pants, grpInfo) match {
        case Left(err) => println(s"ERROR: initDraw ${err}"); Left(err)
        case Right(s)  => initKoMatches(compTyp) match {
          case Left(err)  => println(s"ERROR: initKoMatches ${err}"); Left(err)
          case Right(res) => setStatus(CompPhaseStatus.AUS); mTotal = matches.size; mFix = res; Right(mTotal)
        } 
      }
    }

    def drawKO_Rating(confirmedPants: ArrayBuffer[Pant], compTyp: CompTyp.Value) = {
      val pants = confirmedPants.sortBy(x => -x.rating)
      for (i <- noPlayers until size) { pants += Pant.bye()}
      ko.initDraw_Rating(pants) match {
        case Left(err) => println(s"ERROR: initDraw ${err}"); Left(err)
        case Right(s)  => initKoMatches(compTyp) match {
          case Left(err)  => println(s"ERROR: initKoMatches ${err}"); Left(err)
          case Right(res) => setStatus(CompPhaseStatus.AUS); mTotal = matches.size; mFix = res; Right(mTotal)
        } 
      }
    } 

    //val base = for (c <- baseCoPhs) yield { c.name }
    size = KoRound.getSize(noPlayers)
    ko = new KoRound(size, name, noWinSets)

    if (baseCoPhs.length == 0) {
      drawKO_Rating(pants, compTyp)
    } else {
      val baseCoPh = baseCoPhs(0)
      baseCoPh.coPhCfg match {
        case CompPhaseCfg.GRPS3  | CompPhaseCfg.GRPS34 | CompPhaseCfg.GRPS4  |
             CompPhaseCfg.GRPS45 | CompPhaseCfg.GRPS5  | CompPhaseCfg.GRPS56 | 
             CompPhaseCfg.GRPS6 => drawKO_Grp(pants, baseCoPh, compTyp)
        case _                  => Left(Error("unknownBase")) 
      }
    }
  }  


  /** drawRR  - draw round robin
    *
    * @pants pants       Array of confirmed all possible pants (candidates)
    * @param compTyp
    * @param baseCoPhs   Array of previous/base competition phase
    * @return            Number of games
    */
  def drawRR(confirmedPants: ArrayBuffer[Pant], compTyp: CompTyp.Value, baseCoPhs: ArrayBuffer[CompPhase]): Either[Error, Int] = {
    val pants = confirmedPants.sortBy(x => -x.rating)

    size = noPlayers
    groups = ArrayBuffer[Group]() 

    if (size > Tourney.MaxSizeRR) Left(Error("invalid size")) else {
      println("drawRR")
      groups = groups :+ new Group(1, size, size/2, "", noWinSets)
      val noGroups = 1
      
      for (i <- 0 until noGroups) { groups(i).pants.sortInPlaceBy(x => - x.rating) } 
      val lastPos = groups.foldLeft(1){ (pos, g) =>  g.drawPos = pos; pos + g.size }

      for (i <- 0 until pants.size) {  groups(0).addPant(pants(i), 0) }
      initGrMatches(compTyp) match {
        case Left(err)  => Left(err)
        case Right(res) => setStatus(CompPhaseStatus.AUS); mTotal = matches.size; Right(mTotal) 
      }
    }
  }  


  /** drawGR  - draw Group Round based on previous rounds
    *
    * @pants pants       Array of confirmed all possible pants (candidates)
    * @param compTyp
    * @param baseCoPhs   Array of previous/base competition phase
    * @return            Number of games
    */
  def drawGr(pants: ArrayBuffer[Pant], compTyp: CompTyp.Value, baseCoPhs: ArrayBuffer[CompPhase]): Either[Error, Int] = {
    val grpCfg: List[GroupConfig] = Group.genGrpConfig(coPhCfg, pants.size).toList

    // generate groups
    groups = ArrayBuffer[Group]() 
    for (gEntry <- grpCfg) { groups = groups :+ new Group(gEntry.id, gEntry.size, gEntry.quali, gEntry.name, noWinSets) } 
    val noGroups = groups.size

    // calculate average pant rating
    val (sum, cnt, maxRating) = pants.foldLeft((0,0,0))((a, e) => if (e.rating == 0) (a._1, a._2, a._3) else (a._1 + e.rating, a._2+1, e.rating.max(a._3) ) )
    val avgPantRating = sum/cnt


    // Step 1  - init club name occurence in pants
    // val clubOccuMap = scala.collection.mutable.Map[String, Int]().withDefaultValue(0) 
    // pants.map(pant => { if (pant.club != "") clubOccuMap(pant.club) = clubOccuMap(pant.club) + 1 } )
    // for (i <- 0 until pants.size) { pants(i).setInt("occu", clubOccuMap(pants(i).club)) }


    // Step 2 - position the best players, one in each group  (take given rating)
    // noGroups-Anzahl der Bestplazierten in pantsS1 ganze Rest in pantsS2 
    val (pantsS1, pantsS2) = pants.sortBy(_.rating).reverse.splitAt(noGroups)
    // die bestplazierten auf die Gruppen verteilen
    for (i <- 0 until pantsS1.size) {  groups(i).addPant(pantsS1(i), avgPantRating) }

    // Step 3 - position players with highest occurence and ascending rating
    //val (pantsS3, pantsS4) = pantsS2.sortBy(x => (pants.size + 1 - x.occu) * maxRating + x.effRating).splitAt(noGroups)
    //val pantsS3 = pantsS2.sortBy(x => (pants.size + 1 - x.occu) * maxRating + x.effRating)    
    val pantsS3 = pantsS2.sortBy(_.rating)

    for (i <- 0 until pantsS3.size) {
      val ratings = getMinOccBestAvg(pantsS3(i), groups, pants.size, MAX_RATING, noGroups, avgPantRating)  
      // get index of biggest element
      val bestRatingPos = ratings.zipWithIndex.maxBy(_._1)._2
      groups(bestRatingPos).addPant(pantsS3(i), avgPantRating)
    }

    // Step 3: sort group according to pant rating
    //         drawPos starts at 1
    for (i <- 0 until noGroups) { groups(i).pants.sortInPlaceBy(x => - x.rating) } 
    val lastPos = groups.foldLeft(1){ (pos, g) =>  g.drawPos = pos; pos + g.size }

    // Step 4: generate matches
    initGrMatches(compTyp) match {
      case Left(err)  => Left(err)
      case Right(res) => setStatus(CompPhaseStatus.AUS); mTotal = matches.size; Right(mTotal) 
    }
  }


  //*****************************************************************************
  // Initialize Match Routines
  //*****************************************************************************
  // initialize Group matches
  def initGrMatches(coTyp: CompTyp.Value): Either[Error, Boolean] = {
    import shared.utils.GroupPlan
    matches = ArrayBuffer[MEntry]()

    try { groups.foreach { g =>
      val gPE = GroupPlan.get(g.size)
      for (rnd <-1 to gPE.noRounds) { gPE.rounds(rnd-1).foreach { wgw =>
        matches += MEntryGr.init(coId, coTyp, coPhId, getTyp, 0, g.pants(wgw._1-1).sno, g.pants(wgw._2-1).sno, rnd, g.grId, wgw, noWinSets)
      }}  
    }} catch { case _: Throwable => println("ERROR: initGrMatches -> exception generating matches according to plan"); Left(Error("err0197.msg.initGrMatches.generating")) }

    matches = matches.sortBy(r => (r.round, r.asInstanceOf[MEntryGr].grId))
    for (i <- 0 until matches.size) { matches(i).setGameNo(i+1) } 
    genGrMatchDependencies() match {
      case Left(err)  => Left(err)
      case Right(res) => {
        for (i <- 0 until matches.size) { if (matches(i).asInstanceOf[MEntryGr].hasDepend) { matches(i).setStatus(MEntry.MS_BLOCK)} } 
        Right(res)
      }
    }
  } 

  // initialize matches for KO-System
  def initKoMatches(coTyp: CompTyp.Value): Either[Error, Int] = {
    matches = ArrayBuffer[MEntry]()
    var err      = Error.dummy
    var gameNo   = 0
    var byeCount = 0

    for (r <- ko.rnds to 0 by -1) {
      for (m <- 1 to KoRound.getMatchesPerRound(r)) {
        gameNo = gameNo + 1
        if (r == ko.rnds) {
          // first/highest round initialize with participants
          val pantNo = (m-1)*2
          val byeStatus = (SNO.isBye(ko.pants(pantNo).sno), SNO.isBye(ko.pants(pantNo+1).sno))
          val mtch = byeStatus match {
            case (false, false) => MEntryKo.init(coId, coTyp, coPhId, getTyp, ko.pants(pantNo).sno, ko.pants(pantNo+1).sno, gameNo, r, m, "","", MEntry.MS_READY, (0,0), noWinSets)
            case (false, true)  => {
              byeCount = byeCount +1
              MEntryKo.init(coId, coTyp, coPhId, getTyp, ko.pants(pantNo).sno, ko.pants(pantNo+1).sno, gameNo, r, m, "","", MEntry.MS_FIX, (noWinSets, 0), noWinSets)
            }  
            case (true, false)  => {
              byeCount = byeCount +1
              MEntryKo.init(coId, coTyp, coPhId, getTyp, ko.pants(pantNo).sno, ko.pants(pantNo+1).sno, gameNo, r, m, "","", MEntry.MS_FIX, (0, noWinSets), noWinSets)
            }
            case (true, true)   => {
              err = Error("initKoMatches_invalid_ko_match")
              MEntryKo.init(coId, coTyp, coPhId, getTyp, ko.pants(pantNo).sno, ko.pants(pantNo+1).sno, gameNo, r, m, "","", MEntry.MS_UNKN, (0,0), noWinSets)
            }  
          }
          matches += mtch
        } else {
          matches += MEntryKo.init(coId, coTyp, coPhId, getTyp, "", "", gameNo, r, m, "","", MEntry.MS_MISS, (0,0), noWinSets)
        }
      }
    }

    // propagate bye matches
    for (g <- 1 to KoRound.getMatchesPerRound(ko.rnds)) { val x = propMatch(g) }
    if (err.isDummy) Right(byeCount) else Left(err)
  }


  // Initialize matches for swiss tournament system
  //
  // Should use a minimal weight perfect matching algorithm, i.e. Edmonds algorithm
  // but don't have an approbriate one in Scala, so use 'simple' backtracking algorithm
  // should be fine for small sizes.

  def initSwMatches(coTyp: CompTyp.Value, round: Int, swOption: Int): Either[Error, Unit] = {
    import shared.utils.GroupPlan
    matches = ArrayBuffer[MEntry]()

    val g = groups(0)
    case class PInfo(points: Int, gPos: Int, played: Boolean, name: String)

    val pantMatrix = Array.fill[PInfo](size,size) (PInfo(0,0,false,""))
    //val pantMatrix = Array.fill[PInfo](4) (PInfo(0,0))

    
    // generate list of
    // (grpPosition, name, points, rating)
    val posList = (for(i<-0 until size) yield {
        (i, groups(0).pants(i).name, groups(0).points(i)._1, groups(0).pants(i).rating)
    }).sortBy(-_._4).sortBy(-_._3).to(Array)

    // debug
    println(s"Position List:")
    for(i<-0 until size) println(s"${posList(i)._2} ${i}<-${posList(i)._1}  points: ${posList(i)._3}  rating: ${posList(i)._4}") 

    println(s"Group:")
    for (i<-0 until size) {
      print(s"${g.pants(i).name} ")
      for(j<-0 until size) {
        if (j != size-1) print(s"${g.results(i)(j).sets}") else println(s"${g.results(i)(j).sets}") 
      }
    }

    // initialize pant min weight matrix
    // 
    for (i<-0 until size; j<-i until size) {
      val played = if (i==j) true else g.results(posList(i)._1)(posList(j)._1).valid     // i-j 
      pantMatrix(i)(j) = PInfo(posList(j)._3, posList(j)._1, played, posList(j)._2)
    }


    println(s"Minimum Weight Matrix:")
    for (i<-0 until size) {
      print(s"${g.pants(i).name} ")
      for(j<-0 until size) {      
        if (j<i) print("     ") else {
         val tf = if (pantMatrix(i)(j).played) "T" else "F" 
         if (j != size-1) print(s"${pantMatrix(i)(j).name}${pantMatrix(i)(j).gPos}${tf} ") 
         else             println(s"${pantMatrix(i)(j).name}${pantMatrix(i)(j).gPos}${tf} ") 
        }
      }
    }  

    // HERE DO MINIMAL WEIGHT PERFECT MATCHINT!
    // Generate input matrix for backtracking algorithm, should use Edmonds Alg
    // Sort by points and rating/ttr ... 
    // ...

    // TEST RESULT
    val mwpm = (for (i<-0 until size by 2) yield { (i,i+1) }).to(List)

    mwpm.foreach { mp =>
      matches += MEntryGr.init(coId, coTyp, coPhId, getTyp, 0, g.pants(mp._1).sno, g.pants(mp._2).sno, round, 0, mp, noWinSets)
    }
    Right({})
  }


  //*****************************************************************************
  // Basic Match Routines
  //*****************************************************************************
  def existsMatchNo(matchNo: Int): Boolean = (matchNo>0) && (matchNo <= matches.size)

  def depFinished(gameNo: Int, coPhTyp: CompPhaseTyp.Value): Boolean = {
    try {
      coPhTyp match {
        case CompPhaseTyp.GR | CompPhaseTyp.RR | CompPhaseTyp.SW  => {
          val depend = matches(gameNo-1).asInstanceOf[MEntryGr].getDepend 
          // check if any dependend match is not yet finished
          // set new status based on dependend matches are finished
          val depFinished = depend.map(g => if (existsMatchNo(g)) matches(g-1).finished else true) 
          !depFinished.contains(false)
        }
        case _     => true
      }
    } catch { case _:Throwable => println(s"ERROR depFinished gameNo:${gameNo}"); true}   
  }

  def resetResults(): Unit = {
    for (i<-0 to matches.length-1) matches(i).reset()
    getTyp match {
      case CompPhaseTyp.RR => groups(0).resetResult
      case CompPhaseTyp.SW => groups(0).resetResult
      case CompPhaseTyp.GR => for (i <- 0 to groups.size-1) groups(i).resetResult
      case CompPhaseTyp.KO => ko.resetResult()
      case _      => // do some error handling?
    }
  }


  // setModel enter result into the corresponding model 
  def setModel(m: MEntry): Unit = {  

    try {
      if (m.gameNo-1 >= 0 && m.gameNo-1 < matches.length ) {
        matches(m.gameNo-1) = m 
      } else println(s"ERROR setModel index out of range ${m.gameNo}")

      m.coPhTyp match {
        case CompPhaseTyp.GR | CompPhaseTyp.RR | CompPhaseTyp.SW => {
          val mtch = m.asInstanceOf[MEntryGr]

          if (mtch.grId > 0 & mtch.grId <= groups.length) {
            groups(mtch.grId-1).setMatch(mtch) match {
              case Left(err)  => println(s"ERROR setModel: group match: ${err.toString}" )
              case Right(res) => if (res) groups(mtch.grId-1).calc else println("ERROR setModel: set group match, invalid param")
            }
          } else {
            println("ERROR setModel set group match, invalid group id")
          }
        }  

        case CompPhaseTyp.KO => {
          ko.setMatch(m.asInstanceOf[MEntryKo]) match {
            case Left(err)  => println(s"ERROR setModel: ${err.toString}")
            case Right(res) => if (res) ko.calc else println("ERROR setModel set ko match, invalid param")
          }
        }   
  
        case _      => ()
      }
    } catch { case _: Throwable => println(s"ERROR setModel ${m.toString}")}
  }
  

  // inputMatch - set match result, info, playfield ....
  def inputMatch(gameNo: Int, sets: (Int,Int), result: String, info: String, playfield: String): Either[Error, List[Int]] = {
    try {
      val m = getMatch(gameNo)
      m.setSets(sets)
      m.setResult(result)
      m.setInfo(info)
      m.setPlayfield(playfield)
      m.setStatus(depFinished(gameNo, m.coPhTyp))
      setModel(m)
      updateStatus() 
      Right(propMatch(gameNo))
    } catch { case _:Throwable => Left(Error("err0224.coph.inputMatch.invalidGameNo", gameNo.toString))} 
  }


  // propMatch
  def propMatch(gameNo: Int): List[Int] = { 
    import scala.collection.mutable.ListBuffer
    
    try {
      val triggerList = ListBuffer[Int](gameNo)
      val m = getMatch(gameNo)

      // propagate changes to dependend matches
      // set trigger list for relevant matches
      m.coPhTyp match {

        case CompPhaseTyp.GR | CompPhaseTyp.RR | CompPhaseTyp.SW => {
          // set status for every match to be triggered
          val trigger = m.asInstanceOf[MEntryGr].getTrigger
          for (g <- trigger) { 
            setModel(getMatch(g).setStatus(depFinished(g, m.coPhTyp)))
            triggerList.append(g) 
          }
        }

        case CompPhaseTyp.KO => {
          val (gWin, pWin) = m.asInstanceOf[MEntryKo].getWinPos
          // propagate winner
          if (existsMatchNo(gWin) && m.finished) { 
            setModel(getMatch(gWin).setPant(pWin, m.getWinner).setStatus(true))
            triggerList.append(gWin)
          }  
          // propagate looser i.e. 3rd place match
          val (gLoo, pLoo) = m.asInstanceOf[MEntryKo].getLooPos
          if (existsMatchNo(gLoo) && m.finished) {
            setModel(getMatch(gLoo).setPant(pLoo, m.getLooser).setStatus(true))
            triggerList.append(gLoo)
          }
        }
      }
      updateStatus() 
      triggerList.toList
    } catch { case _: Throwable => println("ERROR propMatch exception"); List() }  
  }


  def getMatch(game: Int): MEntry = {
    getTyp match {
      case CompPhaseTyp.GR => matches(game-1).asInstanceOf[MEntryGr]
      case CompPhaseTyp.RR => matches(game-1).asInstanceOf[MEntryGr]
      case CompPhaseTyp.SW => matches(game-1).asInstanceOf[MEntryGr]
      case CompPhaseTyp.KO => matches(game-1).asInstanceOf[MEntryKo]
      case _      => matches(game-1).asInstanceOf[MEntryBase]
    }    
  }
  

  def resetMatches(): Either[Error, List[Int]] = {
    var error = Error.dummy
    val triggerList = scala.collection.mutable.ListBuffer[Int]()

    println(s"resetMatches coId: ${coId} coPhId: ${coPhId}")
    try {
      getTyp match {
        case CompPhaseTyp.KO | CompPhaseTyp.GR | CompPhaseTyp.RR | CompPhaseTyp.SW  => 
          // val mList = (for (m <- matches) yield { if (m.status == MEntry.MS_FIN && (m.round == maxRnd || m.round == (maxRnd-1))) m.gameNo else 0 }).filter(_ != 0)
          // mList.distinct.sorted foreach { g => triggerList ++= resetMatchPropagate(g) } 
          for (i<-0 to matches.length-1) 
            if (matches(i).status == MEntry.MS_FIN || matches(i).status == MEntry.MS_RUN) 
              resetMatch(matches(i).gameNo) match {
                case Left(err)  => error = err
                case Right(res) => triggerList ++= res
              }
        case _ => {}
      }
      if (error.isDummy) Right(triggerList.distinct.sorted.toList) else Left(error)
    } catch { case _:Throwable => Left(Error("err0229.svc.resetMatches.failed")) }
  }


  def resetMatch(gameNo: Int, resetPantA: Boolean=false, resetPantB: Boolean=false): Either[Error, List[Int]] = {
    import scala.collection.mutable.ListBuffer

    try {
      var error = Error.dummy
      val triggerList = ListBuffer[Int](gameNo)
      val m = getMatch(gameNo)
      m.reset(resetPantA, resetPantB)

      m.coPhTyp match {
        case CompPhaseTyp.GR | CompPhaseTyp.RR  | CompPhaseTyp.SW  => {
          setModel(m.setStatus(depFinished(gameNo, m.coPhTyp)))

          // set status for every match to be triggered
          val trigger = m.asInstanceOf[MEntryGr].getTrigger
          for (g <- trigger) { 
            setModel(getMatch(g).setStatus(depFinished(g, m.coPhTyp)))
            triggerList.append(g)
          }  
        }

        case CompPhaseTyp.KO => {
          setModel(m.setStatus(true))      
          // propagate deletion of that position
          val (gWin, pWin) = m.asInstanceOf[MEntryKo].getWinPos
          //println(s"propagate winner gameNo: ${gWin} position: ${pWin}")
          if (existsMatchNo(gWin)) resetMatch(gWin, pWin==0, pWin==1 ) match {
            case Left(err)  => error = err
            case Right(res) => triggerList ++= res
          }
          
          // propagate looser i.e. 3rd place match
          val (gLoo, pLoo) = m.asInstanceOf[MEntryKo].getLooPos
          //println(s"propagate looser gameNo: ${gLoo} position: ${pWin}")
          if (existsMatchNo(gLoo)) resetMatch(gLoo, pLoo==0, pLoo==1 ) match {
            case Left(err) => error = err
            case Right(res) => triggerList ++= res
          }
        }
      }
      updateStatus() 
      if (error.isDummy) Right(triggerList.toList) else Left(error)
    } catch { case _: Throwable => Left(Error("err0230.svc.resetMatch.game", gameNo.toString))}
  }


  // Example Match Entry for ClickTT
  // <match nr="1" 
  //   group="Gruppe A" 
  //   scheduled="" 
  //   player-a="PLAYER257" player-b="PLAYER256" 
  //   set-a-1="2"  set-a-2="11" set-a-3="4"  set-a-4="11" set-a-5="11" set-a-6="0" set-a-7="0" 
  //   set-b-1="11" set-b-2="0"  set-b-3="11" set-b-4="3"  set-b-5="3"  set-b-6="0" set-b-7="0" 
  //   sets-a="3" matches-a="1" games-a="39" 
  //   sets-b="2" matches-b="0" games-b="28" />

  def getMatchesXML(sno2Id: Map[String,String]): String = {
    val result = new StringBuilder("")
    matches.foreach { m => if (m.countable) {
      val playerA = sno2Id.getOrElse(m.stNoA,"")
      val playerB = sno2Id.getOrElse(m.stNoB,"")
      
      if (playerA != "" && playerB != "") {
        val group = name + " " + getMatchName(m)
        val setsAB = m.sets
        val setA=Array(0,0,0,0,0,0,0)
        val setB=Array(0,0,0,0,0,0,0)
        for ((ballsAB, index) <- m.getBalls.zip(Stream from 0)) {
          setA(index) = ballsAB._1
          setB(index) = ballsAB._2
        }
        val matchesAB = if (setsAB._1 > setsAB._2) (1,0) else if (setsAB._2 > setsAB._1) (0,1) else (0,0)
        val xmlString = s"""<match group="${group}" scheduled="" player-a="${playerA}" player-b="${playerB}"
                            | set-a-1="${setA(0)}"  set-a-2="${setA(1)}" set-a-3="${setA(2)}"  set-a-4="${setA(3)}" set-a-5="${setA(4)}" set-a-6="${setA(5)}" set-a-7="${setA(6)}"
                            | set-b-1="${setB(0)}"  set-b-2="${setB(1)}" set-b-3="${setB(2)}"  set-b-4="${setB(3)}" set-b-5="${setB(4)}" set-b-6="${setB(5)}" set-b-7="${setB(6)}"
                            | sets-a="${setsAB._1}" matches-a="${matchesAB._1}" games-a="${setA.sum}"
                            | sets-b="${setsAB._2}" matches-b="${matchesAB._2}" games-b="${setB.sum}" />""".stripMargin('|')
        result.append(xmlString)
      }  
    }}
    result.toString()
  }

  def getMatchName(m: MEntry): String = {
    m.coPhTyp match {
      case CompPhaseTyp.GR | CompPhaseTyp.RR => groups(m.asInstanceOf[MEntryGr].grId).name
      case CompPhaseTyp.SW => "MatchName"
      case CompPhaseTyp.KO => m.asInstanceOf[MEntryKo].round match {
        case 7 => "Round of 128"
        case 6 => "Round of 64"
        case 5 => "Round of 32"
        case 4 => "Round of 16"
        case 3 => "Quarter-final"
        case 2 => "Semi-final"
        case 1 => "Final"
        case 0 => "Place 3"
        case _ => ""
      }
      case _ => ""
    }  
  }  

  // calculate players position within competition phase
  def calcModel(grId: Int = -1): Boolean = {
    getTyp match {
      case CompPhaseTyp.GR | CompPhaseTyp.RR | CompPhaseTyp.SW => grId match {
         case -1 =>  for (g <- 0 to groups.length-1) { groups(g).calc }; true 
         case g if (g > 0 && g <= groups.length) =>  groups(grId-1).calc; true
         case _ => false
      }
        // if (-1 == grId) { for (g <- 0 to groups.length-1) { groups(g).calc }; true } 
        // else { if (grId > 0 & grId <= groups.length) { groups(grId-1).calc; true } else { false } }  
      case CompPhaseTyp.KO => ko.calc; true
      case _      => false
    }
  }

  // target function for descrete optimization
  def getMinOccBestAvg(pant: Pant, grps: ArrayBuffer[Group], pantSize: Int, maxRating: Int, maxGrpSize: Int, pantAvgRating: Int): ArrayBuffer[Long] = {
    val result = ArrayBuffer.fill[Long](grps.size)(0)

    for (i <- 0 until grps.size) {
      result(i) = {
        if (grps(i).fillCnt == grps(i).size) {
          0L 
        } else {
          // calculate improvement of average rating 
          val curDiffRating = if (grps(i).avgRating==0) 0 else (pantAvgRating - grps(i).avgRating).abs 
          val newDiffRating = (pantAvgRating - ((grps(i).avgRating * grps(i).fillCnt + pant.getEffRating(pantAvgRating)) / (grps(i).fillCnt+1))).abs
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

  // genGrMatchDependencies - generate depend and trigger values
  def genGrMatchDependencies(): Either[Error, Boolean] = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashSet
    import shared.model.Competition._
    try {
      // init map with default value
      val depMap = Map[Long, ListBuffer[Int]]()
      groups.foreach { g => g.pants.foreach { p => if (SNO.valid(p.sno)) { depMap(SNO.plId(p.sno)) = ListBuffer() }}}
      
      // setup list player -> game numbers
      for (m <- matches) {
        m.coTyp match {
          case CompTyp.SINGLE => {
            if (SNO.valid(m.stNoA)) { (depMap(SNO.plId(m.stNoA))) += m.gameNo }
            if (SNO.valid(m.stNoB)) { (depMap(SNO.plId(m.stNoB))) += m.gameNo }
          }
          case CompTyp.DOUBLE => {
            val idAs = getMDLongArr(m.stNoA)
            val idBs = getMDLongArr(m.stNoB)
            if (idAs.size>=1 && SNO.valid(idAs(0))) { (depMap(idAs(0))) += m.gameNo }
            if (idAs.size>=2 && SNO.valid(idAs(1))) { (depMap(idAs(1))) += m.gameNo }                   
            if (idBs.size>=1 && SNO.valid(idBs(0))) { (depMap(idBs(0))) += m.gameNo }
            if (idBs.size>=2 && SNO.valid(idBs(1))) { (depMap(idBs(1))) += m.gameNo }    
          }
        }
      }         

      // calculate depend, split depend on gameNo
      for (m <- matches) {
        val plA:(ListBuffer[Int], ListBuffer[Int])  = if (SNO.valid(m.stNoA)) {
          depMap(SNO.plId(m.stNoA)).partition(_ <= m.gameNo)
        } else { (ListBuffer(), ListBuffer()) }
        val plB:(ListBuffer[Int], ListBuffer[Int])  = if (SNO.valid(m.stNoB)) {
          depMap(SNO.plId(m.stNoB)).partition(_ <= m.gameNo)
        } else { (ListBuffer(), ListBuffer()) }
        val depend = scala.collection.mutable.HashSet[Int]()
        val trigger = scala.collection.mutable.HashSet[Int]()
      
        if (plA._2.size > 0) trigger += (plA._2).sorted.head
        if (plB._2.size > 0) trigger += (plB._2).sorted.head
        if (plA._1.size > 1) depend += plA._1.sorted.reverse(1)
        if (plB._1.size > 1) depend += plB._1.sorted.reverse(1)
        matches(m.gameNo-1).asInstanceOf[MEntryGr].depend  = depend.mkString("·")
        matches(m.gameNo-1).asInstanceOf[MEntryGr].trigger = trigger.mkString("·")          
      }  
    } catch { case _: Throwable => println("genGrMatchDependencies", s"${this}"); Left(Error("??? genGrMatchDependencies")) }
    Right(true)
  }


  //*****************************************************************************
  // General Routines
  //*****************************************************************************

  def encode(version: Int=0) = write[CompPhaseTx](toTx())

  def toTx(): CompPhaseTx = {
    CompPhaseTx(name, coId, coPhId, coPhCfg.id, status.id, demo, size, noPlayers, noWinSets, baseCoPhId, quali.id, candInfo, candidates, matches.map(x=>x.toTx), groups.map(g=>g.toTx), ko.toTx) 
  }
    

  def getMaxRnds(): Int = {
    getTyp match {
      case CompPhaseTyp.RR => if (groups(0).size % 2 == 0) groups(0).size - 1 else groups(0).size
      case CompPhaseTyp.SW => (groups(0).size - 1)/2
      case CompPhaseTyp.GR => if (groups(0).size % 2 == 0) groups(0).size - 1 else groups(0).size
      case CompPhaseTyp.KO => { 
        size match {
          case 2                          =>   1
          case x if (3  <= x && x <= 4)   =>   2
          case x if (5  <= x && x <= 8)   =>   3
          case x if (9  <= x && x <= 16)  =>   4
          case x if (17 <= x && x <= 32)  =>   5
          case x if (33 <= x && x <= 64)  =>   6
          case x if (65 <= x && x <= 128) =>   7
          case _                          =>  -1
        }
      }
      case _      => 0
    }  
  }

 def getTyp: CompPhaseTyp.Value = {
    coPhCfg match {
      case CompPhaseCfg.CFG      => CompPhaseTyp.UNKN 
      case CompPhaseCfg.VRGR     => CompPhaseTyp.GR
      case CompPhaseCfg.ZRGR     => CompPhaseTyp.GR
      case CompPhaseCfg.ERGR     => CompPhaseTyp.GR
      case CompPhaseCfg.TRGR     => CompPhaseTyp.GR
      case CompPhaseCfg.VRKO     => CompPhaseTyp.KO
      case CompPhaseCfg.ERKO     => CompPhaseTyp.KO
      case CompPhaseCfg.TRKO     => CompPhaseTyp.KO
      case CompPhaseCfg.UNKN     => CompPhaseTyp.UNKN
      case CompPhaseCfg.GR3to9   => CompPhaseTyp.GR
      case CompPhaseCfg.KO       => CompPhaseTyp.KO
      case CompPhaseCfg.RR       => CompPhaseTyp.RR
      case CompPhaseCfg.GRPS3    => CompPhaseTyp.GR
      case CompPhaseCfg.GRPS34   => CompPhaseTyp.GR
      case CompPhaseCfg.GRPS4    => CompPhaseTyp.GR
      case CompPhaseCfg.GRPS45   => CompPhaseTyp.GR
      case CompPhaseCfg.GRPS5    => CompPhaseTyp.GR
      case CompPhaseCfg.GRPS56   => CompPhaseTyp.GR
      case CompPhaseCfg.GRPS6    => CompPhaseTyp.GR
      case CompPhaseCfg.SW       => CompPhaseTyp.SW
      case _                     => CompPhaseTyp.UNKN
    }    
  }


  // getDescription of competition phase
  def getDescription(getMsg: (String, Seq[String])=>String) = CompPhase.getDescription(coPhCfg, noPlayers, getMsg)

  // print readable competition phase - for debug purposes
  override def toString() = {

    def matches2Str() = {
      val str = new StringBuilder("-- MATCHES\n")
      for { m <- matches }  yield { 
        str ++= m.toString + "\n"
      } 
      str.toString
    }

    def candidates2Str() = {
      val str = new StringBuilder("-- CANDIDATES\n")
      for { c <- candidates }  yield { 
        str ++= s"${c._1} ${c._2}\n"
      } 
      str.toString
    }

    val str = new StringBuilder(s"${name} coId:${coId} coPhId: ${coPhId} coPhCfg: ${coPhCfg} typ: ${getTyp}\n")
    str.append(s"status: ${status} demo: ${demo} size: ${size}  noPlayers: ${noPlayers} noWinSets: ${noWinSets} baseCoPhId: ${baseCoPhId}\n") 
    str.append(s"${candidates2Str()}\n")
    str.append(s"${matches2Str()}\n") 

    getTyp match {
      case CompPhaseTyp.RR => str.append(groups(0).toString)
      case CompPhaseTyp.SW => str.append(groups(0).toString)
      case CompPhaseTyp.GR => for (i <- 0 to groups.length-1) str.append(groups(i).toString)
      case CompPhaseTyp.KO => str.append(s"${ko.toString}")
      case _      => str.append(s"UNKNOWN COPH: ${coPhCfg}")
    }
    str.toString
  }  

}


//*****************************************************************************
// Companion Object
//*****************************************************************************
object CompPhase {

  implicit val rw: RW[CompPhase] = upickle.default.readwriter[String].bimap[CompPhase](
    x   => write[CompPhaseTx](x.toTx()),   //s"""{ "name", "Hugo" } """,  
    str => fromTx(read[CompPhaseTx](str)) 
  )

  def dummy: CompPhase = CompPhase("", 0L, 0, CompPhaseCfg.CFG, CompPhaseStatus.CFG, false, 0, 0, 0)
    
  def get(coId: Long, coPhId: Int, coPhCfg: CompPhaseCfg.Value, name: String, noWinSets: Int, noPlayers: Int=0): CompPhase = {  

    val coph = CompPhase(name, coId, coPhId, coPhCfg, CompPhaseStatus.CFG, true, 0, 0, noWinSets)
    val coSize   = coph.getTyp match {
      case CompPhaseTyp.GR => coph.size = noPlayers
      case CompPhaseTyp.RR => coph.size = noPlayers
      case CompPhaseTyp.KO => coph.size = KoRound.getSize(noPlayers)
      case CompPhaseTyp.SW => coph.size = noPlayers + noPlayers%2
      case _               => coph.size =0
    }
    coph
  }

  def decode(coPhStr: String): Either[Error, CompPhase] = 
    try Right(fromTx(read[CompPhaseTx](coPhStr)))
    catch { case _: Throwable => Left(Error("err0177.decode.CompPhase", coPhStr.take(20))) }

  def fromTx(tx: CompPhaseTx): CompPhase = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashSet
    import shared.model.Competition._

    def getPant(value: String): Pant = CSVConverter[Pant].from(value).toEither match {
      case Left(err)  => println(s"ERROR getPant -> CSV decoding ${value}"); Pant("", "", "", 0, "", (0,0))
      case Right(res) => res
    }  

    var error = Error.dummy
    try {
      val cop = new CompPhase(tx.name, tx.coId, tx.coPhId, CompPhaseCfg(tx.coPhCfg), CompPhaseStatus(tx.status), tx.demo, tx.size, tx.noPlayers, tx.noWinSets, tx.baseCoPh, QualifyTyp(tx.quali) ) 
      
      cop.candInfo   = tx.candInfo
      cop.candidates = tx.candidates
      cop.matches    = tx.matches.map(x=>x.decode)
      cop.mTotal     = cop.matches.size

      val (fin,fix) = cop.matches.foldLeft((0,0))( (x,m) => {
        val fin = if (m.finished) 1 else 0 
        val fix = if (m.status == MEntry.MS_FIX) 1 else 0 
        (x._1 + fin, x._2 + fix)
      })
      cop.mFinished = fin
      cop.mFix      = fix

      cop.getTyp match {
        case CompPhaseTyp.GR | CompPhaseTyp.RR | CompPhaseTyp.SW  => {
          // setup group with draw position information
          val lastPos = tx.groups.foldLeft(1){ (pos, g) =>
            //cop.groups = cop.groups :+ Group.fromTx(g, pos)
            Group.fromTx(g, pos) match {
              case Left(err)  => error = err
              case Right(grp) => cop.groups = cop.groups :+ grp 
            }
            pos + g.size 
          }

          val mSize = cop.matches.size
          // hook for calculation trigger and depend, if not present
          if (mSize > 0 && !cop.matches(mSize-1).asInstanceOf[MEntryGr].hasDepend) cop.genGrMatchDependencies()
        }  
        case CompPhaseTyp.KO  => KoRound.fromTx(tx.ko) match {
          case Left(err)      => error = err
          case Right(koRound) => cop.ko = koRound
        }
        case _       => {}
      }
      cop
    } catch { case _: Throwable => 
      println(s"ERROR thrown from CompPhase.fromTx ${error.toString}")
      CompPhase("", 0L, 0, CompPhaseCfg.CFG, CompPhaseStatus.CFG, false, 0, 0, 0)
    } 
  }

  def fromTx1(tx: CompPhaseTx1): CompPhase = {
    import scala.collection.mutable.ListBuffer
    import scala.collection.mutable.HashSet
    import shared.model.Competition._

    var error = Error.dummy
    try {
      val cop = new CompPhase(tx.name, tx.coId, tx.coPhId, CompPhaseCfg(tx.coPhCfg), CompPhaseStatus(tx.status), tx.demo, tx.size, tx.noPlayers, tx.noWinSets, tx.baseCoPh, QualifyTyp(tx.quali) ) 
      
      cop.candInfo   = tx.candInfo

      // map PantV1 to Pant 
      cop.candidates = tx.candidates.map { x => (x._1.toPant(x._2), x._3)  }
      cop.matches    = tx.matches.map(x=>x.decode)
      cop.mTotal     = cop.matches.size

      val (fin,fix) = cop.matches.foldLeft((0,0))( (x,m) => {
        val fin = if (m.finished) 1 else 0 
        val fix = if (m.status == MEntry.MS_FIX) 1 else 0 
        (x._1 + fin, x._2 + fix)
      })
      cop.mFinished = fin
      cop.mFix      = fix

      cop.getTyp match {
        case CompPhaseTyp.GR | CompPhaseTyp.RR | CompPhaseTyp.SW => {
          // setup group with draw position information
          val lastPos = tx.groups.foldLeft(1){ (pos, g) =>
            //cop.groups = cop.groups :+ Group.fromTx(g, pos)
            Group.fromTx1(g, pos) match {
              case Left(err)  => error = err
              case Right(grp) => cop.groups = cop.groups :+ grp 
            }
            pos + g.size 
          }

          val mSize = cop.matches.size
          // hook for calculation trigger and depend, if not present
          if (mSize > 0 && !cop.matches(mSize-1).asInstanceOf[MEntryGr].hasDepend) cop.genGrMatchDependencies()
        }  
        case CompPhaseTyp.KO  => KoRound.fromTx1(tx.ko) match {
          case Left(err)      => error = err
          case Right(koRound) => cop.ko = koRound
        }
        case _       => {}
      }
      cop
    } catch { case _: Throwable => 
      println(s"ERROR: thrown from CompPhase.fromTx ${error.toString}")
      CompPhase("", 0L, 0, CompPhaseCfg.CFG, CompPhaseStatus.CFG, false, 0, 0, 0)
    } 
  }



  def getDescription(coPhCfg: CompPhaseCfg.Value, noPlayers: Int, getMsg: (String, Seq[String])=>String): String = {
    import shared.model.Group
    import shared.model.KoRound

    def getMsgNumber(value: Int) = if (value > 0 && value < 10) getMsg(s"number.${value}",Seq()) else value.toString

    coPhCfg match {
      case CompPhaseCfg.GRPS3  => { val size1 = noPlayers / 3; getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1))) }   
      case CompPhaseCfg.GRPS34 => { val (size1, size2) = Group.genGrpSplit(noPlayers, 3); getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1), getMsgNumber(size2))) }
      case CompPhaseCfg.GRPS4  => { val size1 = noPlayers / 4; getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1))) } 
      case CompPhaseCfg.GRPS45 => { val (size1, size2) = Group.genGrpSplit(noPlayers, 4); getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1), getMsgNumber(size2))) } 
      case CompPhaseCfg.GRPS5  => { val size1 = noPlayers / 5; getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1))) }
      case CompPhaseCfg.GRPS56 => { val (size1, size2) = Group.genGrpSplit(noPlayers, 5); getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1), getMsgNumber(size2))) }   
      case CompPhaseCfg.GRPS6  => { val size1 = noPlayers / 6; getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1))) }
      case CompPhaseCfg.KO     => { getMsg(coPhCfg.infoCode, Seq(KoRound.getSize(noPlayers).toString)) }
      case CompPhaseCfg.SW     => { val size1 = noPlayers+(noPlayers%2); getMsg(coPhCfg.infoCode, Seq(getMsgNumber(size1))) }
      case CompPhaseCfg.RR    => { getMsg(coPhCfg.infoCode, Seq(noPlayers.toString)) }
      case _                   => { getMsg(coPhCfg.infoCode, Seq()) }
    }
  }
}



//
// Transfer representation of a competition phase
//
case class CompPhaseTx(
  val name:       String, 
  val coId:       Long, 
  val coPhId:     Int,
  val coPhCfg:    Int,
  val status:     Int,
  var demo:       Boolean, 
  var size:       Int, 
  val noPlayers:  Int,
  val noWinSets:  Int,
  val baseCoPh:   Option[Int],
  val quali:      Int,
  val candInfo:   String, 
  val candidates: ArrayBuffer[(Pant,Boolean)], 
  val matches:    ArrayBuffer[MEntryTx], 
  val groups:     ArrayBuffer[GroupTx],
  val ko:         KoRoundTx
)

object CompPhaseTx {
  implicit def rw: RW[CompPhaseTx] = macroRW 
}


case class CompPhaseTx1(
  val name:       String, 
  val coId:       Long, 
  val coPhId:     Int,
  val coPhCfg:    Int,
  val status:     Int,
  var demo:       Boolean, 
  var size:       Int, 
  val noPlayers:  Int,
  val noWinSets:  Int,
  val baseCoPh:   Option[Int],
  val quali:      Int,
  val candInfo:   String, 
  val candidates: ArrayBuffer[(Pant1,String,Boolean)], 
  val matches:    ArrayBuffer[MEntryTx], 
  val groups:     ArrayBuffer[GroupTx1],
  val ko:         KoRoundTx1
)

object CompPhaseTx1 {
  implicit def rw: RW[CompPhaseTx1] = macroRW 
}



object CompPhaseCfg extends Enumeration {
  val UNKN   = Value(-1, "UNKN")        // UNBEKANNT
  val CFG    = Value(0,  "CFG")         // Auswahl des Spielsystems
  val VRGR   = Value(1,  "VRGR")        // Gruppen Vorrunde
  val ZRGR   = Value(2,  "ZRGR")        // Gruppen Zwischenrunde
  val ERGR   = Value(3,  "ERGR")        // Gruppen Endrunde
  val TRGR   = Value(4,  "TRGR")        // Trost-Gruppen Runde
  val VRKO   = Value(6,  "VRKO")        // KO Vorrunde
  val ERKO   = Value(8,  "ERKO")        // KO Endrunde
  val TRKO   = Value(9,  "TRKO")        // KO Trostrunde
  val GR3to9 = Value(100,"GR3to9")      // Spielsystem mit 3er-9er Gruppen
  val GRPS3  = Value(101,"GRPS3")       // Spielsystem mit 3er-Gruppen
  val GRPS34 = Value(102,"GRPS34")      // Spielsystem mit 3er- und 4er-Gruppen
  val GRPS4  = Value(103,"GRPS4")       // Spielsystem mit 4er-Gruppen
  val GRPS45 = Value(104,"GRPS45")      // Spielsystem mit 4er- und 5er-Gruppen
  val GRPS5  = Value(105,"GRPS5")       // Spielsystem mit 5er-Gruppen
  val GRPS56 = Value(106,"GRPS56")      // Spielsystem mit 5er- und 6er-Gruppen
  val GRPS6  = Value(107,"GRPS6")       // Spielsystem mit 6er-Gruppen
  val RR     = Value(108,"RR")          // Spielsystem jeder-gegen-jeden
  val KO     = Value(109,"KO")          // KO-System
  val SW     = Value(110,"SW")          // Schweizer System

  implicit class CompPhaseCfg(cs: Value) {
    def msgCode  = s"CompPhaseCfg.${cs.toString}"
    def infoCode = s"CompPhaseCfgInfo.${cs.toString}"
  }  
} 

object CompPhaseTyp extends Enumeration {
  val UNKN = Value(-1, "UNKN")
  val GR   = Value(1,  "GR")  // group system
  val KO   = Value(2,  "KO")  // ko system
  val SW   = Value(3,  "SW")  // switz system 
  val RR   = Value(4,  "RR")  // round robin 

  implicit class CompPhaseTyp(cs: Value) {
    def msgCode = s"CompPhaseTyp.${cs.toString}"
  }  
}

object CompPhaseStatus extends Enumeration { 
  val CFG  = Value(0, "CFG")  // Configuration Status
  val AUS  = Value(1, "AUS")  // Auslosung der Vorrunde, Zwischenrunde, Endrunde, Trostrunde
  val EIN  = Value(2, "EIN")  // Auslosung erfolgt, Eingabe der Ergebnisse
  val FIN  = Value(3, "FIN")  // Runde/Phase beendet, Auslosung ZR oder ER kann erfolgen
  val UNKN = Value(-1, "UNKN") 

  implicit class CompPhaseValue(cs: Value) {
    def msgCode = s"CompPhaseStatus.${cs.toString}"
  }

}

object QualifyTyp extends Enumeration { 
  val ALL  = Value(0, "All")  // pre selection all
  val WIN  = Value(1, "WIN")  // pre selection winner of previous round
  val LOO  = Value(2, "LOO")  // pre select looser of previous round

  implicit class QualifyTyp(cs: Value) {
    def msgCode = s"QualifyTyp.${cs.toString}"
  }

}
