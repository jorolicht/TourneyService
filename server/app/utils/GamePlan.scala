package tourn.utils

import play.api.{ Logging }

case class GameEntry  (
  gano: Int, player1: String, player2: String, 
  ngw: Int, ngl: Int, round: Int, 
  depend: List[Int],
  desc: Map[String, String]
)
  
case class GamePlanEntry (
  name : String,
  plan : List[GameEntry]
)

object GamePlan extends Logging {
  var group03: List[GameEntry] = null
  var group04: List[GameEntry] = null

  def init(gplans: List[GamePlanEntry]) = {
    for (gp <- gplans) { gp.name match {
      case "group03"  => group03 = gp.plan
      case "group04"  => group04 = gp.plan
      case _          => logger.info(s"init: plan ${gp.name} not inizialised")
    }}
  }

}