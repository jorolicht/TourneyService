package shared.utils
  
import scala.collection.mutable.{ ArrayBuffer }

case class GamePlanEntry (
  val nameCode : String,
  val size:      Int,  
  val noRounds:  Int,
  val noMatches: Int,
  val rounds:    ArrayBuffer[Array[(Int,Int)]]
)


object GamePlan {
  val Group = ArrayBuffer(
    GamePlanEntry("gameplan.group.0", 0, 0, 0,  ArrayBuffer()),
    GamePlanEntry("gameplan.group.1", 1, 0, 0, ArrayBuffer()),
    GamePlanEntry("gameplan.group.2", 2, 0, 0, ArrayBuffer()),
    GamePlanEntry("gameplan.group.3", 3, 3, 3, ArrayBuffer(
      Array((1,3)),
      Array((2,3)),
      Array((1,2))
    )),
    GamePlanEntry("gameplan.group.4", 4, 3, 6, ArrayBuffer(
      Array((1,4), (2,3)),
      Array((1,3), (2,4)),
      Array((1,2), (3,4))
    )),
    GamePlanEntry("gameplan.group.5", 5, 5, 10, ArrayBuffer(
      Array((1,5), (2,3)),
      Array((4,5), (1,3)),
      Array((2,4), (3,5)),
      Array((1,4), (2,5)),
      Array((3,4), (1,2))
    )),
    GamePlanEntry("gameplan.group.6", 6, 5, 15, ArrayBuffer(
      Array((1,6), (2,3), (4,5)),
      Array((1,3), (4,6), (2,5)),
      Array((1,4), (3,5), (2,6)),
      Array((1,5), (2,4), (3,6)),
      Array((1,2), (3,4), (5,6))
    )),
    GamePlanEntry("gameplan.group.7", 7, 7, 21, ArrayBuffer(
      Array((1,7), (2,3), (4,5)),
      Array((1,6), (3,7), (2,4)),
      Array((1,5), (3,6), (2,7)),
      Array((1,4), (2,5), (6,7)),
      Array((1,3), (4,6), (5,7)),
      Array((2,6), (3,5), (4,7)),
      Array((1,2), (3,4), (5,6))
    )),
    GamePlanEntry("gameplan.group.8", 8, 7, 28, ArrayBuffer(
      Array((1,8), (2,3),	(4,5), (6,7)),
      Array((1,4), (5,8),	(3,6), (2,7)),
      Array((1,3), (4,6),	(2,8), (5,7)),
      Array((1,6), (3,8),	(4,7), (2,5)),
      Array((6,8), (1,7),	(3,5), (2,4)),
      Array((1,5), (3,7),	(2,6), (4,8)),
      Array((1,2), (3,4),	(5,6), (7,8))
    )), 
    GamePlanEntry("gameplan.group.9", 9, 9, 36, ArrayBuffer(
      Array((1,9), (2,3), (4,5), (6,7)),
      Array((1,8), (2,4), (6,9), (3,5)),
      Array((1,7), (4,8), (3,9), (2,5)),
      Array((1,6), (3,7), (2,8), (4,9)),
      Array((1,5), (2,6), (3,8), (7,9)),
      Array((4,6), (1,3), (5,7), (8,9)),
      Array((2,7), (6,8), (5,9), (1,4)),
      Array((3,6), (2,9), (4,7), (5,8)),
      Array((1,2), (3,4), (5,6), (7,8))
    )), 
    GamePlanEntry("gameplan.group.10", 10, 9, 45, ArrayBuffer(
      Array((1,10), (2,3)  ,(4,5),  (6,7), (8,9)),
      Array((1,5),  (3,6)  ,(8,10), (4,9), (2,7)),
      Array((1,8),  (5,9)  ,(4,6),  (3,7), (2,10)),
      Array((1,4),  (5,7)  ,(3,8),  (2,9), (6,10)),
      Array((1,9),  (2,5)  ,(4,7),  (6,8), (3,10)),
      Array((1,7),  (2,6)  ,(5,10), (3,9), (4,8)),
      Array((1,6),  (7,9)  ,(4,10), (3,5), (2,8)),
      Array((6,9),  (7,10) ,(1,3),  (2,4), (5,8)),
      Array((1,2),  (3,4)  ,(5,6),  (7,8), (9,1))
    ))
  )



} 






