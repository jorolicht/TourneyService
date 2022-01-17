package shared.model.tabletennis

import shared.utils.Routines._

package object utility {

  val abc = 'A' to 'Z'

  implicit class TupleSwap[A: Numeric, B: Numeric](t: (A,B)) {
    import Numeric.Implicits._
    def swap (p: (A,B)) = (p._2, p._1)
  }

  implicit class TupleAdd[A: Numeric, B: Numeric](t: (A,B)) {
    import Numeric.Implicits._
    def + (p: (A,B)) = (p._1 + t._1, p._2 + t._2)
  }

  class OrderTupleBySecond[X,Y <% Comparable[Y]] extends Ordering[(X,Y)] {
    def compare(x: (X,Y), y: (X,Y)) = {
      x._2 compareTo y._2
    }
  } 

  def invBall(x: String) = { x(0) match { case '-' => x.substring(1); case '+' => "-" + x.substring(1); case _ => "-" + x } } 

  def getBallFromStr(b: String):(Int,Int) = {
    if (b == "")                      { (-1,-1)
    } else if (b == "+0" | b == "0") { (11,0)
    } else if (b == "-0")             { (0,11)
    } else { b.toIntOption.getOrElse(0) match {  
      case a if   10 to 500 contains a => (a+2, a)
      case b if    1 to   9 contains b => (11, b)
      case c if   -9 to  -1 contains c => (-c, 11)
      case d if -500 to -10 contains d => (-d, 2 - d) 
      case _                           => (-1,-1)
    }}
  }

  def getBalls(balls: Array[String], noSets: Int): (Int, Int) = {
    var res    = (0,0)
    var nASets = 0
    var nBSets = 0
    for (i <- 0 to balls.length-1) {
      val bs = getBallFromStr(balls(i))
      if (bs != (-1,-1) & nASets < noSets & nBSets < noSets) {
        res = res + bs
        if (bs._1 > bs._2) nASets += 1
        if (bs._2 > bs._1) nBSets += 1
      }  
    }
    (res)
  } 

  def validSets(sets: (Int,Int), noSets: Int): Boolean =  {
    (sets._1 == noSets & sets._2 < noSets) | (sets._1 < noSets & sets._2 == noSets)
  } 

  def getSets(balls: Array[String], noSets: Int): (Int, Int) = {
    var nASets = 0
    var nBSets = 0
    for (i <- 0 to balls.length-1) {
      val bs = getBallFromStr(balls(i))
      if (bs != (-1,-1) & nASets < noSets & nBSets < noSets) {
        if (bs._1 > bs._2) nASets += 1
        if (bs._2 > bs._1) nBSets += 1
      }  
    }
    (nASets, nBSets)
  }

  def getPoints(balls: Array[String], noSets: Int): (Int, Int) =  getPoints(getSets(balls, noSets),noSets)
  def getPoints(sets: (Int,Int), noSets: Int): (Int, Int) = {
    if      (sets._1 == noSets & sets._2 < noSets) { (1,0) }
    else if (sets._2 == noSets & sets._1 < noSets) { (0,1) } 
    else                                           { (0,0) }
  } 

}