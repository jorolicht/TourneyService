package scalajs.service

import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }
import scala.scalajs._

import shared.model._
import shared.utils._
import shared.utils.Routines._
import shared.utils.Constants._ 

import scalajs._


trait MatchSvc 
{
  def reverseList[A](list : List[A], result: List[A] = Nil) : List[A] = list match {
    case Nil => result
    case (x :: xs) => reverseList(xs, x :: result)
  }

  def getBallsFromShort(b: String):(Int,Int) = {
    if (b == "")                      { (-1,-1)
    } else if (b == "+0" | b == "0")  { (11,0)
    } else if (b == "-0")             { (0,11)
    } else { b.toIntOption.getOrElse(0) match {  
      case a if   10 to 500 contains a => (a+2, a)
      case b if    1 to   9 contains b => (11, b)
      case c if   -9 to  -1 contains c => (-c, 11)
      case d if -500 to -10 contains d => (-d, 2 - d) 
      case _                           => (-1,-1)
    }}
  }

  def getSetFromShort(b: String):(Int,Int) = {
    if (b == "")                          { (0,0)
    } else if (b == "+0" | b == "0")      { (1,0)
    } else if (b == "-0")                 { (0,1)
    } else { b.toIntOption.getOrElse(0) match {  
      case a if     1 to 1000 contains a => (1,0)
      case b if -1000 to   -1 contains b => (0,1) 
      case _                             => (0,0)
    }}
  }

  def validSets(sets: (Int,Int), noSets: Int): Boolean =  {
    (sets._1 == noSets & sets._2 < noSets) | (sets._1 < noSets & sets._2 == noSets)
  } 

  def getSetsFromShortArr(balls: Array[String], noSets: Int): Either[Error,(Int,Int)] = {
   import cats.implicits._ 
   var res = (0,0)
   for (i<-0 to balls.length-1) { res = res |+| getSetFromShort(balls(i)) }
   if (validSets(res, noSets)) { Right(res) } else { Left(Error(""))}
  }

  def validBalls(balls: Array[String], noSets: Int): Boolean = {
    import cats.implicits._
    var sets = (0,0)
    for (i <- 0 to balls.length-1 ) { sets = sets |+| getSetFromShort(balls(i)) }    
    validSets(sets, noSets)
  } 

}  