package scalajs.service

import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }
import scala.scalajs._

import shared.model._
import shared.utils._
import shared.utils.Routines._
import shared.utils.Constants._ 

import scalajs._


trait DrawSvc 
{
  
  /** genGrpSplit - check whether the numbers of players can be
   *  configurated with groups of size and size+1
   */ 
  def genGrpSplit(noPlayers: Int, size: Int): (Int, Int) = {
    if (noPlayers < 2*size) (0,0) else {
      if (noPlayers % (size+1) == 0) {
        (0, noPlayers/(size+1))
      } else {
        if (noPlayers % size == 0) {
          (noPlayers/size, 0)
        } else {
          val x = noPlayers % size
          (noPlayers/size - x, x)
        }
      }
    }
  }


  /** genGrpConfig - generate configuration array for groups
   *  (grName: String, grId: Int, grSize: Int, pos: Int)
   */   
  def genGrpConfig(secTyp: Int, size: Int): Array[(String, Int, Int, Int)] = {
    var gList:  ArrayBuffer[(Int,Int)] = new ArrayBuffer()
    var result: ArrayBuffer[(String, Int, Int, Int)] = new ArrayBuffer()

    secTyp match {
      case CST_GRPS3  => { val g3 = size / 3; gList += ((3, g3)) } 
      case CST_GRPS34 => { val (g3, g4) = genGrpSplit(size, 3); gList += ((4, g4)); gList += ((3, g3)) } 
      case CST_GRPS4  => { val g4 = size / 4; gList += ((4, g4)) }       
      case CST_GRPS45 => { val (g4, g5) = genGrpSplit(size, 4); gList += ((5, g5)); gList += ((4, g4)) }
      case CST_GRPS5  => { val g5 = size / 5; gList += ((5, g5)) }  
      case CST_GRPS56 => { val (g5, g6) = genGrpSplit(size, 5); gList += ((6, g6)); gList += ((5, g5)) }
      case CST_GRPS6  => { val g6 = size / 6; gList += ((6, g6)) }
    }

    var cnt = 1
    var pos = 1
    for (gListEntry <- gList) {
      for (entry <- 1 to gListEntry._2) {
        result += ((getGroupName(cnt), cnt, gListEntry._1, pos))
        cnt = cnt + 1
        pos = pos + gListEntry._1
      }
    }
    result.to(Array)
  }

  // getGroupName - generates a name for a group (like excel column name)
  def getGroupName(grpId: Int): String = {
    var num    = grpId
    var name   = ""
    var modulo = 0

    if (grpId < 1) {
      "?"
    } else {
      var num    = grpId
      var name   = ""
      var modulo = 0

      while (num > 0) {
        modulo = (num - 1) % 26
        name = ('A'.toInt + modulo).toChar + name
        num = (num - modulo) / 26
      }
      name
    }
  }

  
  // genKOSize - generates KO-size for number of players
  def genKOSize(cntPlayer: Int): Int = cntPlayer match {
    case 2                          =>   2
    case x if (3  <= x && x <= 4)   =>   4
    case x if (5  <= x && x <= 8)   =>   8
    case x if (9  <= x && x <= 16)  =>  16
    case x if (17 <= x && x <= 32)  =>  32
    case x if (33 <= x && x <= 64)  =>  64
    case x if (65 <= x && x <= 128) => 128
    case _                          =>   0
  }

  // genKORnds - generates KO-Rounds for number of players
  def genKORnds(cntPlayer: Int): Int = cntPlayer match {
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