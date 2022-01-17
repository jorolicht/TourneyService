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

  // grpOptions generate all possible options for given size
  def grpOptions(size: Int): List[Int] = {
    def grpOptions21to128(size: Int): List[Int] = {
      val result = ArrayBuffer[Int]()  
      if (size % 3 == 0) result += CST_GRPS3 else result += CST_GRPS34
      if (size % 4 == 0) result += CST_GRPS4 else result += CST_GRPS45
      if (size % 5 == 0) result += CST_GRPS5 else result += CST_GRPS56
      result += CST_KO
      result += CST_SW
      result.to(List)
    }

    size match {
      case 3 | 4 | 5 => List(CST_KO, CST_SW, CST_JGJ)
      case 6         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS3)
      case 7         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS34)
      case 8         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS4)
      case 9         => List(CST_KO, CST_SW, CST_JGJ, CST_GRPS45)
      case 10        => List(CST_KO, CST_SW, CST_GRPS34, CST_GRPS5)
      case 11        => List(CST_KO, CST_SW, CST_GRPS34, CST_GRPS56)
      case 12        => List(CST_KO, CST_SW, CST_GRPS4, CST_GRPS6)
      case 13        => List(CST_KO, CST_SW, CST_GRPS45)
      case 14        => List(CST_KO, CST_SW, CST_GRPS45)
      case 15        => List(CST_KO, CST_SW, CST_GRPS3, CST_GRPS5)
      case 16        => List(CST_KO, CST_SW, CST_GRPS4, CST_GRPS56)
      case 17        => List(CST_KO, CST_SW, CST_GRPS45, CST_GRPS56)
      case 18        => List(CST_KO, CST_SW, CST_GRPS3, CST_GRPS45, CST_GRPS6)
      case 19        => List(CST_KO, CST_SW, CST_GRPS34, CST_GRPS45)
      case 20        => List(CST_KO, CST_SW, CST_GRPS4, CST_GRPS5)

      case i if (i > 21 && i <= 128) => grpOptions21to128(i)
      case _                         => List()        
    }
  }

}  