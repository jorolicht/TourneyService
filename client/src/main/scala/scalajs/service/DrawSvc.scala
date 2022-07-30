package scalajs.service

import scala.collection.mutable.{ ArrayBuffer, HashMap, Map }
import scala.scalajs._

import shared.model._
import shared.model.CompPhase._
import shared.utils._
import shared.utils.Routines._
import shared.utils.Constants._ 

import scalajs._


trait DrawSvc 
{
  

  
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