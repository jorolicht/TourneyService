// package shared.model.tabletennis

// import upickle.default._
// import upickle.default.{ReadWriter => RW, macroRW}

// import shared.utils.Constants._


// // relevant information of a player within an competition
// case class PlayerEntry(
//   var sno:    String,         // start number(s) concatenated string of player identifieres  
//   val name:   String,                     
//   val club:   String, 
//   val rating: Int,            // eg. ttr for table tennis
//   var place:  (Int,Int),      // position after finishing the round (group or ko)
// ) {
//   //def stringify() = s"${sno}·${pos}·${plId}·${plId2}·${name}·${club}·${place._1}·${place._2}"
//   def stringify() = s"${sno}^${name}^${club}^${rating}^${place._1}^${place._2}^_"
// }


// object PlayerEntry {
//   implicit def rw: RW[PlayerEntry] = macroRW
//   def obify(peStr: String): PlayerEntry = {
//     val p = peStr.split("\\^")
//     try { 
//       PlayerEntry(p(0), p(1), p(2), p(3).toInt, (p(4).toInt, p(5).toInt))
//     } catch { case _: Throwable => PlayerEntry("0","","",(0,0)) }
//   }

//   def bye() =  PlayerEntry(PLID_BYE.toString, "FREILOS", "", 0, (0, 0))
// }