package shared.model

import upickle.default._
import java.util.UUID


/*
 * Tourney Director
 */
case class Director(
  userId:       UUID,
  name:         String,
  email:        String,                   
  address:      String,
  password:     String,       // hash value of password
  allowContact: Boolean,
  lastContact:  String
) {
  def stringify  = write((userId, name, email, address, password, allowContact, lastContact))
}

object Director {
  def tupled = (this.apply _).tupled
  def obify(x: String) = tupled(read[(UUID, String, String, String, String, Boolean, String)](x))
}