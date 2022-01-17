package shared.utils

case class UpdateTrigger(cmdName: String, ident: String, toId: Long, coId: Long, coPh: Int, grId: Int) {
  override def toString = s"$cmdName#$ident#$toId#$coId#$coPh#$grId"
}

object UpdateTrigger {
  // The injection method (optional)
  def apply(cmdName: String, ident: String, toId: Long, coId: Long, grId: Int) = new UpdateTrigger(cmdName,ident,toId,0L,0,0)
  def apply(cmdName: String, toId: Long) = new UpdateTrigger(cmdName,"000000",toId,0L,0,0)
  def apply(cmdName: String, ident: String, toId: Long) = new UpdateTrigger(cmdName,ident,toId,0L,0,0)
  
  // The extraction method (mandatory)
  def unapply(str: String): Option[(String, String, Long, Long, Int, Int)] = {
    val parts = str split "#"
    try Some(parts(0).trim, parts(1).trim, parts(2).toLong, parts(3).toLong, parts(4).toInt, parts(5).toInt)
    catch { case _: Throwable => None }
  }  
}
