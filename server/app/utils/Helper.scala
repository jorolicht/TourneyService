package tourn.utils

import play.api.{ Environment, Configuration, Logging }

/**
  * Helper Utilities
  */
object Helper {

  def getConfig(entry: String, default:String="")(implicit cfg: Configuration) = {
    if (cfg.has(entry)) cfg.get[String](entry) else default
  }

  def getConfig(entry: String, default:Boolean)(implicit cfg: Configuration) = {
    if (cfg.has(entry)) cfg.get[Boolean](entry) else default
  }
  
  def getConfig(entry: String, default:Long)(implicit cfg: Configuration) = {
    if (cfg.has(entry)) cfg.get[Long](entry) else default
  }

}
