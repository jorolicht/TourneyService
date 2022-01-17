package scalajs.usecase


/** Helper
 * 
 */
object Helper {
  import scalajs.AppEnv
  def debug(func: => String, msg: =>String) = AppEnv.logger.debug(s"${func}-> ${msg}")
  def info(func:  => String, msg: =>String) = AppEnv.logger.info(s"${func}-> ${msg}")
  def warn(func:  => String, msg: =>String) = AppEnv.logger.warn(s"${func}-> ${msg}")
  def error(func: => String, msg: =>String) = AppEnv.logger.error(s"${func}-> ${msg}")
}