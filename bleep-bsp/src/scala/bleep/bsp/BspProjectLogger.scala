package bleep.bsp

/** Project-scoped logger that sends BSP log messages.
  *
  * All logging in bleep-bsp goes through this interface, ensuring messages are always associated with a specific project.
  */
trait BspProjectLogger {
  def info(message: String): Unit
  def warn(message: String): Unit
  def error(message: String): Unit
}

object BspProjectLogger {
  val silent: BspProjectLogger = new BspProjectLogger {
    def info(message: String): Unit = ()
    def warn(message: String): Unit = ()
    def error(message: String): Unit = ()
  }
}
