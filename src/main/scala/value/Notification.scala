package value

class Notification(message: String) extends Value {
  override def toString: String = message
}

object Notification {
  val OK = Notification("OK")
  val DONE = Notification("DONE")
  val UNSPECIFIED = Notification("UNSPECIFIED")
}
