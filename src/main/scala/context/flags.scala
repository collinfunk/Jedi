package context
import value._

object flags {
  val BY_REF = 0
  val BY_NAME = 1
  val BY_TEXT = 2
  var paramPassing = BY_NAME
  var staticScope = true
  var errorRaised: Value = Notification.UNSPECIFIED
  def setError(gripe: String) = { errorRaised = new Error(Chars(gripe))}
  def getError = errorRaised.asInstanceOf[Error]
  def errorSet: Boolean = errorRaised.isInstanceOf[Error]
  def clearError = {errorRaised = Notification.UNSPECIFIED}
}