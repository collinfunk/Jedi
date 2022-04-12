package expression
import context.Enviornment
import value._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
  override def execute(env: Enviornment): Value =
    if (condition.execute(env) == Boole.TRUE)
      consequent.execute(env)
    else if (alternative != null)
      alternative.execute(env)
    else {
      Notification.UNSPECIFIED
    }
}
