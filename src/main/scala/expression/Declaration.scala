package expression
import context.Enviornment
import value._

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm {
  override def execute(env: Enviornment): Value =
    env(identifier) = expression.execute(env)
    Notification.OK
}
