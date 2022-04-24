package expression
import context.Enviornment
import value.{Closure, Value}

case class Lambda(parameters: List[Identifier], body: Expression) extends SpecialForm {
  override def execute(env: Enviornment): Value =
    new Closure(parameters, env, body)
}
