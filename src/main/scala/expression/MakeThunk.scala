package expression
import context.Enviornment
import value.{Value, Thunk}

case class MakeThunk(body: Expression) extends SpecialForm {
  override def execute(env: Enviornment): Value = new Thunk(body, env)

}
