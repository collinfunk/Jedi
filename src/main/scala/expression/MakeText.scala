package expression
import context.Enviornment
import value.{Value, Text}

case class MakeText(body: Expression) extends SpecialForm {
  override def execute(env: Enviornment): Value = new Text(body)

}