package expression
import context.Enviornment
import value.Value

case class Block(expressions: List[Expression]) extends SpecialForm {
  override def execute(env: Enviornment): Value = {
    val tempEnviornment = new Enviornment(env)
    expressions.map(_.execute(tempEnviornment)).last
  }
}
