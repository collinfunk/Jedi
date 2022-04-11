package expression
import value.Value
import context.Enviornment

class Literal extends Expression with Value {
  def execute(env: Enviornment): Value = this
}
