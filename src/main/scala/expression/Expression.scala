package expression
import value.Value
import context.Enviornment


trait Expression {
  def execute(env: Enviornment): Value

}
