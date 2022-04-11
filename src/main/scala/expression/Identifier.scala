package expression
import value.Value
import expression.Expression
import context.Enviornment


case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Enviornment) = env(this)
}
