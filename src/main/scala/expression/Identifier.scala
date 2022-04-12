package expression
import context.Enviornment
import value.Value


case class Identifier(val name: String) extends Expression {
  override def toString = name

  override def execute(env: Enviornment): Value = env(this)
}
