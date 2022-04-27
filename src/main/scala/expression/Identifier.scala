package expression
import context.Enviornment
import value.{Thunk, Text, Value}


case class Identifier(name: String) extends Expression {
  override def toString = name

  override def execute(env: Enviornment): Value =
    env(this) match {
      case thunk: Thunk => thunk()
      case text: Text => text.body.execute(env)
      case value: Value => value
    }
}
