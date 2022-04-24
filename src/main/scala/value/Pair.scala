package value

case class Pair(first: Value, second: Value ) extends Value {
  override def toString: String = "(" +first.toString + ", " + second.toString + ")"
}
