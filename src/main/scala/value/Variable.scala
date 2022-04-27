package value

case class Variable(content: Value) extends Value {
  override def toString: String = "[" + content + "]"
}
