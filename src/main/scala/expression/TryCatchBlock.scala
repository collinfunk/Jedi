package expression
import context.Enviornment
import value.Value

class TryCatchBlock(tryBlock: Block, catchBlock: Block) extends SpecialForm {
  override def execute(env: Enviornment): Value =
    if (tryBlock.execute(env).isInstanceOf[Error])
      tryBlock.execute(env)
    else
      catchBlock.execute(env)

}
