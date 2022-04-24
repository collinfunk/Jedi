package value

import context.Enviornment
import expression.Expression

class Thunk(val body: Expression, val defEnv: Enviornment) extends Closure(Nil, defEnv, body) {
  private var cache: Value = null
  def apply() =
    if(cache == null) {
      cache = super.apply(Nil, defEnv)
    }
    cache
}
