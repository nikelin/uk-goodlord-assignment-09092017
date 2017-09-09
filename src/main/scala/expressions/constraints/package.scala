package expressions

import expressions.Expression._

package object constraints {

  trait ExpressionConstraint {
    def isSatisfied(exp: Expression): Boolean
  }

  object HasNoNegativeGroups extends ExpressionConstraint {
    override def isSatisfied(expression: Expression): Boolean = {
      expression match {
        case Literal(x) => x > 0
        case Sub(x: Expression, y: Expression) => evaluateExpression(x) - evaluateExpression(y) > 0
        case Add(x: Expression, y: Expression) => evaluateExpression(x) + evaluateExpression(y) > 0
        case Mul(x, y) => isSatisfied(x) || isSatisfied(y)
      }
    }
  }

}
