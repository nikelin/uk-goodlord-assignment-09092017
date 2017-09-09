package expressions

import Expression._

package object constraints {
  /**
    * Expression constraints
    *
    * This trait represents possible constraints imposed by the evaluation environment, i.e. non-negative sub-groups.
    */
  trait ExpressionConstraint {
    def check(op: Expression): Boolean
  }

  object NonNegativeExpressionConstraint extends ExpressionConstraint {
    override def check(op: Expression): Boolean = {
      evaluateExpression(op) > 0
    }
  }
}
