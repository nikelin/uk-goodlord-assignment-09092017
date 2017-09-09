import expressions.constraints.ExpressionConstraint

package object expressions {
  import Expression._

  private def computeAllOperations(subList: Iterable[Int])(implicit validation: ExpressionConstraint): Iterable[Expression] = {
    def operations(x: Expression, y: Expression): List[Expression] =
      List(Expression.Mul(x, y), Expression.Add(x, y), Expression.Sub(x, y))

    subList match {
      case Nil => Nil
      case _ :: Nil =>
        subList map Expression.Literal
      case x :: xs =>
        for {
          y <- computeAllOperations(xs)
          z <- operations(Expression.Literal(x), y).filter(validation.check)
        } yield z
    }
  }

  private def permute(items: List[Int]): Iterator[List[Int]] =
    items.toSet.subsets.flatMap(v => v.toList.permutations)

  def combinations(items: List[Int]): Iterator[Expression] =
    permute(items).flatMap(computeAllOperations(_)(expressions.constraints.NonNegativeExpressionConstraint))

  def findSolution(items: List[Int], target: Int): Option[Expression] =
    combinations(items).find(evaluateExpression(_) == target)

}
