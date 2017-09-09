import expressions.constraints.{ExpressionConstraint, HasNoNegativeGroups}

package object expressions {
  import Expression._

  private def computeAllOperations(subList: Iterable[Int]): Stream[Expression] = {
    def operations(x: Expression, y: Expression): List[Expression] =
      List(Expression.Mul(x, y), Expression.Add(x, y), Expression.Sub(x, y))

    subList match {
      case Nil => Stream.empty
      case _ :: Nil =>
        subList.map(Expression.Literal).toStream
      case x :: xs =>
        for {
          y <- computeAllOperations(xs)
          z <- operations(Expression.Literal(x), y)
        } yield z
    }
  }

  private def permute(items: List[Int]): Iterator[List[Int]] =
    items.toSet.subsets.flatMap(v => v.toList.permutations)

  private def combinations(items: List[Int])(implicit constraint: ExpressionConstraint): Stream[Expression] =
    permute(items).toStream.flatMap(computeAllOperations(_).filter(constraint.isSatisfied))

  def findSolutions(items: List[Int], target: Int)(implicit constraint: ExpressionConstraint = HasNoNegativeGroups): Stream[Expression] =
    combinations(items).filter(evaluateExpression(_) == target)

  def findSolution(items: List[Int], target: Int)(implicit constraint: ExpressionConstraint = HasNoNegativeGroups): Option[Expression] =
    combinations(items).find(evaluateExpression(_) == target)

}
