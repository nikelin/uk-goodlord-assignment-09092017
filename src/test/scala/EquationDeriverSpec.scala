import org.scalatest.{Matchers, WordSpec}

class EquationDeriverSpec extends WordSpec with Matchers {
  import expressions._
  import Expression._

  "Equation deriver" must {

    "exit within a finite time in a case of an increased amount of input data" in {
      findSolutions((1 to 100).toList, 42).take(1000)
    }

    "produce a correct equations list for a given solvable problem" in {
      val result = findSolutions(List(2, 3, 5, 6), 42)
      result should not be empty
      result should contain (Expression.Mul(Expression.Literal(6), Expression.Add(Expression.Literal(2), Expression.Literal(5))))
    }

    "produce a correct equation for a given solvable problem" in {
      val result = findSolution(List(2, 3, 5, 6), 42)
      result should not be empty
      result.get should be (Expression.Mul(Expression.Literal(6), Expression.Add(Expression.Literal(2), Expression.Literal(5))))
    }

  }

}
