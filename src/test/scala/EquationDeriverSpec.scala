import org.scalatest.concurrent.{TimeLimits}
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class EquationDeriverSpec extends WordSpec with Matchers with TimeLimits {
  import expressions._
  import Expression._

  "Equation deriver" must {

    "exit within a finite time in a case of an increased amount of input data" in {
      // there should be a better metric to use, as the execution time may vary depending on the environment
      failAfter(500.millis) {
        findSolutions((1 to 100).toList, 42).take(1000)
      }
    }

    "only produce correct solutions" in {
      // there should be a better metric to use, as the execution time may vary depending on the environment
      failAfter(1000.millis) {
        val solutions = findSolutions((1 to 100).toList, 42).take(1000)
        solutions foreach { solution =>
          evaluateExpression(solution) should be (42)
        }
      }
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
