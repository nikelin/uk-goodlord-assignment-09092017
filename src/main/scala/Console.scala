import expressions._

import scala.util.Try

object Console extends App {
  import scalaz.effect._
  import IO._
  import lcs._
  import Expression._

  def readInt: IO[Option[Int]] = readLn.map(v => Try(v.toInt).toOption)

  def error(msg: String): IO[Unit] = putStrLn(s"[ERROR] - $msg")

  def runLCS: IO[Unit] =
    for {
      _ <- putStrLn("Left string:")
      leftString <- readLn
      _ <- putStrLn("Right string:")
      rightString <- readLn
      result = lcs(leftString, rightString)
      _ <- putStrLn(s"Result = $result")
    } yield {}

  def runExpressions: IO[Unit] = {
    def readItems(list: List[Int]): IO[List[Int]] =
      for {
        _ <- putStrLn(s"Enter a positive integer or any other key to finish: ${list.mkString(",")}")
        item <- readInt
        items <- item match {
          case Some(v) if v > 0 => readItems(v :: list)
          case _ => IO(list)
        }
      } yield items

    def readTarget: IO[Int] =
      for {
        _ <- putStrLn("Enter a positive integer for a target number: ")
        numberOpt <- readInt
        number <- numberOpt match {
          case Some(v) => IO(v)
          case None => readTarget
        }
      } yield number

    for {
      items <- readItems(Nil)
      target <- readTarget
      variants = findSolution(items, target)
      _ <- variants match {
        case Some(solution) => putStrLn(printExpression(solution))
        case _ => error("No solution exists")
      }
    } yield {}
  }

  def program: IO[Unit] =
    for {
      _ <- putStrLn("Task (1 - LCS, 2 - Expressions): ")
      taskId <- readInt
      result <- taskId match {
        case Some(1) => runLCS
        case Some(2) => runExpressions
        case _ =>
          error("Need a task") flatMap { _ =>
            program
          }
      }
    } yield result

  program.unsafePerformIO()

}
