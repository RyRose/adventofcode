//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d09.p1

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream
import scala.collection.mutable.ListBuffer

// 10  13  16  21  30  45  68
//    3   3   5   9  15  23
//      0   2   4   6   8
//        2   2   2   2
//          0   0   0

case class ProblemState(lines: List[List[Long]])

object Main extends IOApp {

  val intRe = "(-|\\d)+".r

  def parse(input: String): ProblemState = {
    val lines = input.split("\n").toList
    ProblemState(lines.map(intRe.findAllIn(_).map(_.toLong).toList))
  }

  def solve(state: ProblemState): List[Long] = {
    state.lines.map { line =>
      val buf = ListBuffer.empty[List[Long]]
      buf += line
      while (buf.last.exists(_ != 0L)) {
        buf += buf.last
          .sliding(2)
          .collect { case List(a, b) => b - a }
          .toList
      }
      buf.map(_.lastOption.getOrElse(0L)).sum
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(parse)
      .map(solve)
      .map(_.sum)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
