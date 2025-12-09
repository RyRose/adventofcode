//> using dep "org.typelevel::cats-effect:3.6.3"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d12.p1

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream

case class ProblemState0(line: String, locations: List[Int])

object Main extends IOApp {

  def solve0(input: String): ProblemState0 = {
    val Array(line, rawSprings) = input.split(" ")
    ProblemState0(
      line.trim,
      rawSprings.split(",").toList.map(_.trim).map(_.toInt)
    )
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8Lines(Path(args.head))
      .evalTap(IO.println)
      .filter(_.nonEmpty)
      .map(solve0)
      .evalTap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
