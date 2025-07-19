//> using dep "org.typelevel::cats-effect:3.6.1"
//> using dep "co.fs2::fs2-io:3.12.0"

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.ExitCode

object Main04P1 extends IOApp {

  val integerPattern = "[0-9]+".r

  def lineScore(line: String): Int = {
    val rawLine                = line.split(":").last
    val Array(rawWin, rawList) =
      rawLine.split('|').map(integerPattern.findAllIn(_))
    val winInts = rawWin.toSet
    val count   = rawList.count(winInts.contains(_))
    if (count == 0) {
      0
    } else {
      math.pow(2, count - 1).toInt
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8Lines(Path(args.head))
      .filter(_.nonEmpty)
      .map(lineScore)
      .fold(0)(_ + _)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
