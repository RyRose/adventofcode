//> using dep "org.typelevel::cats-effect:3.6.1"
//> using dep "co.fs2::fs2-io:3.12.0"

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.ExitCode
import scala.collection.mutable.ArrayBuffer

object Main04P2 extends IOApp {

  val integerPattern = "[0-9]+".r

  def lineScore(line: String): Int = {
    val rawLine                = line.split(":").last
    val Array(rawWin, rawList) =
      rawLine.split('|').map(integerPattern.findAllIn(_))
    val winInts = rawWin.toSet
    rawList.count(winInts.contains(_))
  }

  def solve(input: Vector[Int]): Int = {
    var buffer = new ArrayBuffer[Int]()
    buffer.insertAll(0, Array.fill(input.length)(0))
    for (case (value, i) <- input.zipWithIndex) {
      if (value > 0) {
        for (j <- i + 1 until math.min(i + value + 1, input.length)) {
          buffer(j) = buffer(i) + buffer(j) + 1
        }
      }
    }
    buffer.sum + input.length
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8Lines(Path(args.head))
      .filter(_.nonEmpty)
      .map(lineScore)
      .compile
      .toVector
      .map(solve)
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
