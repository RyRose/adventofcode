//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d06.ps1

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.ExitCode
import cats.implicits._

// Time:      7  15   30
// Distance:  9  40  200
// 2 5
// 4 11
// 11 19

case class Race(time: Long, distance: Long)

case class ProblemState(races: List[Race])

object PS1 extends IOApp {

  val intRe = "\\d+".r

  def parse(input: String): ProblemState = {
    val lines     = input.split("\n")
    val times     = intRe.findAllIn(lines(0)).map(_.toLong)
    val distances = intRe.findAllIn(lines(1)).map(_.toLong)
    ProblemState(
      times.zip(distances).map((time, distance) => Race(time, distance)).toList
    )
  }

  def binarySearch(
      start: Long,
      end: Long,
      target: Long,
      apply: (Long) => Long
  ): Long = {
    var low  = start
    var high = end
    while (low < high) {
      val mid    = (low + high) / 2
      val result = apply(mid)
      if (result <= target) {
        low = mid + 1
      } else {
        high = mid
      }
    }
    low
  }

  def solve(race: Race): Long = {
    val start = 0
    val end   = race.time
    val res   = binarySearch(0, end, race.distance, (x) => x * (end - x))
    math.abs((race.time - res) - res) + 1
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(parse)
      .map(_.races.map(solve))
      .map(_.fold(1L)(_ * _.max(1L)))
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
