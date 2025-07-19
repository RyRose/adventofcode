//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d05

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.ExitCode
import java.util.Arrays

case class Range(source: Long, destination: Long, length: Long) {
  def offset: Long = destination - source
  def contains(value: Long): Boolean =
    source <= value && value <= (source + length)
}

case class ProblemState(seeds: List[Long], maps: List[List[Range]])

object PS1 extends IOApp {

  val intRe = "[0-9]+".r

  def parseRaw(contents: String): ProblemState = {
    val lines = contents.split("\n").toList
    val seeds = intRe.findAllIn(lines.head).map(_.toLong).toList
    val ranges = lines.tail.map { line =>
      intRe.findAllIn(line).map(_.toLong).toList match {
        case List(destination, source, length) =>
          Some(Range(source, destination, length))
        case _ => None
      }
    }
    val maps = ranges.foldLeft(List[List[Range]]()) {
      case (acc, None) =>
        if (acc.isEmpty || !acc.last.isEmpty) acc :+ List()
        else acc
      case (acc, Some(value)) =>
        acc.init :+ (acc.last :+ value)
    }
    ProblemState(seeds, maps.map(_.sortBy(_.source)))
  }

  def solve(state: ProblemState): List[Long] = {
    state.seeds.map { seed =>
      println(s"Processing seed: $seed")
      state.maps.foldLeft(seed) { (current, ranges) =>
        println(s"Current seed: $current, Ranges: ${ranges.mkString(", ")}")
        Arrays.binarySearch(
          ranges.toArray,
          Range(current, 0, 0),
          (a: Range, b: Range) => { java.lang.Long.compare(a.source, b.source) }
        ) match {
          case index if index >= 0 =>
            println(s"Found exact match at index: $index")
            val range = ranges(index)
            current + range.offset
          case index if ranges(math.max(0, -index - 2)).contains(current) =>
            println(
              s"Found range containing current seed at index: $index"
            )
            val range = ranges(math.max(0, -index - 2))
            current + range.offset
          case index =>
            println(
              s"No applicable range found for current seed: $current, index: $index"
            )
            current // No applicable range found, return current seed
        }
      }
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(parseRaw)
      .map(solve)
      .map(_.min)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
