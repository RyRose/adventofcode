//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d05.p2

import cats.effect.{IO, IOApp, ExitCode}
import fs2.io.file.{Files, Path}

case class Range(source: Long, destination: Long, length: Long) {
  def sourceEnd: Long = source + length
  def destinationEnd: Long = destination + length
}

case class ProblemState(seedRanges: List[(Long, Long)], maps: List[List[Range]])

object PS2Fast extends IOApp {

  val intRe = "[0-9]+".r

  def parseRaw(contents: String): ProblemState = {
    val groups = contents.split("\n\n").toList

    val seedRanges = intRe
      .findAllIn(groups.head)
      .map(_.toLong)
      .toList
      .grouped(2)
      .collect { case List(start, len) => (start, len) }
      .toList

    val maps = groups.tail.map { group =>
      group.linesIterator
        .filter(_.exists(_.isDigit)) // skip headers
        .map(intRe.findAllIn(_).map(_.toLong).toList)
        .collect { case List(dest, src, len) => Range(src, dest, len) }
        .toList
    }

    ProblemState(seedRanges, maps)
  }

  def mapRanges(
      inputRanges: List[(Long, Long)],
      map: List[Range]
  ): List[(Long, Long)] = {
    val result = scala.collection.mutable.ListBuffer.empty[(Long, Long)]

    for ((start, length) <- inputRanges) {
      var pending = List((start, length))

      for (r <- map) {
        val next = scala.collection.mutable.ListBuffer.empty[(Long, Long)]

        while (pending.nonEmpty) {
          val (s, l) = pending.head
          pending = pending.tail

          val e = s + l
          val rStart = r.source
          val rEnd = r.source + r.length

          if (e <= rStart || s >= rEnd) {
            // No overlap
            next += ((s, l))
          } else {
            // Before overlap
            if (s < rStart)
              next += ((s, rStart - s))

            // Overlapping region
            val overlapStart = s.max(rStart)
            val overlapEnd = e.min(rEnd)
            val overlapLen = overlapEnd - overlapStart
            val mappedStart = r.destination + (overlapStart - r.source)
            result += ((mappedStart, overlapLen))

            // After overlap
            if (e > rEnd)
              pending = ((rEnd, e - rEnd)) :: pending
          }
        }

        pending = next.toList
      }

      result ++= pending
    }

    result.toList
  }

  def solve(state: ProblemState): Long = {
    val finalRanges = state.maps.foldLeft(state.seedRanges)(mapRanges)
    finalRanges.map(_._1).min
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(parseRaw)
      .map(solve)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
