//> using dep "org.typelevel::cats-effect:3.6.1"
//> using dep "co.fs2::fs2-io:3.12.0"

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.ExitCode
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class NumberLocation(num: Int, row: Int, col: Int, len: Int) {}

object Main03P2 extends IOApp {

  val integerPattern = "[0-9]+".r

  val locations = List(
    (0, 1),
    (1, 0),
    (1, 1),
    (0, -1),
    (-1, 0),
    (-1, -1),
    (-1, 1),
    (1, -1)
  )

  def solution(input: String): Int = {
    val arr = input.split("\n").map(_.split(""))

    var numbers = ArrayBuffer[NumberLocation]()
    for (case (row, rowI) <- arr.zipWithIndex) {
      for (it <- integerPattern.findAllMatchIn(row.mkString(""))) {
        numbers += NumberLocation(
          num = it.group(0).toInt,
          row = rowI,
          col = it.start,
          len = it.end - it.start
        )
      }
    }

    val ret = for {
      (row, i) <- arr.zipWithIndex
      (col, j) <- row.zipWithIndex if col == "*"
    } yield {
      val filtered = numbers.filter(loc => {
        locations.exists { case (x, y) =>
          !(loc.row != (i + x) || (j + y) < loc.col || (j + y) >= (loc.col + loc.len))
        }
      })
      if (filtered.length == 2) {
        val (l, r) = (filtered.head, filtered.last)
        l.num * r.num
      } else {
        0
      }
    }
    ret.sum
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(solution)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
