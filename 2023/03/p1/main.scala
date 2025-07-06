//> using dep "org.typelevel::cats-effect:3.6.1"
//> using dep "co.fs2::fs2-io:3.12.0"

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.ExitCode

object Main03P1 extends IOApp {

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

    def containsSymbol(i: Int, j: Int): Boolean = {
      if (i < 0 || i >= arr.length || j < 0 || j >= arr(i).length) false
      else
        arr(i)(j) match {
          case "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" |
              "." =>
            false
          case _ => true
        }
    }

    var ret = 0
    for (case (row, rowI) <- arr.zipWithIndex) {
      for (it <- integerPattern.findAllMatchIn(row.mkString(""))) {
        val valid = (it.start until it.end).exists(j =>
          val i = rowI
          (locations).exists((di, dj) => containsSymbol(i + di, j + dj))
        )
        if (valid) {
          ret += it.group(0).toInt
        }
      }
    }
    ret
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
