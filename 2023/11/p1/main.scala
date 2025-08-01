//> using dep "org.typelevel::cats-effect:3.6.3"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d11.p1

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream

case class ProblemState0(
    grid: List[List[Char]]
)

case class ProblemState1(
    grid: List[List[Char]],
    rows: List[Int],
    cols: List[Int]
)

case class Galaxy(x: Int, y: Int)

case class ProblemState2(
    galaxies: List[Galaxy],
    rows: List[Int],
    cols: List[Int]
)

case class ProblemState3(galaxies: List[Galaxy])

object Main extends IOApp {

  def solve0(input: String): ProblemState0 = {
    val lines = input.linesIterator.toList
    val grid  = lines.map(_.toList)
    ProblemState0(grid)
  }

  def solve1(input: ProblemState0): ProblemState1 = {
    val rows = input.grid
      .map(x => if (x.exists(_ == '#')) 0 else 1)
      .foldLeft((0, List.empty[Int])) { case ((sum, acc), x) =>
        (sum + x, acc :+ sum)
      }
      ._2
    val cols = input.grid.transpose
      .map(x => if (x.exists(_ == '#')) 0 else 1)
      .foldLeft((0, List.empty[Int])) { case ((sum, acc), x) =>
        (sum + x, acc :+ sum)
      }
      ._2
    ProblemState1(input.grid, rows, cols)
  }

  def solve2(input: ProblemState1): ProblemState2 = {
    val galaxies = for {
      (row, r) <- input.grid.zipWithIndex
      (col, c) <- row.zipWithIndex if col == '#'
    } yield Galaxy(r, c)
    ProblemState2(galaxies, input.rows, input.cols)
  }

  def solve3(input: ProblemState2): ProblemState3 = {
    ProblemState3(input.galaxies.map { galaxy =>
      Galaxy(galaxy.x + input.rows(galaxy.x), galaxy.y + input.cols(galaxy.y))
    })
  }

  def solve4(input: ProblemState3): Int = {
    input.galaxies.combinations(2).foldLeft(0) { (sum, pair) =>
      val (g1, g2) = (pair.head, pair.last)
      sum + math.abs(g1.x - g2.x) + math.abs(g1.y - g2.y)
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(solve0)
      .map(solve1)
      .map(solve2)
      .map(solve3)
      .map(solve4)
      .evalTap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
