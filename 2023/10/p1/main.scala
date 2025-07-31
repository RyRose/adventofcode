//> using dep "org.typelevel::cats-effect:3.6.3"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d10.p1

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream
import scala.annotation.tailrec

enum Tile {
  case Vertical, Horizontal, NE, NW, SE, SW, Ground, Start, Path
}

case class ProblemState(tiles: Array[Array[Tile]])

object Main extends IOApp {

  def charToTile(c: Char): Tile = c match {
    case '|' => Tile.Vertical
    case '-' => Tile.Horizontal
    case 'L' => Tile.NE
    case 'J' => Tile.NW
    case '7' => Tile.SW
    case 'F' => Tile.SE
    case '.' => Tile.Ground
    case 'S' => Tile.Start
    case _ => throw new IllegalArgumentException(s"Unknown tile character: $c")
  }

  def parse(input: String): ProblemState = {
    val tiles =
      input.linesIterator
        .map(line => line.toArray.map(c => charToTile(c)))
        .toArray
    ProblemState(tiles)
  }

  def check(state: ProblemState, len: Int, x: Int, y: Int): Boolean = {
    if (
      x < 0 || y < 0 || x >= state.tiles.length || y >= state.tiles(0).length
    ) {
      false
    } else {
      val tile = state.tiles(x)(y)
      state.tiles(x)(y) = Tile.Path
      println(s"Checking tile at ($x, $y): $tile, length: $len")
      val ret = tile match {
        case Tile.Ground => false
        case Tile.Start if len == 0 => {
          state.tiles(x)(y) = Tile.Start // Restore the tile
          check(state, len + 1, x + 1, y)
          || check(state, len + 1, x - 1, y)
          || check(state, len + 1, x, y + 1)
          || check(state, len + 1, x, y - 1)
        }
        case Tile.Start if len <= 2 => false
        case Tile.Start =>
          println(s"Found a path of length $len")
          println(s"$len+1//2 = " + ((len + 1) / 2))
          true
        case Tile.Horizontal =>
          check(state, len + 1, x, y + 1) || check(state, len + 1, x, y - 1)
        case Tile.Vertical =>
          check(state, len + 1, x + 1, y) || check(state, len + 1, x - 1, y)
        case Tile.NE =>
          check(state, len + 1, x - 1, y) || check(state, len + 1, x, y + 1)
        case Tile.NW =>
          check(state, len + 1, x - 1, y) || check(state, len + 1, x, y - 1)
        case Tile.SE =>
          check(state, len + 1, x + 1, y) || check(state, len + 1, x, y + 1)
        case Tile.SW =>
          check(state, len + 1, x + 1, y) || check(state, len + 1, x, y - 1)
        case Tile.Path => false
      }
      if (!ret) {
        state.tiles(x)(y) = tile
      }
      ret
    }
  }

  def solve(state: ProblemState): Int =
    state.tiles.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.indexOf(Tile.Start) match {
        case -1 => None
        case startIndex =>
          if (check(state, 0, rowIndex, startIndex)) Some(1) else None
      }
    }.sum

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .evalTap(IO.println)
      .map(parse)
      .map(solve)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
