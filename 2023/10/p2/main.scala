//> using dep "org.typelevel::cats-effect:3.6.3"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d10.p2

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream
import scala.annotation.tailrec
import scala.collection.mutable

enum Tile {
  case Vertical, Horizontal, NE, NW, SE, SW, Ground, Start, Outside, InnerPath,
    Empty
}

case class TilePath(tile: Tile, path: Boolean)

case class ProblemState(tiles: Array[Array[TilePath]])

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
    case '█' => Tile.InnerPath
    case _ => throw new IllegalArgumentException(s"Unknown tile character: $c")
  }

  def tileToChar(tile: Tile): Char = tile match {
    case Tile.Vertical   => '|'
    case Tile.Horizontal => '-'
    case Tile.NE         => 'L'
    case Tile.NW         => 'J'
    case Tile.SW         => '7'
    case Tile.SE         => 'F'
    case Tile.Ground     => '.'
    case Tile.Start      => 'S'
    case Tile.Outside    => ' '
    case Tile.Empty      => '*'
    case Tile.InnerPath  => '█'
  }

  def printProblemState(state: ProblemState): String = {
    state.tiles
      .map { row =>
        row.map {
          case TilePath(tile, true)  => s"\u001b[7m${tileToChar(tile)}\u001b[0m"
          case TilePath(tile, false) => s"${tileToChar(tile)}"

        }.mkString
      }
      .mkString("\n")
  }

  def parse(input: String): ProblemState = {
    val separator = TilePath(Tile.Empty, false)
    val tiles =
      input.linesIterator
        .map(line => {
          val arr = line.toArray.map(c => TilePath(charToTile(c), false))
          arr.init.flatMap(x => Array(x, separator)) :+ arr.last
        })
        .toArray
    ProblemState(
      tiles.init.flatMap(x =>
        Array(x, Array.fill(tiles.head.length)(separator))
      ) :+
        tiles.last
    )
  }

  def check(
      state: ProblemState,
      len: Int,
      prev: (Int, Int),
      x: Int,
      y: Int
  ): Boolean = {
    if (
      x < 0 || y < 0 || x >= state.tiles.length || y >= state.tiles(0).length
    ) {
      false
    } else {
      val cur = (x, y)
      val mid = if (prev == (-1, -1)) {
        None
      } else {
        val dx = cur(0) - prev(0)
        val dy = cur(1) - prev(1)
        val m = (prev(0) + dx / 2, prev(1) + dy / 2)
        val mv = state.tiles(m(0))(m(1))
        state.tiles(m(0))(m(1)) = TilePath(Tile.InnerPath, true)
        Some((m, mv))
      }
      val tp = state.tiles(x)(y)
      state.tiles(x)(y) =
        tp.copy(path = true) // Mark the tile as part of the path
      val ret = tp match {
        case TilePath(_, true)            => false // Already visited this tile
        case TilePath(Tile.Ground, false) => false
        case TilePath(Tile.Start, false) if len == 0 => {
          state.tiles(x)(y) = TilePath(Tile.Start, false)
          check(state, len + 1, cur, x + 2, y)
          || check(state, len + 1, cur, x - 2, y)
          || check(state, len + 1, cur, x, y + 2)
          || check(state, len + 1, cur, x, y - 2)
        }
        case TilePath(Tile.Start, false) if len <= 2 => false
        case TilePath(Tile.Start, false) =>
          true
        case TilePath(Tile.Horizontal, false) =>
          check(state, len + 1, cur, x, y + 2) || check(
            state,
            len + 1,
            cur,
            x,
            y - 2
          )
        case TilePath(Tile.Vertical, false) =>
          check(state, len + 1, cur, x + 2, y) || check(
            state,
            len + 1,
            cur,
            x - 2,
            y
          )
        case TilePath(Tile.NE, false) =>
          check(state, len + 1, cur, x - 2, y) || check(
            state,
            len + 1,
            cur,
            x,
            y + 2
          )
        case TilePath(Tile.NW, false) =>
          check(state, len + 1, cur, x - 2, y) || check(
            state,
            len + 1,
            cur,
            x,
            y - 2
          )
        case TilePath(Tile.SE, false) =>
          check(state, len + 1, cur, x + 2, y) || check(
            state,
            len + 1,
            cur,
            x,
            y + 2
          )
        case TilePath(Tile.SW, false) =>
          check(state, len + 1, cur, x + 2, y) || check(
            state,
            len + 1,
            cur,
            x,
            y - 2
          )
        case TilePath(Tile.Outside, false) => false
        case TilePath(Tile.InnerPath, false) =>
          throw new RuntimeException(
            "InnerPath tile found in check"
          )
        case TilePath(Tile.Empty, false) =>
          throw new RuntimeException("Empty tile found in check")
      }
      if (!ret) {
        state.tiles(x)(y) = tp
        mid match {
          case Some((m, mv)) =>
            state.tiles(m(0))(m(1)) = mv
          case None =>
        }
      }
      ret
    }
  }

  def markOutside(state: ProblemState, x: Int, y: Int): Unit = {
    if (
      x < 0 || y < 0 || x >= state.tiles.length || y >= state.tiles(0).length
    ) {
      return // Out of bounds
    }
    state.tiles(x)(y) match {
      case TilePath(Tile.Outside, _) => return // Already marked as outside
      case TilePath(_, true)         => return // Already part of the path
      case _                         => // Continue marking
    }
    state.tiles(x)(y) = TilePath(Tile.Outside, false) // Mark as outside
    // Recursively mark adjacent tiles
    markOutside(state, x + 1, y)
    markOutside(state, x - 1, y)
    markOutside(state, x, y + 1)
    markOutside(state, x, y - 1)
  }

  def solve3(state: ProblemState): ProblemState = {
    state.tiles.head.indices.foreach { colIndex =>
      markOutside(state, 0, colIndex)
      markOutside(state, state.tiles.length - 1, colIndex)
    }

    state.tiles.indices.foreach { rowIndex =>
      markOutside(state, rowIndex, 0)
      markOutside(state, rowIndex, state.tiles.head.length - 1)
    }
    state
  }

  def solve4(state: ProblemState): Int = {
    state.tiles.map { row =>
      row.count(tile =>
        !tile.path && tile.tile != Tile.Outside && tile.tile != Tile.InnerPath && tile.tile != Tile.Empty
      )
    }.sum

  }

  def solve(state: ProblemState): IO[Unit] =
    state.tiles.zipWithIndex
      .foreach { case (row, rowIndex) =>
        row.zipWithIndex.collectFirst {
          case (TilePath(Tile.Start, _), startIndex) =>
            check(state, 0, (-1, -1), rowIndex, startIndex)
        }
      }
    IO.unit

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(parse)
      .evalTap(solve)
      .map(solve3)
      .map(solve4)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
