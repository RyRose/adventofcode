//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d08.p2.fast

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream

enum Direction {
  case Left, Right
}

case class ProblemState(
    instructions: List[Direction],
    network: Map[String, (String, String)]
)

case class Location(node: String, step: Int, idx: Int) {
  def key: (String, Int) = (node, idx)
}

case class NProblemState(
    problemState: ProblemState,
    loops: Map[String, List[Location]]
)

object Main extends IOApp {

  val lettersRe = "([A-Z]|[0-9])+".r

  def gcd(a: Long, b: Long): Long = {
    if (b == 0) a else gcd(b, a % b)
  }

  def lcm(a: Long, b: Long): Long = {
    (a * b).abs / gcd(a, b)
  }

  def parse(input: String): ProblemState = {
    val lines = input.split("\n").toList
    val instructions = lines.head.split("").toList.map {
      case "L" => Direction.Left
      case "R" => Direction.Right
    }
    val network = lines.tail.tail.map { line =>
      val parts = lettersRe.findAllIn(line).toList
      parts(0) -> (parts(1), parts(2))
    }.toMap
    ProblemState(instructions, network)
  }

  def parse2(state: ProblemState): NProblemState = {
    val loops = state.network.keys
      .filter(_.endsWith("A"))
      .map { node =>
        var cur = node
        var idx = 0
        var steps = 0
        val locations = scala.collection.mutable.ListBuffer[Location]()
        while (locations.isEmpty || locations.head.key != (cur, idx)) {
          if (cur.endsWith("Z")) {
            locations += Location(cur, steps, idx)
          }
          val split = state.network.get(cur).get
          cur = state.instructions(idx) match {
            case Direction.Left  => split(0)
            case Direction.Right => split(1)
          }
          idx += 1
          idx %= state.instructions.length
          steps += 1
        }
        locations += Location(cur, steps, idx)
        (node, locations.toList)
      }
      .toMap
    NProblemState(state, loops)
  }

  def solve(state: NProblemState): Long = {
    var ret = 1L
    for ((_, locations) <- state.loops) {
      ret = lcm(ret, locations.head.step)
    }
    ret
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(parse)
      .map(parse2)
      .map(solve)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
