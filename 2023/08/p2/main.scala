//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d08.p2

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

object Main extends IOApp {

  val lettersRe = "([A-Z]|[0-9])+".r

  def parse(input: String): ProblemState = {
    val lines        = input.split("\n").toList
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

  def solve(state: ProblemState): Int = {
    var nodes = state.network.keys.filter(_.endsWith("A")).toArray
    var i     = 0
    var steps = 0
    while (nodes.exists(!_.endsWith("Z"))) {
      if (steps % 100000 == 0) {
        println(s"Steps: $steps, Current nodes: ${nodes.mkString(", ")}")
      }
      nodes.zipWithIndex.foreach { case (node, index) =>
        val split = state.network.get(node).get
        nodes(index) = state.instructions(i) match {
          case Direction.Left  => split(0)
          case Direction.Right => split(1)
        }
      }
      steps += 1
      i += 1
      i %= state.instructions.length
    }
    steps
  }

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8(Path(args.head))
      .map(parse)
      .map(solve)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
