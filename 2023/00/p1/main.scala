//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d00.p1

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream

object Main extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    Files[IO]
      .readUtf8Lines(Path(args.head))
      .filter(line => line.nonEmpty)
      .evalMap(IO.println)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
