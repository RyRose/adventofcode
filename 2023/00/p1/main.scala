//> using dep "org.typelevel::cats-effect:3.6.1"
//> using dep "co.fs2::fs2-io:3.12.0"

import cats.effect.{IO, IOApp}
import fs2.io.file.{Files, Path}
import fs2.Stream
import cats.effect.ExitCode

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
