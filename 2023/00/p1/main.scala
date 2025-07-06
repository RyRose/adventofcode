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
