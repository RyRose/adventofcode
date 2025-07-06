//> using lib "org.typelevel::cats-effect:3.5.2"

import scala.io.Source
import scala.util.Using

@main
def hello(filepath: String): Unit = {
  val numberPattern = "[0-9]".r
  Using(Source.fromFile(filepath)) { source =>
    var result = 0
    source
      .getLines()
      .foreach({ line =>
        {
          val first = numberPattern.findFirstMatchIn(line) match
            case Some(value) => value.group(0).toInt
            case None => throw new IllegalArgumentException(s"bad line: $line")
          val last = numberPattern.findFirstMatchIn(line.reverse) match
            case Some(value) => value.group(0).toInt
            case None => throw new IllegalArgumentException(s"bad line: $line")
          result += first * 10 + last
        }
      })
    println(result)
  }
}
