import scala.io.Source
import scala.util.Using
import scala.util.matching.Regex

def stringToInt(s: String): Int = {
  s match {
    case "one"   => 1
    case "two"   => 2
    case "three" => 3
    case "four"  => 4
    case "five"  => 5
    case "six"   => 6
    case "seven" => 7
    case "eight" => 8
    case "nine"  => 9
    case _ =>
      s.toIntOption.getOrElse(
        throw new IllegalArgumentException(s"bad string: $s")
      )
  }
}

def findMatch(pattern: Regex, line: String, reverse: Boolean): Int = {
  pattern.findFirstIn(line) match {
    case Some(value) =>
      value.toIntOption match
        case Some(num)       => num
        case None if reverse => stringToInt(value.reverse)
        case None            => stringToInt(value)
    case None => throw new IllegalArgumentException(s"bad line: $line")
  }
}

@main
def hello(filepath: String): Unit = {
  val numberPattern = "one|two|three|four|five|six|seven|eight|nine|[0-9]".r
  val numberPatternBackward =
    "eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9]".r
  Using(Source.fromFile(filepath)) { source =>
    var result = 0
    source
      .getLines()
      .foreach({ line =>
        {
          val firstMatch = findMatch(numberPattern, line, reverse = false)
          val secondMatch = findMatch(
            numberPatternBackward,
            line.reverse,
            reverse = true
          )
          result += (firstMatch * 10) + secondMatch
        }
      })
    println(result)
  }
}
