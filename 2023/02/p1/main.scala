import scala.util.Using
import scala.io.Source

@main
def main1(filepath: String): Unit = {
  val integerRE = "[0-9]+".r
  val letterRE = "[a-z]+".r
  val map = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14
  )
  var total = 0
  Using(Source.fromFile(filepath)) { source =>
    for (line <- source.getLines()) {
      val List(game, bag) = line.split(":").toList
      val gameIndex = integerRE.findFirstIn(game).get.toInt
      var valid = true
      bag.split(";").foreach { grab =>
        grab.split(",").foreach { item =>
          val itemCount = integerRE.findFirstIn(item).get.toInt
          val word = letterRE.findFirstIn(item).get
          if (map.get(word).get < itemCount) {
            valid = false
          }
        }
      }
      if (valid) {
        total += gameIndex
      }
    }
  }.recover { case e: Exception =>
    println(s"An error occurred: ${e.getMessage}")
  }

  println(s"Total: $total")
}
