import scala.util.Using
import scala.io.Source
import scala.collection.mutable

@main
def main2(filepath: String): Unit = {
  val integerRE = "[0-9]+".r
  val letterRE  = "[a-z]+".r
  var total     = 0
  Using(Source.fromFile(filepath)) { source =>
    for (line <- source.getLines()) {
      val List(game, bag) = line.split(":").toList
      val gameIndex       = integerRE.findFirstIn(game).get.toInt
      var map             = mutable.Map(
        "red"   -> 0,
        "green" -> 0,
        "blue"  -> 0
      )
      bag.split(";").foreach { grab =>
        grab.split(",").foreach { item =>
          val itemCount = integerRE.findFirstIn(item).get.toInt
          val word      = letterRE.findFirstIn(item).get
          map(word) = math.max(map(word), itemCount)
        }
      }
      var power = 1
      map.values.foreach { count =>
        power *= count
      }
      total += power
    }
  }.recover { case e: Exception =>
    println(s"An error occurred: ${e.getMessage}")
  }

  println(s"Total: $total")
}
