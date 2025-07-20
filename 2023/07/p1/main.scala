//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d07.p1

import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream

enum Type {
  case HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse,
    FourOfAKind, FiveOfAKind
}

case class Hand(rawHand: String, handType: Type, cards: List[Int], bid: Int) {
  def key: (Int, List[Int]) = (handType.ordinal, cards)
}

object Main extends IOApp {

  def cardValue(card: Char): Int = {
    card match {
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
      case 'T' => 10
      case 'J' => 11
      case 'Q' => 12
      case 'K' => 13
      case 'A' => 14
    }
  }

  def cardsHandType(hand: String): Type = {
    val cardValues = hand.map(cardValue).sorted
    val grouped    = cardValues.groupBy(identity).view.mapValues(_.size).toMap
    grouped.size match {
      case 5 => Type.HighCard
      case 4 => Type.OnePair
      case 3 =>
        if (grouped.values.exists(_ == 3)) Type.ThreeOfAKind else Type.TwoPair
      case 2 =>
        if (grouped.values.exists(_ == 4)) Type.FourOfAKind else Type.FullHouse
      case 1 => Type.FiveOfAKind
    }

  }

  def parse(input: String): List[Hand] = {
    input.split("\n").toList.map { line =>
      val Array(rawCards, rawBid) = line.split(" ")
      Hand(
        rawCards,
        cardsHandType(rawCards),
        rawCards.map(cardValue).toList,
        rawBid.toInt
      )
    }
  }

  def solve(hands: List[Hand]): Int = {
    hands
      .sortBy(_.key)
      .zipWithIndex
      .collect { case (hand, index) =>
        (index + 1) * hand.bid
      }
      .sum
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
