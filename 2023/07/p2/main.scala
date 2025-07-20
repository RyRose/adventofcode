//> using dep "org.typelevel::cats-effect:3.6.2"
//> using dep "co.fs2::fs2-io:3.12.0"

package y2023.d07.p2

import scala.math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.io.file.{Files, Path}
import fs2.Stream

enum Type {
  case Empty, HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse,
    FourOfAKind, FiveOfAKind

  def increment(jokers: Int): Type = {
    (this, jokers) match {
      case (t, j) if j == 0       => t
      case (Type.Empty, j)        => Type.HighCard.increment(j - 1)
      case (Type.HighCard, j)     => Type.OnePair.increment(j - 1)
      case (Type.OnePair, j)      => Type.ThreeOfAKind.increment(j - 1)
      case (Type.TwoPair, j)      => Type.FullHouse.increment(j - 1)
      case (Type.ThreeOfAKind, j) => Type.FourOfAKind.increment(j - 1)
      case (Type.FourOfAKind, j)  => Type.FiveOfAKind.increment(j - 1)
      case _                      =>
        throw new MatchError(
          s"No match found for value: $this and jokers: $jokers"
        )
    }
  }
}

case class Hand(rawHand: String, handType: Type, cards: List[Int], bid: Int) {
  def key: (Int, List[Int]) = (handType.ordinal, cards)
}

object Main extends IOApp {

  def cardValue(card: Char): Int = {
    card match {
      case 'J' => 1
      case '2' => 2
      case '3' => 3
      case '4' => 4
      case '5' => 5
      case '6' => 6
      case '7' => 7
      case '8' => 8
      case '9' => 9
      case 'T' => 10
      case 'Q' => 12
      case 'K' => 13
      case 'A' => 14
    }
  }

  def cardsHandType(hand: String): Type = {
    val jokers     = hand.count(_ == 'J')
    val cardValues = hand.filter(_ != 'J').map(cardValue).sorted
    val grouped    = cardValues.groupBy(identity).view.mapValues(_.size).toMap
    val handType   = true match {
      case _ if grouped.values.exists(_ == 5) => Type.FiveOfAKind
      case _ if grouped.values.exists(_ == 4) => Type.FourOfAKind
      case _ if grouped.values.exists(_ == 3) =>
        if (grouped.values.exists(_ == 2)) Type.FullHouse else Type.ThreeOfAKind
      case _ if grouped.values.count(_ == 2) == 2 => Type.TwoPair
      case _ if grouped.values.exists(_ == 2)     => Type.OnePair
      case _ if grouped.values.exists(_ == 1)     => Type.HighCard
      case _: Boolean                             => Type.Empty
    }
    handType.increment(jokers)
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
