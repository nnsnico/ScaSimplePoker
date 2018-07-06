import cats._
import cats.implicits._

import Hand._
import Main.DiscardList
import PokerHand._

import scala.language.implicitConversions

sealed abstract class PokerHand(val index: Int)

object PokerHand {

  implicit val ord: Order[PokerHand] = Order.by((p: PokerHand) => p.index)

  case object HighCards extends PokerHand(0)

  case object OnePair extends PokerHand(1)

  case object TwoPair extends PokerHand(2)

  case object ThreeOfAKind extends PokerHand(3)

  case object Straight extends PokerHand(4)

  case object Flush extends PokerHand(5)

  case object FullHouse extends PokerHand(6)

  case object FourOfAKind extends PokerHand(7)

  case object StraightFlush extends PokerHand(8)

}

case class Hand(fromHand: List[Card]) {
  def pokerHand(hand: Hand): (PokerHand, Card) = {
    val hands: List[Hand => Option[(PokerHand, Card)]] =
      List(
        onePair,
        twoPair,
        threeOfAKind,
        straight,
        flush,
        fourOfAKind,
        fullHouse,
        straightFlush
      )

    val handResult: Option[(PokerHand, Card)] =
      Functor[List]
        .map(hands) { it =>
          it(hand)
        }
        .foldLeft(Option.empty[(PokerHand, Card)]) { (x, acc) =>
          acc.orElse(x)
        }

    handResult match {
      case Some(x) => x
      case None    => (HighCards, hand.last)
    }
  }
}

// TODO: cats.NonEmptyListを使用して空のリストをOptionalに判別する条件分岐を削除する
object Hand {
  implicit def convertHandToListCard(hand: Hand): List[Card] = hand.fromHand

  implicit def toHand(cards: List[Card]): Option[Hand] =
    if (cards.size == 5) Some(Hand(cards.sorted)) else None

  def pokerHand(hand: Hand): (PokerHand, Card) = {
    val hands: List[Hand => Option[(PokerHand, Card)]] =
      List(onePair, twoPair, threeOfAKind, straight, flush, fourOfAKind, fullHouse, straightFlush)

    val handResult: Option[(PokerHand, Card)] =
      Functor[List]
        .map(hands)(_(hand))
        .foldLeft(Option.empty[(PokerHand, Card)]) { (x, acc) =>
          acc.orElse(x)
        }

    handResult match {
      case Some(x) => x
      case None    => (HighCards, hand.last)
    }
  }

  private def extract[A, B](f: B => A)(l: List[B]): List[(A, B)] = l.map { c =>
    (f(c), c)
  }

  private def flushHint(hand: Hand): Option[Card] =
    if (hand.forall(card => card.suit == hand.head.suit)) Some(hand.tail.last)
    else None

  private def nOfKindDiscards(hand: Hand): DiscardList = {
    def allNOfKinds(hand: Hand): List[Card] = {
      val kinds: List[Option[List[List[Card]]]] =
        List(nOfKindHint(2)(hand), nOfKindHint(3)(hand), nOfKindHint(4)(hand))
      kinds.flatten.flatten.flatten
    }

    hand.filter { card =>
      !allNOfKinds(hand).contains(card)
    }
  }

  private def nOfKindHint(n: Int)(hand: Hand): Option[List[List[Card]]] = {
    def groupBy(eq: (Card, Card) => Boolean)(list: List[Card]): List[List[Card]] = {
      list match {
        case head :: tail =>
          val newHead = head :: tail.takeWhile(eq(head, _))
          newHead :: groupBy(eq)(tail.dropWhile(eq(head, _)))
        case _ => List.empty
      }
    }

    val cards = groupBy { (x, y) =>
      x.num == y.num
    }(hand).filter { cards =>
      cards.length == n
    }

    if (cards.isEmpty) None else Some(cards)
  }

  private def straightHint(hand: Hand): Option[Card] = {
    import Card._

    def isStraight(xs: List[Int]): Boolean =
      xs == (xs.head to xs.head + 4).toList

    def judgeStraight(l: List[(Int, Card)]): Option[Card] =
      if (isStraight(l.map { f =>
            f._1
          })) Some(l.last._2)
      else None

    judgeStraight(extract(cardStrength)(hand)) orElse judgeStraight(
      extract(cardNumber)(hand).sortWith { (judge1, judge2) =>
        Order.gt(judge1._2, judge2._2) // Cardの強さで比較する
      })
  }

  def onePair(hand: Hand): Option[(PokerHand, Card)] = {
    for (cs <- nOfKindHint(2)(hand)) yield {
      if (cs.isEmpty) return None else return Some(OnePair, cs.flatten.last)
    }
  }

  def twoPair(hand: Hand): Option[(PokerHand, Card)] = {
    for (cs <- nOfKindHint(2)(hand)) yield {
      cs.length match {
        case 2 => return Some(TwoPair, cs.flatten.last)
        case _ => return None
      }
    }
  }

  def threeOfAKind(hand: Hand): Option[(PokerHand, Card)] = {
    for (cs <- nOfKindHint(3)(hand)) yield {
      if (cs.isEmpty) return None
      else return Some(ThreeOfAKind, cs.flatten.last)
    }
  }

  def straight(hand: Hand): Option[(PokerHand, Card)] = {
    for (c <- straightHint(hand)) yield (Straight, c)
  }

  def flush(hand: Hand): Option[(PokerHand, Card)] = {
    for (c <- flushHint(hand)) yield (Flush, c)
  }

  def fullHouse(hand: Hand): Option[(PokerHand, Card)] = {
    for (cs <- nOfKindHint(3)(hand) if nOfKindHint(2)(hand).isDefined) yield {
      (FullHouse, cs.flatten.last)
    }
  }

  def fourOfAKind(hand: Hand): Option[(PokerHand, Card)] = {
    for (cs <- nOfKindHint(4)(hand)) yield {
      if (cs.isEmpty) return None
      else return Some(FourOfAKind, cs.flatten.max)
    }
  }

  def straightFlush(hand: Hand): Option[(PokerHand, Card)] = {
    for (c <- straightHint(hand);
         d <- flushHint(hand))
      yield (StraightFlush, List(c, d).max)
  }

  def aiSelectDiscards(hand: Hand): DiscardList = {
    straightHint(hand) orElse flushHint(hand) getOrElse None match {
      case None           => nOfKindDiscards(hand)
      case Some(xs: Card) => List(xs)
    }
  }
}
