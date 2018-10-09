import cats._
import cats.implicits._
import cats.data.{NonEmptyList, OptionT}
import Hand._
import Main.DiscardList
import PokerHand._

import scala.language.implicitConversions

sealed abstract class PokerHand(strength: Int) {
  val toStrength: Int = strength
}

object PokerHand {

  implicit val ord: Order[PokerHand] = Order.by((p: PokerHand) => p.toStrength)

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

object Hand {
  implicit def convertHandToListCard(hand: Hand): List[Card] = hand.fromHand

  implicit def toHand(cards: List[Card]): Option[Hand] =
    if (cards.size == 5) Hand(cards.sorted).some else none

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

  private def extract[A, B](f: B => A)(l: List[B]): List[(A, B)] =
    l.map { c =>
      (f(c), c)
    }

  private def flushHint(hand: Hand): Option[Card] =
    if (hand.forall(card => card.suit == hand.head.suit))
      hand.tail.last.some
    else
      none

  private def nOfKindDiscards(hand: Hand): DiscardList = {
    def allNOfKinds(h: Hand): List[Card] = {
      val kinds: List[Option[NonEmptyList[List[Card]]]] =
        List(nOfKindHint(2)(h), nOfKindHint(3)(h), nOfKindHint(4)(h))

      (for {
        cards <- OptionT(kinds).map(_.toList.flatten) // OptionT[List, List[Card]]
        card  <- OptionT.liftF(cards)                 // OptionT[List, Card]
      } yield card).value.flatten // List[Option[Card]] → List[Card]
    }

    hand.filterNot(allNOfKinds(hand).contains)
  }

  private def nOfKindHint(n: Int)(hand: Hand): Option[NonEmptyList[List[Card]]] = {
    def groupBy(eq: (Card, Card) => Boolean)(list: List[Card]): List[List[Card]] =
      list match {
        case head :: tail =>
          val newHead = head :: tail.takeWhile(eq(head, _))
          newHead :: groupBy(eq)(tail.dropWhile(eq(head, _)))
        case _ => List.empty
      }

    val cards: List[List[Card]] = groupBy { (x, y) =>
      x.num == y.num
    }(hand).filter { cards =>
      cards.length == n
    }

    cards.toNel
  }

  private def straightHint(hand: Hand): Option[Card] = {
    import Card._

    def isStraight(xs: List[Int]): Boolean =
      xs == (xs.head to xs.head + 4).toList

    def judgeStraight(l: List[(Int, Card)]): Option[Card] =
      if (isStraight(l.map { f =>
            f._1
          }))
        l.last._2.some
      else
        none

    judgeStraight(extract(cardStrength)(hand))
      .orElse(judgeStraight(extract(cardNumber)(hand).sortWith { (judge1, judge2) =>
        Order.gt(judge1._2, judge2._2) // Cardの強さで比較する
      }))
  }

  def onePair(hand: Hand): Option[(PokerHand, Card)] =
    for (cs <- nOfKindHint(2)(hand)) yield (OnePair, cs.toList.flatten.last)

  def twoPair(hand: Hand): Option[(PokerHand, Card)] =
    for (cs <- nOfKindHint(2)(hand)) yield {
      cs.length match {
        case 2 => return (TwoPair, cs.toList.flatten.last).some
        case _ => return none
      }
    }

  def threeOfAKind(hand: Hand): Option[(PokerHand, Card)] =
    for (cs <- nOfKindHint(3)(hand)) yield (ThreeOfAKind, cs.toList.flatten.last)

  def straight(hand: Hand): Option[(PokerHand, Card)] =
    for (c <- straightHint(hand)) yield (Straight, c)

  def flush(hand: Hand): Option[(PokerHand, Card)] =
    for (c <- flushHint(hand)) yield (Flush, c)

  def fullHouse(hand: Hand): Option[(PokerHand, Card)] =
    for (cs <- nOfKindHint(3)(hand) if nOfKindHint(2)(hand).isDefined) yield {
      (FullHouse, cs.toList.flatten.last)
    }

  def fourOfAKind(hand: Hand): Option[(PokerHand, Card)] =
    for (cs <- nOfKindHint(4)(hand)) yield (FourOfAKind, cs.toList.flatten.max)

  def straightFlush(hand: Hand): Option[(PokerHand, Card)] =
    for {
      c <- straightHint(hand)
      d <- flushHint(hand)
    } yield (StraightFlush, List(c, d).max)

  def aiSelectDiscards(hand: Hand): DiscardList =
    straightHint(hand).orElse(flushHint(hand)).getOrElse(None) match {
      case None           => nOfKindDiscards(hand)
      case Some(xs: Card) => List(xs)
    }
}
