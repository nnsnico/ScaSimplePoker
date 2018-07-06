import Suit.{Clubs, Diamonds, Hearts, Spades}
import cats._
import cats.implicits._

sealed trait Suit

object Suit {

  case object Hearts extends Suit

  case object Diamonds extends Suit

  case object Clubs extends Suit

  case object Spades extends Suit

  val values = List(Hearts, Diamonds, Clubs, Spades)
}

case class Card(num: Int, suit: Suit) {
  val cardNum: String = (num, suit) match {
    case (i, Hearts)   => "H" + showCardNumber(i)
    case (i, Diamonds) => "D" + showCardNumber(i)
    case (i, Clubs)    => "C" + showCardNumber(i)
    case (i, Spades)   => "S" + showCardNumber(i)
  }

  private def showCardNumber(num: Int): String = num match {
    case 14     => "A_"
    case 13     => "K_"
    case 12     => "Q_"
    case 11     => "J_"
    case 10     => "10"
    case x: Int => x.toString + "_"
  }
}

object Card {
  implicit val ord: Order[Card] = Order.by((card: Card) => card.num)

  def cardSuit(card: Card): Suit = card.suit

  def cardNumber(card: Card): Int = card.num

  def allCards: List[Card] =
    for (suit <- Suit.values;
         num <- (2 to 14).toList) yield Card(num, suit)

  def allStrCards: List[String] =
    for (suit <- Suit.values;
         num <- (2 to 14).toList) yield Card(num, suit).cardNum

  def cardStrength(card: Card): Int = card.num
}
