import java.util.Scanner

import cats._
import cats.implicits._
import cats.data.NonEmptyList

import scala.util.Try

import Players._
import Card._

object Main extends App {

  type DiscardList = List[Card]
  type Deck        = List[Card]

  def getHand(deck: Deck): Option[(Hand, Deck)] =
    for (hand <- Hand.toHand(deck.take(5))) yield (hand, deck.drop(5))

  def getDiscardList(hand: Hand): Option[DiscardList] = {
    def selectByIndexes[A](any: List[A], inputs: List[Int]): Option[List[A]] = {
      val discardList: Option[NonEmptyList[A]] = inputs.flatMap { i =>
        any.get(i - 1)
      }.toNel

      for (res <- discardList) yield res.toList
    }

    // 入力された整数値を分割してListにぶちこむ(1〜5)
    // 6以降、あるいは他のInt型以外のものを入力したときはやり直す
    val scanner: Scanner = new Scanner(System.in)
    val input: Option[List[Int]] =
      Try(Some(scanner.next.split("").map(_.toInt).toList))
        .getOrElse(None)

    for {
      intList <- input
      res     <- selectByIndexes(hand, intList)
    } yield res
  }

  def drawHand(deck: Deck, discardList: DiscardList, hand: Hand): Option[(Hand, Deck)] = {
    val nl = hand.filter { x =>
      !discardList.contains(x)
    }
    val nr = deck.drop(5 - nl.length)
    for (hand <- Hand.toHand((nl ++ deck).take(5))) yield {
      (hand, nr)
    }
  }

  def showChangeHand(discards: DiscardList, hand: Hand): String = {
    def judge(x: Card): String =
      if (discards.contains(x))
        s""" ${x.cardNum} """
      else
        s"""[${x.cardNum}]"""

    hand.flatMap { discard =>
      judge(discard)
    }.mkString
  }

  def printHand(discards: DiscardList, hand: Hand, player: Players): Unit =
    println(s"""-- ${player.showName}の手札 : ${showChangeHand(discards, hand)}""")

  // left < right => -1, left > right => 1, left == right => 0
  def judgeVictory(myRes: (PokerHand, Card), enemyRes: (PokerHand, Card)): Int =
    Order.compare(myRes, enemyRes)

  def printResult(
      mHand: Hand,
      eHand: Hand,
      mRes: (PokerHand, Card),
      eRes: (PokerHand, Card)
  ): Unit = {
    println(" ***** 結果発表!! ***** ")
    printHand(List.empty, mHand, Player)
    printHand(List.empty, eHand, Enemy)

    println(s"""***** ${Player.showName}の手札は${mRes._1}で、最強カードは、${mRes._2.cardNum}でした *****""")
    println(s"""***** ${Enemy.showName}の手札は${eRes._1}で、最強カードは、${eRes._2.cardNum}でした *****""")

    judgeVictory(mRes, eRes) match {
      case 0  => println("引き分けです")
      case 1  => println(s"${Player.showName}の勝ちです")
      case -1 => println(s"${Enemy.showName}の負けです")
    }
  }

  def ynQuestion[A](str: String, yes: => A, no: => A): A = {
    print(s"""$str (y/n) """)
    val scanner = new Scanner(System.in)
    val input   = scanner.nextLine

    input match {
      case "y" => yes
      case "n" => no
      case _ =>
        println("-- `y`か`n`で入力してね")
        ynQuestion(str, yes, no)
    }
  }

  def matchPoker(handDeck: (Hand, Deck)): Unit = {
    import Players._

    val (mRes, nDeck, mnHand) = playPoker(handDeck._1, handDeck._2, Player)
    getHand(nDeck) match {
      case None => new Exception("予期せぬエラー")
      case Some((eHand, eDeck)) =>
        val (eRes, _, neHand) = playPoker(eHand, eDeck, Enemy)
        printResult(mnHand, neHand, mRes, eRes)
    }
  }

  def playPoker(hand: Hand, deck: Deck, player: Players): ((PokerHand, Card), Deck, Hand) = {

    def inputDisuse(hand: Hand): DiscardList = {
      printHand(List.empty, hand, Player) // 最初に捨てるカードはないのでemptyを渡す
      println("-- 捨てるカードを選んでね")

      val gotDisuse = getDiscardList(hand)
      gotDisuse match {
        case None =>
          println("-- 1~5の数値を並べて入力してね")
          inputDisuse(hand)
        case Some(disuses) =>
          printHand(disuses, hand, Player)
          ynQuestion(s"-- ${Player.showName} : これでいい？", disuses, inputDisuse(hand))
      }
    }

    def aiDisuse(hand: Hand): DiscardList = {
      val res = Hand.aiSelectDiscards(hand)
      printHand(res, hand, Enemy)
      println(s"-- ${Enemy.showName}:これでいいよ!")
      res
    }

    val discards = if (player == Player) inputDisuse(hand) else aiDisuse(hand)
    drawHand(deck, discards, hand) match {
      case Some((nHand, nDeck)) =>
        val res = Hand.pokerHand(nHand)
        (res, nDeck, nHand)
      case None => throw new Exception("予期せぬエラー")
    }
  }

  // main
  println("------------------")
  println("-- simple poker --")
  println("------------------")

  import scala.util.Random.shuffle

  val deck: Deck = shuffle(Card.allCards)
  getHand(deck) match {
    case None      => new Exception("予期せぬエラー: getHand in simpleGame")
    case Some(res) => matchPoker(res)
  }

  // いまここで実行されるのはらめえ
  lazy val main: Unit = Main.main(Array.empty)
  lazy val bye: Unit  = println("-- またねノシノシ")

  print("-- もっかいやる？ (y/n) ")
  val input: String = new Scanner(System.in).nextLine

  input match {
    case "y" => main
    case "n" => bye
    case _ =>
      println("-- `y`か`n`で入力してね")
      ynQuestion("-- もっかいやる？", main, bye)
  }
}
