package poker

import scala.collection.mutable.HashMap

object Suit extends Enumeration {
  type Suit = Value

  val Heart = Value("Heart")
  val Spade = Value("Spade")
  val Club = Value("Club")
  val Diamond = Value("Diamond")
}
final case class Card(value: Int, suit: Suit.Suit)

object Poker {

  type Hand = List[Card]

  object CompareResult extends Enumeration {
    type CompareResult = Value
    val Win, Lost, Draw = Value

    def isFinal(value: CompareResult): Boolean = {
      return value != CompareResult.Draw
    }
  }

  case class HandPower(rank: HandRank.HandRank, highs: List[Int])

  object HandRank extends Enumeration {
    type HandRank = Value

    val StraightFlush = Value(0)
    val FourCard = Value(1)
    val Fullhouse = Value(2)
    val Flush = Value(3)
    val Straight = Value(4)
    val ThreeOfKind = Value(5)
    val TwoPairs = Value(6)
    val OnePair = Value(7)
    val Nothing = Value(8)
  }

  private def compareHighs(
      high1: List[Int],
      high2: List[Int]
  ): CompareResult.CompareResult = {
    high1
      .zip(high2)
      .foldLeft(CompareResult.Draw)((result, pair) => {
        if (result != CompareResult.Draw) {
          return result
        } else if (pair._1 > pair._2) {
          return CompareResult.Win
        } else if (pair._1 < pair._2) {
          return CompareResult.Lost
        } else {
          return CompareResult.Draw
        }
      })
  }

  private def isConsequtive(highs: List[Int]): Boolean = {
    for (i <- 0 until highs.length - 1) {
      if (highs(i) != highs(i + 1) + 1) {
        return false
      }
    }
    return true
  }

  def compareRank(
      handRank1: HandRank.HandRank,
      handRank2: HandRank.HandRank
  ): CompareResult.CompareResult = {
    if (handRank1.id < handRank2.id) return CompareResult.Win
    if (handRank1.id > handRank2.id) return CompareResult.Lost
    CompareResult.Draw
  }

  def comparePokerHand(
      hand1: Hand,
      hand2: Hand
  ): CompareResult.CompareResult = {
    val handPower1 = handPower(hand1)
    val handPower2 = handPower(hand2)
    compareHandPower(handPower1, handPower2)
  }

  def compareHandPower(
      handPower1: HandPower,
      handPower2: HandPower
  ): CompareResult.CompareResult = {
    val rankComparison = compareRank(handPower1.rank, handPower2.rank)
    if (rankComparison == CompareResult.Draw) {
      compareHighs(
        handPower1.highs,
        handPower2.highs
      )
    } else {
      rankComparison
    }
  }

  def handPower(hand: Hand): HandPower = {
    val freqMap =
      hand.foldLeft(Map[Int, Int]())((acc, card: Card) => {
        acc.get(card.value) match {
          case None              => acc + (card.value -> 1)
          case Some(currentFreq) => acc + (card.value -> (currentFreq + 1))
        }
      })
    val pairPattern = freqMap.values.toList.sorted

    val highs = freqMap.toSeq
      .sortWith((pair1, pair2) => {
        val cardVal1 = pair1._1
        val cardVal2 = pair2._1
        val cardFreq1 = pair1._2
        val cardFreq2 = pair2._2
        if (cardFreq1 == cardFreq2) {
          cardVal1 > cardVal2
        } else {
          cardFreq1 > cardFreq2
        }
      })
      .map(_._1)
      .toList
    val highsAceFirst =
      highs.map(i => if (i == 14) 1 else i).sorted(Ordering[Int].reverse)

    val isFlush = hand.map(card => card.suit).distinct.length == 1

    pairPattern match {
      case 1 :: 4 :: Nil      => return HandPower(HandRank.FourCard, highs)
      case 2 :: 3 :: Nil      => return HandPower(HandRank.Fullhouse, highs)
      case 1 :: 1 :: 3 :: Nil => return HandPower(HandRank.ThreeOfKind, highs)
      case 1 :: 2 :: 2 :: Nil => return HandPower(HandRank.TwoPairs, highs)
      case 1 :: 1 :: 1 :: 2 :: Nil =>
        return HandPower(HandRank.OnePair, highs)
      case _ => {
        if (isConsequtive(highs) && isFlush) {
          HandPower(HandRank.StraightFlush, highs)
        } else if (isConsequtive(highsAceFirst) && isFlush) {
          HandPower(HandRank.StraightFlush, highsAceFirst)
        } else if (isFlush) {
          HandPower(HandRank.Flush, highs)
        } else if (isConsequtive(highs)) {
          HandPower(HandRank.Straight, highs)
        } else if (isConsequtive(highsAceFirst)) {
          HandPower(HandRank.Straight, highsAceFirst)
        } else {
          HandPower(HandRank.Nothing, highs)
        }
      }
    }
  }
}
