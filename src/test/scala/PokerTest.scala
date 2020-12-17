import poker._

object Fixtures {
  val straightFlush = List(
    Card(13, Suit.Club),
    Card(12, Suit.Club),
    Card(10, Suit.Club),
    Card(11, Suit.Club),
    Card(14, Suit.Club)
  )

  val straightFlushAceFirst = List(
    Card(2, Suit.Club),
    Card(3, Suit.Club),
    Card(4, Suit.Club),
    Card(5, Suit.Club),
    Card(14, Suit.Club)
  )

  val fourCardTenHand = List(
    Card(10, Suit.Club),
    Card(10, Suit.Diamond),
    Card(10, Suit.Heart),
    Card(10, Suit.Spade),
    Card(14, Suit.Club)
  )
  val fourCardAceHand = List(
    Card(14, Suit.Club),
    Card(14, Suit.Diamond),
    Card(14, Suit.Heart),
    Card(14, Suit.Spade),
    Card(10, Suit.Club)
  )
  val flushHand = List(
    Card(13, Suit.Club),
    Card(12, Suit.Club),
    Card(2, Suit.Club),
    Card(3, Suit.Club),
    Card(14, Suit.Club)
  )
  val fullHouseHand = List(
    Card(10, Suit.Club),
    Card(10, Suit.Diamond),
    Card(10, Suit.Heart),
    Card(14, Suit.Spade),
    Card(14, Suit.Club)
  )
  val straightHand = List(
    Card(10, Suit.Club),
    Card(11, Suit.Diamond),
    Card(12, Suit.Heart),
    Card(13, Suit.Spade),
    Card(14, Suit.Club)
  )

  val straightHandAceFirst = List(
    Card(2, Suit.Club),
    Card(3, Suit.Diamond),
    Card(4, Suit.Heart),
    Card(5, Suit.Spade),
    Card(14, Suit.Club)
  )

  val threeOfKind = List(
    Card(2, Suit.Club),
    Card(2, Suit.Diamond),
    Card(2, Suit.Heart),
    Card(3, Suit.Spade),
    Card(14, Suit.Club)
  )

  val twoPairs = List(
    Card(2, Suit.Club),
    Card(2, Suit.Diamond),
    Card(14, Suit.Heart),
    Card(3, Suit.Spade),
    Card(14, Suit.Club)
  )

  val onePair = List(
    Card(2, Suit.Club),
    Card(2, Suit.Diamond),
    Card(5, Suit.Heart),
    Card(3, Suit.Spade),
    Card(14, Suit.Club)
  )

  val highs = List(
    Card(14, Suit.Club),
    Card(8, Suit.Diamond),
    Card(5, Suit.Heart),
    Card(3, Suit.Spade),
    Card(2, Suit.Club)
  )
}

class PokerTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Fourcard win Flush") {
    assert(
      Poker.CompareResult.Win == Poker.comparePokerHand(
        Fixtures.fourCardTenHand,
        Fixtures.flushHand
      )
    )
  }

  test("Straight lose FourCard") {
    assert(
      Poker.CompareResult.Lost == Poker.comparePokerHand(
        Fixtures.straightHand,
        Fixtures.fourCardAceHand
      )
    )
  }

  test("Straight ace first lose normal straight FourCard") {
    assert(
      Poker.CompareResult.Lost == Poker.comparePokerHand(
        Fixtures.straightHandAceFirst,
        Fixtures.straightHand
      )
    )
  }

  test("Fourcard win Fourcard higher") {
    assert(
      Poker.CompareResult.Win == Poker.comparePokerHand(
        Fixtures.fourCardAceHand,
        Fixtures.fourCardTenHand
      )
    )
  }

  test("Rank comparison") {
    assert(
      Poker.CompareResult.Win == Poker.compareRank(
        Poker.HandRank.FourCard,
        Poker.HandRank.Fullhouse
      )
    )

    assert(
      Poker.CompareResult.Draw == Poker.compareRank(
        Poker.HandRank.FourCard,
        Poker.HandRank.FourCard
      )
    )

    assert(
      Poker.CompareResult.Lost == Poker.compareRank(
        Poker.HandRank.Straight,
        Poker.HandRank.Flush
      )
    )
  }
}

class HandPowerTest extends org.scalatest.funsuite.AnyFunSuite {

  test("Convert straight flush rank correctly") {
    val actual = Poker.handPower(Fixtures.straightFlush)
    assert(Poker.HandRank.StraightFlush == actual.rank)
    assert(List(14, 13, 12, 11, 10) == actual.highs)
  }

  test("Convert straight flush rank (ace first) correctly") {
    val actual = Poker.handPower(Fixtures.straightFlushAceFirst)
    assert(Poker.HandRank.StraightFlush == actual.rank)
    assert(List(5, 4, 3, 2, 1) == actual.highs)
  }

  test("Convert four card rank correctly") {
    val actual = Poker.handPower(Fixtures.fourCardTenHand)
    assert(Poker.HandRank.FourCard == actual.rank)
    assert(List(10, 14) == actual.highs)
  }

  test("Convert flush card rank correctly") {
    val actual = Poker.handPower(Fixtures.flushHand)
    assert(Poker.HandRank.Flush == actual.rank)
    assert(List(14, 13, 12, 3, 2) == actual.highs)
  }

  test("Convert fullhouse hand rank correctly") {
    val actual = Poker.handPower(Fixtures.fullHouseHand)
    assert(Poker.HandRank.Fullhouse == actual.rank)
    assert(List(10, 14) == actual.highs)
  }

  test("Convert straight card rank correctly") {
    val actual = Poker.handPower(Fixtures.straightHand)
    assert(Poker.HandRank.Straight == actual.rank)
    assert(List(14, 13, 12, 11, 10) == actual.highs)
  }

  test("Convert straight card start ace rank correctly") {
    val actual = Poker.handPower(Fixtures.straightHandAceFirst)
    assert(Poker.HandRank.Straight == actual.rank)
    assert(List(5, 4, 3, 2, 1) == actual.highs)
  }

  test("Convert three of a kind rank correctly") {
    val actual = Poker.handPower(Fixtures.threeOfKind)
    assert(Poker.HandRank.ThreeOfKind == actual.rank)
    assert(List(2, 14, 3) == actual.highs)
  }

  test("Convert two pair rank correctly") {
    val actual = Poker.handPower(Fixtures.twoPairs)
    assert(Poker.HandRank.TwoPairs == actual.rank)
    assert(List(14, 2, 3) == actual.highs)
  }

  test("Convert one pair rank") {
    val actual = Poker.handPower(Fixtures.onePair)
    assert(Poker.HandRank.OnePair == actual.rank)
    assert(List(2, 14, 5, 3) == actual.highs)
  }

  test("Convert high card rank") {
    val actual = Poker.handPower(Fixtures.highs)
    assert(Poker.HandRank.Nothing == actual.rank)
    assert(List(14, 8, 5, 3, 2) == actual.highs)
  }
}
