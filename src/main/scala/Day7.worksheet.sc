def readInput(fileName: String) =
  io.Source.fromResource(s"day7/$fileName.txt").getLines().toList

val OneOrMoreSpace = "\\s+"

enum Type:
  case FiveOfAKind, FourOfAKind, FullHouse, ThreeOfAKind, TwoPair, OnePair,
    HighCard

/*
// For Part 1
val CardRanking = Map[Char, Int](
  ('A', 13),
  ('K', 12),
  ('Q', 11),
  ('J', 10),
  ('T', 9),
  ('9', 8),
  ('8', 7),
  ('7', 6),
  ('6', 5),
  ('5', 4),
  ('4', 3),
  ('3', 2),
  ('2', 1)
)
 */

// For Part 2
val CardRanking = Map[Char, Int](
  ('A', 13),
  ('K', 12),
  ('Q', 11),
  ('T', 10),
  ('9', 9),
  ('8', 8),
  ('7', 7),
  ('6', 6),
  ('5', 5),
  ('4', 4),
  ('3', 3),
  ('2', 2),
  ('J', 1)
)

def compareCards(first: List[Char], second: List[Char]): Int =
  (first, second) match
    case (head :: tail, h :: t) =>
      val firstRank  = CardRanking.getOrElse(head, 0)
      val secondRank = CardRanking.getOrElse(h, 0)

      // Descending ordering
      if firstRank == secondRank then compareCards(tail, t)
      else secondRank - firstRank
    case (Nil, h :: t)       => 1
    case (head :: tail, Nil) => -1
    case (Nil, Nil)          => 0

case class Hand(cards: String, bid: Int) extends Ordered[Hand]:
  override def compare(that: Hand): Int = compareCards(cards.toList, that.cards.toList)

def processInput(input: List[String]): List[Hand] =
  def loop(remaining: List[String], hands: List[Hand]): List[Hand] =
    remaining match
      case head :: tail =>
        val Array(cards, bid) = head.split(OneOrMoreSpace)
        loop(tail, hands :+ Hand(cards, bid.toInt))
      case Nil =>
        hands

  loop(input, List.empty[Hand])

def buildOccurrenceMap(hand: Hand): Map[Char, Int] =
  hand.cards.foldLeft(Map.empty[Char, Int]) { (acc, curr) =>
    acc.get(curr) match
      case None        => acc.updated(curr, 1)
      case Some(value) => acc.updated(curr, value + 1)
  }

def categorizeHand(hand: Hand, occurrenceMap: Map[Char, Int]): (Type, Hand) =
  val fiveSame    = occurrenceMap.values.filter(_ == 5).toList.length
  val fourSame    = occurrenceMap.values.filter(_ == 4).toList.length
  val threeSame   = occurrenceMap.values.filter(_ == 3).toList.length
  val twoSame     = occurrenceMap.values.filter(_ == 2).toList.length
  val allDistinct = occurrenceMap.values.filter(_ == 1).toList.length

  (fiveSame, fourSame, threeSame, twoSame, allDistinct) match
    case (1, _, _, _, _) =>
      (Type.FiveOfAKind, hand)
    case (_, 1, _, _, _) =>
      (Type.FourOfAKind, hand)
    case (_, _, _, 2, _) =>
      (Type.TwoPair, hand)
    case (_, _, _, 1, 3) =>
      (Type.OnePair, hand)
    case (_, _, _, _, 5) =>
      (Type.HighCard, hand)
    case (_, _, 1, 1, _) =>
      (Type.FullHouse, hand)
    case (_, _, 1, _, 2) =>
      (Type.ThreeOfAKind, hand)

def categorizeHandPart2(hand: Hand, occurrenceMap: Map[Char, Int]): (Type, Hand) =
  if occurrenceMap.contains('J') then
    val newHand = {
      val highestOccurence = occurrenceMap.filter(_._1 != 'J')

      if highestOccurence.nonEmpty then
        hand.copy(hand.cards.replaceAll('J'.toString, highestOccurence.maxBy(_._2)._1.toString), hand.bid)
      else hand.copy(hand.cards.replaceAll('J'.toString, 'A'.toString), hand.bid)
    }
    val newOccurrenceMap = buildOccurrenceMap(newHand)
    val (cardType, _)    = categorizeHand(newHand, newOccurrenceMap)
    (cardType, hand)
  else categorizeHand(hand, occurrenceMap)

def buildTypeMap(
  hands: List[Hand],
  categorizeFunc: (hand: Hand, occMap: Map[Char, Int]) => (Type, Hand)
): Map[Type, List[Hand]] =
  val tmpMap = Type.values.foldLeft(Map.empty[Type, List[Hand]])((acc, curr) => acc + (curr -> List.empty[Hand]))

  val res = hands.foldLeft(tmpMap) { (acc, currHand) =>
    val occurrenceMap   = buildOccurrenceMap(currHand)
    val categorizedHand = categorizeFunc(currHand, occurrenceMap)

    acc.get(categorizedHand._1) match
      case None                  => acc.updated(categorizedHand._1, List(categorizedHand._2))
      case Some(categorizedList) => acc.updated(categorizedHand._1, categorizedList :+ categorizedHand._2)
  }

  Type.values.foldLeft(res) { (acc, curr) =>
    acc.updated(curr, acc.getOrElse(curr, List.empty[Hand]).sorted)
  }

def getDescendingOrderedHands(typeMap: Map[Type, List[Hand]]): List[Hand] =
  Type.values.foldLeft(List.empty[Hand]) { (acc, curr) =>
    acc ++ typeMap.getOrElse(curr, List.empty[Hand])
  }

def calculateTotalWinnings(orderedHands: List[Hand]): Int =
  ((orderedHands.length to 1 by -1) zip orderedHands).foldLeft(0) { (acc, curr) =>
    acc + (curr._1 * curr._2.bid)
  }

// val data = readInput("day7_1_test")
val data      = readInput("day7_1")
val processed = processInput(data)

/*
val d       = buildTypeMap(processed, categorizeHand)
val ordered = getDescendingOrderedHands(d)
calculateTotalWinnings(ordered)
 */
// Part 1 solution: 248396258

val d       = buildTypeMap(processed, categorizeHandPart2)
val ordered = getDescendingOrderedHands(d)
calculateTotalWinnings(ordered)
// Part 2 solution: 246436046
