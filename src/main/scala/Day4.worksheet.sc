def readInput(fileName: String): List[String] =
  io.Source.fromResource(s"day4/$fileName.txt").getLines().toList

// val data: List[String] = readInput("day4_1_test")
val data: List[String] = readInput("day4_1")

case class Card(id: Int, winningNumbers: Set[Int], receivedNumbers: Set[Int])

val OneOrMoreSpace = "\\s+"

def extractNumbers(input: String): Set[Int] =
  input
    .split(OneOrMoreSpace)
    .toList
    .filterNot(_.isEmpty)
    .toSet
    .map(_.toInt)

def processInput(input: List[String]): List[Card] =
  input.map { case s"Card $id: $numbers" =>
    numbers match
      case s"$winningNumbers | $receivedNumbers" =>
        val winningNums  = extractNumbers(winningNumbers)
        val receivedNums = extractNumbers(receivedNumbers)
        val extractedId  = id.replaceAll(OneOrMoreSpace, "").toInt

        Card(extractedId, winningNums, receivedNums)
  }

def calculateWinningPoints(input: Set[Int]): Int =
  def loop(data: List[Int], counter: Int = 0, accumulator: Int = 0): Int =
    if counter == 0 && data.nonEmpty then loop(data.tail, counter + 1, accumulator + 1)
    else if data.isEmpty then accumulator
    else
      data match
        case _ :: tail =>
          loop(tail, counter + 1, accumulator * 2)
        case Nil =>
          loop(Nil, counter, accumulator)

  loop(input.toList)

def calculateSumOfWinningPoints(cards: List[Card]): Int =
  cards.map { card =>
    calculateWinningPoints(card.winningNumbers intersect card.receivedNumbers)
  }.sum

def initializeCardOccurenceMap(): Map[Int, Int] =
  (1 to data.length)
    .foldLeft(Map.empty[Int, Int]) { (acc, curr) =>
      acc + (curr -> 1)
    }

def calculateTotalScratchCards(input: List[Card]) =
  input
    .foldLeft(initializeCardOccurenceMap()) { (acc, curr) =>
      val cardOccurences = acc.getOrElse(curr.id, 0)
      val matchingNumbers =
        (curr.winningNumbers intersect curr.receivedNumbers)

      if matchingNumbers.nonEmpty then
        (curr.id + 1 to matchingNumbers.size + curr.id).foldLeft(acc) { (accum, currIdx) =>
          accum.get(currIdx) match
            case Some(value) => accum.updated(currIdx, value + cardOccurences)
            case None        => accum
        }
      else acc
    }
    .values
    .sum

val processedInput = processInput(data)
calculateSumOfWinningPoints(processedInput)
// Solution for part 1: 18653

calculateTotalScratchCards(processedInput)
// Solution for part 2: 5921508
