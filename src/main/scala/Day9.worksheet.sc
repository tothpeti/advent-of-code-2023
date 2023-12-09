val OneOrMoreSpace = "\\s+"

def readInput(fileName: String): List[String] =
  io.Source.fromResource(s"day9/$fileName.txt").getLines().toList

def processInput(input: List[String]): List[List[Long]] =
  input.map(_.split(OneOrMoreSpace).toList.map(_.toLong))

def calculateExtrapolatedValue(input: List[Long]): Long =
  def loop(list: List[Long], accumulator: List[Long]): List[Long] =
    if list.forall(_ == 0) then accumulator
    else
      val newList = list.sliding(2).map(pair => pair.last - pair.head).toList
      loop(newList, newList.last +: accumulator)

  val differences = loop(input, List.empty[Long])
  input.last + differences.sum

def sumOfExtrapolatedValues(input: List[List[Long]]): Long =
  input.map(row => calculateExtrapolatedValue(row)).sum

def sumOfExtrapolatedValuesBackwards(input: List[List[Long]]): Long =
  input.map(row => calculateExtrapolatedValue(row.reverse)).sum

// val data = readInput("day9_1_test")
val data = readInput("day9_1")

val processed = processInput(data)
// sumOfExtrapolatedValues(processed)
// Part 1 solution: 1930746032

sumOfExtrapolatedValuesBackwards(processed)
// Part 2 solution: 1154
