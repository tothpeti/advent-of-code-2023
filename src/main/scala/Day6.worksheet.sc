def readInput(fileName: String) =
  io.Source.fromResource(s"day6/$fileName.txt").getLines().toList

// val data: List[String] = readInput("day6_1_test")
val data: List[String] = readInput("day6_1")

val OneOrMoreDigits = "\\D+"
val OneOrMoreSpace = "\\s+"

case class Race(time: BigInt, distance: BigInt)

def processInput(input: List[String]) =
  def loop(
      remainingTimes: List[String],
      remainingRecords: List[String],
      races: List[Race]
  ): List[Race] = {
    (remainingTimes, remainingRecords) match
      case (head :: tail, h :: t) =>
        loop(tail, t, races :+ Race(BigInt(head), BigInt(h)))
      case (head :: tail, Nil) => races
      case (Nil, h :: t)       => races
      case (Nil, Nil)          => races
  }

  val splitted = data
    .map(
      _.split(OneOrMoreDigits)
        .filter(_.nonEmpty)
        .toList
    )

  loop(splitted(0), splitted(1), List.empty[Race])

def processInputPart2(input: List[String]) =
  val splitted = data
    .map(
      _.split(OneOrMoreDigits)
        .filter(_.nonEmpty)
        .mkString
    )

  Race(BigInt(splitted(0)), BigInt(splitted(1)))

def getMinimumTimeToBeatRecord(race: Race): BigInt =
  def loop(
      remainingTime: BigInt,
      minButtonHoldTime: BigInt,
      recordToBeat: BigInt
  ): BigInt = {
    if remainingTime == 0 then minButtonHoldTime
    else if remainingTime == race.time then
      loop(remainingTime - 1, minButtonHoldTime + 1, recordToBeat)
    else
      val distance = remainingTime * minButtonHoldTime

      if distance > recordToBeat then minButtonHoldTime
      else loop(remainingTime - 1, minButtonHoldTime + 1, recordToBeat)
  }

  loop(race.time, 0, race.distance)

def getProductOfNumberOfPossibleWays(input: List[Race]) =
  input.map { r =>
    val minTime = getMinimumTimeToBeatRecord(r)
    val maxTime = r.time - minTime

    (minTime to maxTime).length
  }.product

val processedData = processInput(data)
getProductOfNumberOfPossibleWays(processedData)
// Part 1 solution: 449820

val processedDataPart2 = processInputPart2(data)
getProductOfNumberOfPossibleWays(List(processedDataPart2))
// Part 2 solution: 42250895
