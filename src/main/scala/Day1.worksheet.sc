def readInput(fileName: String) =
  io.Source.fromResource(s"day1/$fileName.txt").getLines().toList

def getSumOfAllCalibrations(input: List[String]) =
  input.map { line =>
    val extractedDigits = line.filter(_.isDigit)
    val firstDigit = extractedDigits.headOption.getOrElse("")
    val lastDigit = extractedDigits.lastOption.getOrElse("")

    s"${firstDigit}${lastDigit}".toInt
  }.sum

val WrittenToRealNumbersMap = Map(
  "one" -> "1",
  "two" -> "2",
  "three" -> "3",
  "four" -> "4",
  "five" -> "5",
  "six" -> "6",
  "seven" -> "7",
  "eight" -> "8",
  "nine" -> "9"
)

val MaxLength = WrittenToRealNumbersMap.keys.map(_.length).max

def processWrittenNumbers(input: List[String]) =
  input
    .map { str =>
      str
        .sliding(MaxLength)
        .toList
        .map { subString =>
          val num =
            WrittenToRealNumbersMap.keys
              .filter(subString.contains)
              .headOption
              .getOrElse("")
          subString.replaceAll(num, WrittenToRealNumbersMap.getOrElse(num, ""))
        }
        .mkString
    }

val inputTest1 = readInput("day1_1_test")
getSumOfAllCalibrations(input = inputTest1)

val inputReal1 = readInput("day1_1")
getSumOfAllCalibrations(input = inputReal1)

val inputTest2 = readInput("day1_2_test")
val processed = processWrittenNumbers(input = inputTest2)
getSumOfAllCalibrations(processed)

val inputReal2 = readInput("day1_2")
val processed2 = processWrittenNumbers(input = inputReal2)
getSumOfAllCalibrations(processed2)
