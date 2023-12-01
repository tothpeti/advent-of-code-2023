import scala.io.Source

def readInput(fileName: String) =
  Source.fromResource(s"day1/$fileName.txt").getLines().toList

def getSumOfAllCalibrations(input: List[String]) =
  input.map { line =>
    val extractedDigits = line.filter(_.isDigit)
    val firstDigit = extractedDigits.headOption.getOrElse("")
    val lastDigit = extractedDigits.lastOption.getOrElse("")

    s"${firstDigit}${lastDigit}".toInt
  }.sum

val WrittenNumbers =
  Set("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

val MaxLength = WrittenNumbers.map(_.length).max

def mapWrittenNumberToRealNumber(input: String): String =
  input match
    case "one"   => "1"
    case "two"   => "2"
    case "three" => "3"
    case "four"  => "4"
    case "five"  => "5"
    case "six"   => "6"
    case "seven" => "7"
    case "eight" => "8"
    case "nine"  => "9"
    case _       => ""

def processWrittenNumbers(input: List[String]) =
  input
    .map { str =>
      str
        .sliding(MaxLength)
        .toList
        .map { subString =>
          val num =
            WrittenNumbers.filter(subString.contains).headOption.getOrElse("")
          subString.replaceAll(num, mapWrittenNumberToRealNumber(num))
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
