def readInput(fileName: String): List[String] =
  io.Source.fromResource(s"day5/$fileName.txt").getLines().toList

val OneOrMoreSpace = "\\s+"

val data: List[String] = readInput("day5_1_test").filter(_.nonEmpty)
// val data: List[String] = readInput("day5_1").filter(_.nonEmpty)

case class SourceToDestination(
    sourceStart: Int,
    destinationStart: Int,
    rangeLength: Int
)

case class ConverterMap(
    name: String,
    mappings: List[SourceToDestination]
)

case class Seed(
    start: Int,
    end: Int
)

def extractSeeds(input: List[String]): (List[Int], List[String]) =
  input match
    case head :: tail =>
      head match
        case s"seeds: $seeds" =>
          (seeds.split(OneOrMoreSpace).toList.map(_.toInt), tail)
    case Nil =>
      (List.empty, input)

def extractSourceToDestinationMaps(input: List[String]) = {
  def getMappings(
      list: List[String],
      accumulator: List[SourceToDestination]
  ): (List[String], List[SourceToDestination]) =
    list match
      case head :: tail =>
        head match
          case s"$destination $source $rangeLength" =>
            getMappings(
              tail,
              accumulator :+ SourceToDestination(
                sourceStart = source.toInt,
                destinationStart = destination.toInt,
                rangeLength = rangeLength.toInt
              )
            )
          case s"$_ map:" =>
            (list, accumulator)
      case Nil =>
        (list, accumulator)

  def loop(
      remaining: List[String],
      accumulator: List[ConverterMap]
  ): List[ConverterMap] = {
    remaining match
      case head :: tail =>
        head match
          case s"$name map:" =>
            val (leftovers, mappings) = getMappings(tail, List.empty)
            loop(leftovers, accumulator :+ ConverterMap(name, mappings))
      case Nil =>
        accumulator
  }

  loop(input, List.empty)
}

def findConvertedNumber(
    lastConvertedNumber: Int,
    converterMap: ConverterMap
): Int =
  val foundMappings = converterMap.mappings.filter { mapping =>
    mapping.sourceStart <= lastConvertedNumber && lastConvertedNumber <= mapping.sourceStart + mapping.rangeLength
  }

  if foundMappings.isEmpty then lastConvertedNumber
  else
    val foundMapping = foundMappings.head

    if foundMapping.sourceStart < foundMapping.destinationStart then
      val diff = foundMapping.destinationStart - foundMapping.sourceStart
      lastConvertedNumber + diff
    else
      val diff = foundMapping.sourceStart - foundMapping.destinationStart
      lastConvertedNumber - diff

def findLocation(
    currentSeed: Int,
    maps: List[ConverterMap]
): Int = {
  maps.foldLeft(currentSeed) { (lastConvertedNumber, converterMap) =>
    findConvertedNumber(lastConvertedNumber, converterMap)
  }
}

def findMinimumLocation(
    data: List[String]
): Int =
  val (seeds, leftovers) = extractSeeds(data)
  val converterMaps = extractSourceToDestinationMaps(leftovers)

  seeds
    .foldLeft(List.empty[Int]) { (accumulator, currentSeed) =>
      accumulator :+ findLocation(currentSeed, converterMaps)
    }
    .min

findMinimumLocation(data)
// part 1: 388071289
