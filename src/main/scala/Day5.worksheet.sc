import scala.collection.mutable.ListBuffer
def readInput(fileName: String): List[String] =
  io.Source.fromResource(s"day5/$fileName.txt").getLines().toList

val OneOrMoreSpace = "\\s+"

val data: List[String] = readInput("day5_1_test").filter(_.nonEmpty)
// val data: List[String] = readInput("day5_1").filter(_.nonEmpty)

case class SourceToDestination(
  sourceStart: BigInt,
  destinationStart: BigInt,
  rangeLength: BigInt
)

case class ConverterMap(
  name: String,
  mappings: List[SourceToDestination]
)

case class Seed(
  start: BigInt,
  end: BigInt
)

def extractSeeds(input: List[String]): (List[BigInt], List[String]) =
  input match
    case head :: tail =>
      head match
        case s"seeds: $seeds" =>
          (seeds.split(OneOrMoreSpace).toList.map(BigInt(_)), tail)
    case Nil =>
      (List.empty, input)

def extractRangedSeeds(input: List[String]): (ListBuffer[Seed], List[String]) =
  input match
    case head :: tail =>
      head match
        case s"seeds: $seeds" =>
          val extractedSeeds =
            seeds
              .split(OneOrMoreSpace)
              .grouped(2)
              .map(pair =>
                Seed(
                  start = BigInt(pair(0)),
                  end = BigInt(pair(0)) + BigInt(pair(1))
                )
              )

          (ListBuffer.from(extractedSeeds), tail)
    case Nil =>
      (ListBuffer.empty[Seed], input)

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
                sourceStart = BigInt(source),
                destinationStart = BigInt(destination),
                rangeLength = BigInt(rangeLength)
              )
            )
          case s"$_ map:" =>
            (list, accumulator)
      case Nil =>
        (list, accumulator)

  def loop(
    remaining: List[String],
    accumulator: List[ConverterMap]
  ): List[ConverterMap] =
    remaining match
      case head :: tail =>
        head match
          case s"$name map:" =>
            val (leftovers, mappings) = getMappings(tail, List.empty)
            loop(leftovers, accumulator :+ ConverterMap(name, mappings))
      case Nil =>
        accumulator

  loop(input, List.empty)
}

def findConvertedNumber(
  lastConvertedNumber: BigInt,
  converterMap: ConverterMap
): BigInt =
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
  currentSeed: BigInt,
  maps: List[ConverterMap]
): BigInt =
  maps.foldLeft(currentSeed) { (lastConvertedNumber, converterMap) =>
    findConvertedNumber(lastConvertedNumber, converterMap)
  }

def findMinimumLocation(
  data: List[String]
): BigInt =
  val (seeds, leftovers) = extractSeeds(data)
  val converterMaps      = extractSourceToDestinationMaps(leftovers)

  seeds
    .foldLeft(List.empty[BigInt]) { (accumulator, currentSeed) =>
      accumulator :+ findLocation(currentSeed, converterMaps)
    }
    .min

def findLocationPart2(
  seeds: ListBuffer[Seed],
  mappings: List[SourceToDestination]
): ListBuffer[Seed] =
  var tmpSeeds = seeds
  var answers  = ListBuffer.empty[Seed]

  for mapping <- mappings do
    val sourceEnd = mapping.sourceStart + mapping.rangeLength
    var tmp       = ListBuffer.empty[Seed]

    while tmpSeeds.nonEmpty do
      /*
      [popped.start                                     popped.end]
                      [mapping.source     sourceEnd]
      [BEFORE        ][INTER                       ][AFTER        ]
       */
      val popped = tmpSeeds.remove(0)
      val before = Seed(popped.start, popped.end.min(mapping.sourceStart))
      val inter = Seed(
        popped.start.max(mapping.sourceStart),
        sourceEnd.min(popped.end)
      )
      val after = Seed(sourceEnd.max(popped.start), popped.end)

      if before.start < before.end then tmp.append(before)

      if inter.start < inter.end then
        answers.append(
          Seed(
            inter.start - mapping.sourceStart + mapping.destinationStart,
            inter.end - mapping.sourceStart + mapping.destinationStart
          )
        )

      if after.start < after.end then tmp.append(after)

    tmpSeeds = tmp

  answers ++ tmpSeeds

def findMinimumLocationPart2(data: List[String]): BigInt =
  var (seeds, leftovers) = extractRangedSeeds(data)
  val converterMaps      = extractSourceToDestinationMaps(leftovers)

  for converterMap <- converterMaps do seeds = findLocationPart2(seeds, converterMap.mappings)

  seeds.minBy(_.start).start

// findMinimumLocation(data)
// part 1: 388071289

findMinimumLocationPart2(data)
// part 2: 84206669
