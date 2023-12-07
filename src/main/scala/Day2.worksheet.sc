import scala.annotation.tailrec

val MaxPossibleRedCubes   = 12
val MaxPossibleGreenCubes = 13
val MaxPosibleBlueCubes   = 14

case class SetOfCubes(green: Int, blue: Int, red: Int)

def readInput(fileName: String) =
  io.Source.fromResource(s"day2/$fileName.txt").getLines().toList

def createSetOfCubes(play: List[String]): SetOfCubes =
  @tailrec
  def loop(list: List[String], obj: SetOfCubes): SetOfCubes =
    if list.isEmpty then obj
    else
      list.head match
        case s"$numberOfCube blue" =>
          loop(list.tail, obj.copy(blue = numberOfCube.toInt))
        case s"$numberOfCube green" =>
          loop(list.tail, obj.copy(green = numberOfCube.toInt))
        case s"$numberOfCube red" =>
          loop(list.tail, obj.copy(red = numberOfCube.toInt))
        case _ => loop(list.tail, SetOfCubes(green = 0, red = 0, blue = 0))

  loop(play, SetOfCubes(green = 0, red = 0, blue = 0))

def processGames(input: List[String]): List[List[SetOfCubes]] =
  val games = input.map { game =>
    game
      .split(":")
      .tail
      .mkString
      .trim()
      .split(";")
      .toList
  }

  games.map { game =>
    game.map { play =>
      val preparedPlay = play.trim().split(",").toList.map(_.trim())
      createSetOfCubes(preparedPlay)
    }
  }

def countNumberOfPossibleGames(games: List[List[SetOfCubes]]): Int =
  games
    .zip(LazyList.from(1))
    .filter { game =>
      game._1.forall(subset =>
        subset.green <= MaxPossibleGreenCubes && subset.blue <= MaxPosibleBlueCubes && subset.red <= MaxPossibleRedCubes
      )
    }
    .map(_._2)
    .sum

def sumOfPowerOfSubsetCubesOfGames(
  games: List[List[SetOfCubes]]
): Int =
  games.map { game =>
    game.maxBy(_.green).green * game.maxBy(_.blue).blue * game.maxBy(_.red).red
  }.sum

// Part 1
// val inputTest1: List[String] = readInput("day2_1_test")
val inputReal1: List[String] = readInput("day2_1")
val processedGamesReal1      = processGames(inputReal1)
countNumberOfPossibleGames(processedGamesReal1)

// Part 2
// val inputTest2: List[String] = readInput("day2_2_test")
// we could use the real input of Task 1 for solving Task 2
val inputReal2: List[String] = readInput("day2_1")
val processedGamesReal2      = processGames(inputReal2)

sumOfPowerOfSubsetCubesOfGames(processedGamesReal2)
