def readInput(fileName: String) =
  io.Source.fromResource(s"day3/$fileName.txt").getLines().toList

// val data: List[String] = readInput("day3_1_test")
val data: List[String] = readInput("day3_1")
// val data: List[String] = readInput("day3_1_own")

val MaxHeight = data.length
val MaxWidth = data(0).length

def isSpecialChar(char: Char): Boolean =
  !char.isDigit && char != '.'

case class Direction(x: Int, y: Int)

val Directions = List(
  Direction(-1, 0), // above
  Direction(1, 0), // below
  Direction(0, 1), // right
  Direction(0, -1) // left
)

def checkIfPositionIsNotOutOfBound(
    currPosX: Int,
    currPosY: Int
): Boolean =
  currPosX >= 0 && currPosY >= 0 &&
    currPosX < MaxHeight && currPosY < MaxWidth

def searchEndDigit(posX: Int, start: Int, end: Int, step: Int): Int =
  (start to end by step)
    .map(y => (data(posX)(y), y))
    .takeWhile(((ch, idx) => ch != '.' && !isSpecialChar(ch)))
    .last
    ._2

def gatherNumberParts(
    posX: Int,
    start: Int,
    end: Int,
    step: Int
): List[String] =
  val numberParts = (start to end by step)
    .map(y => data(posX)(y))
    .takeWhile(ch => ch != '.' && !isSpecialChar(ch))

  if step == 1 then List(numberParts.mkString)
  else List(numberParts.reverse.mkString)

def getDiagonalNumbers(
    posX: Int,
    posY: Int,
    directions: List[Direction],
    gatheredValues: List[String]
): List[String] = {
  directions match
    case head :: tail =>
      head match
        case Direction(0, 1) =>
          // Moving "cursor" to the right
          if data(posX)(posY + head.y).isDigit then
            val id = searchEndDigit(posX, posY + head.y, MaxWidth - 1, 1)
            val nums = gatherNumberParts(posX, id, 0, -1)
            getDiagonalNumbers(posX, posY, tail, gatheredValues ++ nums)
          else getDiagonalNumbers(posX, posY, tail, gatheredValues)

        case Direction(0, -1) =>
          // Moving "cursor" to the left
          if data(posX)(posY + head.y).isDigit then
            val id = searchEndDigit(posX, posY + head.y, MaxWidth - 1, 1)
            val nums = gatherNumberParts(posX, id, 0, -1)
            getDiagonalNumbers(posX, posY, tail, gatheredValues ++ nums)
          else getDiagonalNumbers(posX, posY, tail, gatheredValues)
        case _ => List.empty
    case Nil =>
      gatheredValues
}

def findNumbersWithEverySymbolAndDirection(
    specialCharPosX: Int,
    specialCharPosY: Int
): List[Int] = {
  Directions
    .map { direction =>
      val newPosX = direction.x + specialCharPosX
      val newPosY = direction.y + specialCharPosY

      // Diagonal case
      if checkIfPositionIsNotOutOfBound(newPosX, newPosY) &&
        data(newPosX)(newPosY) == '.'
      then
        direction match
          case Direction(-1, 0) | Direction(1, 0) =>
            getDiagonalNumbers(
              newPosX,
              newPosY,
              List(Direction(0, 1), Direction(0, -1)),
              List.empty
            )
          case _ => List.empty
      else if checkIfPositionIsNotOutOfBound(newPosX, newPosY) &&
        data(newPosX)(newPosY).isDigit
      then
        val id = searchEndDigit(newPosX, newPosY, MaxWidth - 1, 1)
        gatherNumberParts(newPosX, id, 0, -1)
      else List.empty
    }
    .flatten
    .collect { case x if x != "" => x.toInt }
}

def findNumbersForOnlyDiagonal(
    specialCharPosX: Int,
    specialCharPosY: Int
): List[Int] = {
  Directions
    .map { direction =>
      val newPosX = direction.x + specialCharPosX
      val newPosY = direction.y + specialCharPosY

      // Diagonal case
      if checkIfPositionIsNotOutOfBound(newPosX, newPosY) &&
        data(newPosX)(newPosY) == '.'
      then
        direction match
          case Direction(-1, 0) | Direction(1, 0) =>
            (
              (specialCharPosX, specialCharPosY),
              getDiagonalNumbers(
                newPosX,
                newPosY,
                List(Direction(0, 1), Direction(0, -1)),
                List.empty
              )
            )
          case _ => ((-1, -1), List.empty[String])
      else if checkIfPositionIsNotOutOfBound(newPosX, newPosY) &&
        data(newPosX)(newPosY).isDigit
      then
        val id = searchEndDigit(newPosX, newPosY, MaxWidth - 1, 1)
        (
          (specialCharPosX, specialCharPosY),
          gatherNumberParts(newPosX, id, 0, -1)
        )
      else ((-1, -1), List.empty[String])
    }
    .filter { case (position, _) => position._1 != -1 && position._2 != -1 }
    .filter(_._2.nonEmpty)
    .flatMap(_._2)
    .map(_.toInt)
}

def sumOfParts() =
  (
    for
      x <- 0 until MaxHeight
      y <- 0 until MaxWidth
      num =
        if isSpecialChar(data(x)(y)) then
          findNumbersWithEverySymbolAndDirection(x, y)
        else List.empty
    yield num
  ).toList.flatten
    .filter(_ != 0)
    .sum

def sumOfProductOfGears() =
  (
    for
      x <- 0 until MaxHeight
      y <- 0 until MaxWidth
      num =
        if isSpecialChar(data(x)(y)) && data(x)(y) == '*' then
          findNumbersForOnlyDiagonal(x, y)
        else List.empty
      res = if num.length == 2 then num else List.empty
    yield res
  ).toList
    .filter(_.nonEmpty)
    .map(_.product)
    .sum

sumOfParts()
// Result for part1: 544664

sumOfProductOfGears()
// Result of part2: 84495585
