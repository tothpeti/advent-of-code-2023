import scala.collection.immutable.ListMap

def readInput(fileName: String): List[String] =
  io.Source.fromResource(s"day8/$fileName.txt").getLines().toList

case class Node(name: String, left: String, right: String)

val OneOrMoreSpace = "\\s+"

// val data: List[String] = readInput("day8_1_test")
// val data: List[String] = readInput("day8_1_test2")
// val data: List[String] = readInput("day8_1")
val data: List[String] = readInput("day8_2_test")

def processInput(input: List[String]): (String, ListMap[String, Node]) =
  def processElements(remaining: List[String], accumulator: ListMap[String, Node]): ListMap[String, Node] =
    remaining match
      case head :: next =>
        head match
          case s"$currName = ($left, $right)" =>
            processElements(next, accumulator + (currName -> Node(currName, left, right)))
      case Nil =>
        accumulator

  data.filter(_.nonEmpty) match
    case head :: next =>
      (head, processElements(next, ListMap[String, Node]().empty))
    case Nil =>
      ("", ListMap[String, Node]().empty)

def calculateTotalNumberOfSteps(input: (String, ListMap[String, Node]), starterNode: String) =
  def loop(remaining: List[Char], currentElement: String, counter: Int = 0): Int =
    if currentElement == "ZZZ" then counter
    else
      remaining match
        case head :: tail =>
          if head == 'R' then loop(tail, input._2(currentElement).right, counter + 1)
          else loop(tail, input._2(currentElement).left, counter + 1)
        case Nil =>
          loop(input._1.toList, currentElement, counter)

  loop(input._1.toList, starterNode)

def calculateTotalNumberOfStepsPart2(input: (String, ListMap[String, Node])): BigInt =
  def lcm(list: Seq[BigInt]): BigInt =
    list.foldLeft(1: BigInt) { (a, b) =>
      b * a / LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
    }

  def loop(remaining: List[Char], currentElement: String, counter: Int = 0): Int =
    if currentElement.endsWith("Z") then counter
    else
      remaining match
        case head :: tail =>
          if head == 'R' then loop(tail, input._2(currentElement).right, counter + 1)
          else loop(tail, input._2(currentElement).left, counter + 1)
        case Nil =>
          loop(input._1.toList, currentElement, counter)

  val counters = processed._2.filter { case (key, value) => key.endsWith("A") }
    .map(_._2)
    .toList
    .map { node =>
      BigInt(loop(input._1.toList, node.name))
    }

  lcm(counters)

// val processed = processInput(data)
// calculateTotalNumberOfSteps(processed, "AAA")
// Part 1 solution: 15517

val processed = processInput(data)
calculateTotalNumberOfStepsPart2(processed)
// Part 2 solution: 14935034899483
