import scala.io.Source
import scala.collection.mutable.{ListBuffer, Stack}

object FifthDec {
  val day = 5

  val pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r

  val smallStack = List(
    Stack("N", "Z"),
    Stack("D", "C", "M"),
    Stack("P")
  )

  val largeStack = List(
    Stack("V", "C", "W", "L", "R", "M", "F", "Q"),
    Stack("L", "Q", "D"),
    Stack("B", "N", "C", "W", "G", "R", "S", "P"),
    Stack("G", "Q", "B", "H", "D", "C", "L"),
    Stack("S", "Z", "F", "L", "G", "V"),
    Stack("P", "N", "G", "D"),
    Stack("W", "C", "F", "V", "P", "Z", "D"),
    Stack("S", "M", "D", "P", "C"),
    Stack("C", "P", "M", "V", "T", "W", "N", "Z")
  )

  val currentStack = largeStack

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def move(n: Int, from: Int, to: Int, stack: List[Stack[String]]): Unit = {
    for (i <- 0 to n - 1){
        val toMove = stack(from - 1).pop()
        stack(to - 1).push(toMove)
    }
  }

  def moveNew(n: Int, from: Int, to: Int, stack: List[Stack[String]]): Unit = {
    val popped = ListBuffer[String]()
    for (i <- 0 to n - 1){
        popped += stack(from - 1).pop()
    }
    stack(to - 1).pushAll(popped.toList.reverse)
  }

  def getAction(line: String): Unit = {
    line match {
        case pattern(n, from, to) => move(n.toInt, from.toInt, to.toInt, currentStack)
    }
  }

  def getNewAction(line: String): Unit = {
    line match {
        case pattern(n, from, to) => moveNew(n.toInt, from.toInt, to.toInt, currentStack)
    }
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", getNewAction)
    println(currentStack.map(e => e.top).mkString)  
  }
}