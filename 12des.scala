import scala.io.Source
import scala.collection.mutable.{ListBuffer, Queue}

object TwelfthDec {
  val day = 12

  var map = ListBuffer[String]()
  var values = ListBuffer[ListBuffer[Int]]()

  var myPos: (Int, Int) = (0,0)
  var goalPos: (Int, Int) = (0,0)

  var possibleEnds: ListBuffer[(Int, Int)] = ListBuffer()

  var visited: ListBuffer[(Int, Int)] = ListBuffer()
  var toVisit: Queue[(Int, Int)] = Queue()

  def readFile(filename: String): Unit = {
    for ((line, index) <- Source.fromFile(filename).getLines.zipWithIndex) {
      map += line
      values += ListBuffer(line.toCharArray().map(_ => Int.MaxValue)*)
      if (line.contains("S")){
        myPos = (index, line.indexOf("S"))
      }
      for ((l, i) <- line.toCharArray().zipWithIndex){ if (l == 'a' || l == 'S') possibleEnds += ((index, i)) else ()}
      if (line.contains("E")){
        goalPos = (index, line.indexOf("E"))
        values(goalPos._1)(goalPos._2) = 0
        toVisit.enqueue(goalPos)
      }
    }
  }

  def allowedMove(pos: (Int, Int), nextPos: (Int, Int)): Boolean = {
    var c = map(pos._1).charAt(pos._2)
    var nextC = map(nextPos._1).charAt(nextPos._2)
    if (c == 'E'){
        c = 'z'
    } else if (nextC == 'S') {
        nextC = 'a'
    }
    c.toInt - nextC.toInt <= 1
  }

  def isDefined(pos: (Int, Int)): Boolean = {
    pos._1 >= 0 && pos._2 >= 0 && pos._1 < map.length && pos._2 < map.head.length()
  }

  def getPossibleMoves(currentPos: (Int, Int)): Unit= {
    val moveU = (currentPos._1 - 1, currentPos._2)
    val moveD = (currentPos._1 + 1, currentPos._2)
    val moveL = (currentPos._1, currentPos._2 - 1)
    val moveR = (currentPos._1, currentPos._2 + 1)
    ListBuffer(moveU, moveD, moveL, moveR).filter(m => isDefined(m) && allowedMove(currentPos, m) && !toVisit.exists(p=> m == p) && !visited.exists(p => m == p)).foreach(m => {
          values(m._1)(m._2) = Math.min(values(m._1)(m._2), values(currentPos._1)(currentPos._2) + 1)
          toVisit.enqueue(m)
        })
  }

  def allPossibleEndsFound(possibleEnds: ListBuffer[(Int, Int)]): Boolean = {
    !possibleEnds.exists(e => values(e._1)(e._2) == Int.MaxValue)
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt")
    // If queue is empty, we have visited all possible ends
    while (!allPossibleEndsFound(possibleEnds) && !toVisit.isEmpty){
        var next = toVisit.dequeue()
        getPossibleMoves((next._1, next._2))
        visited += ((next._1, next._2))
    }
    println(values(myPos._1)(myPos._2))
    println(possibleEnds.map(e => values(e._1)(e._2)).min)
  }
}