import scala.io.Source
import scala.collection.mutable.ListBuffer

object NinthDec {
  val day = 9

  var headPos: (Int, Int) = (0,0)
  var tails = ListBuffer[(Int, Int)](
    (0,0),
    (0,0),
    (0,0),
    (0,0),
    (0,0),
    (0,0),
    (0,0),
    (0,0),
    (0,0),
  )
  var tailPos: (Int, Int) = (0,0)

  var visited = ListBuffer[(Int, Int)]((0,0))

  val pattern = "([URDL]) ([0-9]+)".r

  def isAdjacent(head: (Int, Int), tail: (Int, Int)): Boolean = {
    (head._1 - tail._1).abs < 2 && (head._2 - tail._2).abs < 2
  }

  def performAction(action: String, pos: (Int, Int)): (Int, Int) = {
    action match {
        case "U" => (pos._1 + 1, pos._2)
        case "D" => (pos._1 - 1, pos._2)
        case "L" => (pos._1, pos._2 - 1)
        case "R" => (pos._1, pos._2 + 1)
        case "N" => pos
    }
  }
  
  def getAction(head: (Int, Int), tail: (Int, Int)): String = {
    if (head._1 - tail._1 >= 2){
        if (head._2 - tail._2 == 1) {
            "R"
        } else if (head._2 - tail._2 == -1){
            "L"
        } else {
            "U"
        }
    } else if (head._1 - tail._1 <= -2) {
        if (head._2 - tail._2 == 1) {
            "R"
        } else if (head._2 - tail._2 == -1){
            "L"
        } else {
            "D"
        }
    } else if (head._2 - tail._2 >= 2) {
        if (head._1 - tail._1 == 1) {
            "U"
        } else if (head._1 - tail._1 == -1){
            "D"
        } else {
            "R"
        }
    } else if (head._2 - tail._2 <= - 2) {
        if (head._1 - tail._1 == 1) {
            "U"
        } else if (head._1 - tail._1 == -1){
            "D"
        } else {
            "L"
        }
    } else {
        "N"
    }
  }

  def isDiagonal(head: (Int, Int), tail: (Int, Int)): Boolean = {
    (head._1 - tail._1).abs == 2 && (head._2 - tail._2).abs == 2
  }

  def moveTail(head: (Int, Int), tail: (Int, Int)): (Int, Int) = {
    var temp = tail
    if (isDiagonal(head, tail)){
        temp = (temp._1 + (head._1 - temp._1) / 2, temp._2 + (head._2 - temp._2) / 2)
    }
    while (!isAdjacent(head, temp)){
        temp = performAction(getAction(head, temp), temp)
    }
    temp
  }

  def doStuff(line: String): Unit = {
    line match {
        case pattern(action, moves) => for (i <- 0 to moves.toInt - 1) {
            headPos = performAction(action, headPos)
            if (!isAdjacent(headPos, tailPos)){
                tailPos = moveTail(headPos, tailPos)
                visited += tailPos
            }
        }
    }
  }

  def doStuff2(line: String): Unit = {
    println(s"(${headPos._1}, ${headPos._2}), " ++ tails.map(e => s"(${e._1}, ${e._2})").mkString(", "))
    line match {
        case pattern(action, moves) => for (i <- 0 to moves.toInt - 1) {
            headPos = performAction(action, headPos)
            if (!isAdjacent(headPos, tails(0))){
                tails(0) = moveTail(headPos, tails(0))
            }
            for (i <- 1 to tails.length - 1 if (!isAdjacent(tails(i-1), tails(i)))) {
                tails(i) = moveTail(tails(i-1), tails(i))
                if (i == 8){
                    visited += tails(8)
                }
            }
        }
    }
  }

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", doStuff2)
    println(s"(${headPos._1}, ${headPos._2}), " ++ tails.map(e => s"(${e._1}, ${e._2})").mkString(", "))
    println(visited.toSet.toList.length)
  }
}