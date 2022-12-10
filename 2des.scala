import scala.io.Source
import scala.collection.mutable.ListBuffer

object SecondDec {
  val day = 2

  var score = 0
  var newScore = 0

  def getScore(line: String): Unit = {
    val splitted = line.split(" ")
    val selectionScore = splitted.last match {
        case "X" => 1
        case "Y" => 2
        case "Z" => 3
    }
    val winScore = (splitted.head, splitted.last) match {
        case ("A", "Y") | ("B", "Z") | ("C", "X") => 6
        case ("A", "X") | ("B", "Y") | ("C", "Z") => 3
        case _ => 0
    }
    score = score + selectionScore + winScore
  }

  def getNewScore(line: String): Unit = {
    val splitted = line.split(" ")
    val selectionScore = splitted.last match {
        case "X" => 0
        case "Y" => 3
        case "Z" => 6
    }
    val winScore = (splitted.head, splitted.last) match {
        case ("B", "Z") | ("C", "Y") | ("A", "X") => 3
        case ("A", "Z") | ("B", "Y") | ("C", "X") => 2
        case _ => 1
    }
    newScore = newScore + selectionScore + winScore
  }

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", getScore)
    readFile(s"inputs_2022/day_${day}_large.txt", getNewScore)
    println(score)
  }
}