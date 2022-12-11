import scala.io.Source
import scala.collection.mutable.ListBuffer

object ThirdDec {
  val day = 3

  var score = 0

  var badgeScore = 0

  var sacks = ListBuffer[String]()

  def getScore(char: Char): Unit = {
    if (char.isUpper) {
        score = score + char.toInt - 38
    } else {
        score = score + char.toInt - 96
    }
  }

  def getBadgeScore(char: Char): Unit = {
    if (char.isUpper) {
        badgeScore = badgeScore + char.toInt - 38
    } else {
        badgeScore = badgeScore + char.toInt - 96
    }
  }

  def getPriorityFromLine(line: String): Unit = {
    val comp1 = line.slice(0, line.length() / 2)
    val comp2 = line.slice(line.length() / 2, line.length())
    var visited = ""
    for (i <- comp1.toCharArray() if comp2.contains(i) && !visited.contains(i)) { 
        getScore(i)
        visited += i
    }
  }

  def computeCommonItem(line: String): Unit = {
    var visited = false
    for (i <- line.toCharArray() if sacks(0).contains(i) && sacks(1).contains(i) && !visited) { 
        visited = true
        getBadgeScore(i) 
    }
  }

  def readFile(filename: String, cb: String => Unit): Unit = {
    for ((line, index) <- Source.fromFile(filename).getLines.zipWithIndex) {
      cb(line)
      if (index % 3 == 0){
        sacks = ListBuffer()
        sacks += line
      } else if (index % 3 == 2){
        computeCommonItem(line)
      } else {
        sacks += line
      }
    }
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", getPriorityFromLine)
    println(score)
    println(badgeScore)
  }
}