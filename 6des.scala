import scala.io.Source
import scala.collection.mutable.ListBuffer

object SixthDec {
  val day = 6

  var processed = 0
  var currentWindow = ""

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def getMarker(line: String): Unit = {
    for ((c, index) <- line.toCharArray().zipWithIndex){
        currentWindow = (currentWindow + c)
        currentWindow = currentWindow.slice(currentWindow.length() - 4, currentWindow.length())
        if (currentWindow.distinct.length() == 4 && processed == 0) {
            processed = index + 1
        }
    }
  }

  def getMessageMarker(line: String): Unit = {
    for ((c, index) <- line.toCharArray().zipWithIndex){
        currentWindow = (currentWindow + c)
        currentWindow = currentWindow.slice(currentWindow.length() - 14, currentWindow.length())
        if (currentWindow.distinct.length() == 14 && processed == 0) {
            processed = index + 1
        }
    }
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", getMessageMarker)
    println(processed)
  }
}