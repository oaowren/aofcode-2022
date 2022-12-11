import scala.io.Source
import scala.collection.mutable.ListBuffer

object SeventhDec {
  val day = 7

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_small.txt")
  }
}