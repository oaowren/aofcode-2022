import scala.io.Source
import scala.collection.mutable.ListBuffer

object FourthDec {
  val day = 4

  var count = 0
  var overlaps = 0

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def numberOfOverlaps(pairs: Array[Array[Int]]): Unit = {
    if (pairs(0)(1) >= pairs(1)(0) && pairs(0)(1) <= pairs(1)(1)) { overlaps += 1}
    else if (pairs(1)(1) >= pairs(0)(0) && pairs(1)(1) <= pairs(0)(1)) { overlaps += 1}
  }

  def pairIsContained(line: String): Unit = {
    val pairs = line.split(",").map(x => x.split("-").map(_.toInt))
    if ((pairs(0)(0) >= pairs(1)(0) && pairs(0)(1) <= pairs(1)(1)) || (pairs(0)(0) <= pairs(1)(0) && pairs(0)(1) >= pairs(1)(1))){count += 1}
    numberOfOverlaps(pairs)
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", pairIsContained)
    println(count)
    println(overlaps)
  }
}