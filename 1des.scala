import scala.io.Source
import scala.collection.mutable.ListBuffer

object FirstDec {
  val day = 1

  var ind = 0

  var elves = ListBuffer[Int](0)

  def countCals(s: String): Unit = {
    if (s.isEmpty()){
        ind = ind + 1
        elves += 0
    } else {
        elves(ind) = elves(ind) + s.toInt
    }
  }

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", countCals)
    val elvesSorted = elves.toList.sorted.reverse
    println(elvesSorted(0))
    println(elvesSorted(0) + elvesSorted(1) + elvesSorted(2))
  }
}