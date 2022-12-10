import scala.io.Source
import scala.collection.mutable.ListBuffer

object TenDec {
  val day = 10

  var tick = 0
  var x = 1

  var tickValues = new ListBuffer[Int]()
  var image = new ListBuffer[String]()
  var curStr = ""

  def draw(): Unit = {
    if (((tick % 40) >= x-1) && ((tick % 40) <= x+1)){
      curStr += "#"
    } else {
      curStr += "."
    }
    if ((curStr.length() % 40) == 0){
      image += curStr
      curStr = ""
    }
  }

  def clock(action: String): Unit = {
    val _ = draw()
    tick = tick + 1
    if ((tick - 20) % 40 == 0){
      println(s"$tick: ${tick*x}")
      tickValues += tick*x
    }
    action match {
      case "noop" => ()
      case e if e.contains("addx") => {
        val _ = draw()
        tick = tick + 1
        if ((tick - 20) % 40 == 0){
          println(s"$tick: ${tick*x}")
          tickValues += tick*x
        }
        val toAdd = e.split(" ").last.toInt
        x = x + toAdd
      }
    }
  }

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  val solution = List(
    "##..##..##..##..##..##..##..##..##..##..",
    "###...###...###...###...###...###...###.",
    "####....####....####....####....####....",
    "#####.....#####.....#####.....#####.....",
    "######......######......######......####",
    "#######.......#######.......#######....."
  )

  def printImage(image: List[String]): Unit = {
    image.foreach(i => println(i))
  }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", clock)
    printImage(image.toList)
  }
}