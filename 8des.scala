import scala.io.Source
import scala.collection.mutable.ListBuffer

object EighthDec {
  val day = 8

  var trees = ListBuffer[String]()

  var discoveredTrees = ListBuffer[(Int, Int)]()

  def xTrees(row: String, x: Int): Unit = {
    discoveredTrees += ((x,0))
    discoveredTrees += ((x, row.length() - 1))
    var tallestFromTheLeft = row.charAt(0).toInt
    for ((c, i) <- row.slice(1, row.length() - 1).toCharArray().zipWithIndex){
        if (c.toInt > tallestFromTheLeft) {
            discoveredTrees += ((x, i +1))
            tallestFromTheLeft = c.toInt
        }
    }
    var tallestFromTheRight = row.charAt(row.length() - 1).toInt
    for ((c, i) <- row.slice(1, row.length() - 1).toCharArray().reverse.zipWithIndex){
        if (c.toInt > tallestFromTheRight) {
            discoveredTrees += ((x, row.length() - 2 - i))
            tallestFromTheRight = c.toInt
        }
    }
  }

  def yTrees(row: String, y: Int): Unit = {
    discoveredTrees += ((0,y))
    discoveredTrees += ((row.length() - 1, y))
    var tallestFromTheLeft = row.charAt(0).toInt
    for ((c, i) <- row.slice(1, row.length() - 1).toCharArray().zipWithIndex){
        if (c.toInt > tallestFromTheLeft) {
            discoveredTrees += ((i + 1, y))
            tallestFromTheLeft = c.toInt
        }
    }
    var tallestFromTheRight = row.charAt(row.length() - 1).toInt
    for ((c, i) <- row.slice(1, row.length() - 1).toCharArray().reverse.zipWithIndex){
        if (c.toInt > tallestFromTheRight) {
            discoveredTrees += ((row.length() - 2 - i, y))
            tallestFromTheRight = c.toInt
        }
    }
  }

  def scenicScore(tree: Int, row: String): Int = {
    val height = row.charAt(tree).toInt
    val firstBlockingTreeLeft = row.slice(0,tree).toCharArray().zipWithIndex.findLast {
        case (el, ind) => el.toInt >= height
    }.map(_._2).getOrElse(0)
    val firstBlockingTreeRight = row.slice(tree+1, row.length()).toCharArray().reverse.zipWithIndex.findLast {
        case (el, ind) => el.toInt >= height
    }.map(_._2).getOrElse(0)
    ((row.length() - 1 - tree) - firstBlockingTreeRight) * (tree - firstBlockingTreeLeft)
  }

  def addToTrees(s: String): Unit = {
    trees += s
  }

  def runner(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }

  def main(args: Array[String]) = {
    runner(s"inputs_2022/day_${day}_large.txt", addToTrees)
    for ((tree, index) <- trees.toList.zipWithIndex) {
        xTrees(tree, index)
    }
    val invertedTrees = (for (i <- 0 to trees.head.length() - 1) yield trees.map(e => e.charAt(i)).mkString).toList
    for ((tree, index) <- invertedTrees.zipWithIndex){
        yTrees(tree, index)
    }
    println(discoveredTrees.toSet.toList.length)
    println(trees.toList.zipWithIndex.flatMap {
        case (el, x) => 
            el.toCharArray().zipWithIndex.map {
                case (el2, y) => 
                    scenicScore(y, trees.toList(x)) * scenicScore(x, invertedTrees(y))
            }
        }.reduce((el, acc) => if (el > acc) el else acc)
    )
  }
}