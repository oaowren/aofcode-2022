import scala.io.Source
import scala.collection.mutable.ListBuffer

object EleventhDec {

  var items = ListBuffer[ListBuffer[Long]](
    ListBuffer[Long](98L, 70L, 75L, 80L, 84L, 89L, 55L, 98L),
    ListBuffer[Long](59L),
    ListBuffer[Long](77L, 95L, 54L, 65L, 89L),
    ListBuffer[Long](71L, 64L, 75L),
    ListBuffer[Long](74L, 55L, 87L, 98L),
    ListBuffer[Long](90L, 98L, 85L, 52L, 91L, 60L),
    ListBuffer[Long](99L, 51L),
    ListBuffer[Long](98L, 94L, 59L, 76L, 51L, 65L, 75L)
  )
  
  def reduceNum(item: Long): Long = {
    // Math.floordiv(item, 3L)
    item % 9699690L
  }

  def monkey0(item: Long): (Long, Int) = {
    val op = reduceNum(item * 2) 
    if (op % 11 == 0) (op, 1) else (op, 4)
  } 

  def monkey1(item: Long): (Long, Int) = {
    val op = reduceNum(item * item)
    if (op % 19 == 0) (op, 7) else (op, 3)
  } 

  def monkey2(item: Long): (Long, Int) = {
    val op = reduceNum(item + 6)
    if (op % 7 == 0) (op, 0) else (op, 5)
  } 

  def monkey3(item: Long): (Long, Int) = {
    val op = reduceNum(item + 2)
    if (op % 17 == 0) (op, 6) else (op, 2)
  } 

  def monkey4(item: Long): (Long, Int) = {
    val op = reduceNum(item * 11)
    if (op % 3 == 0) (op, 1) else (op, 7)
  } 

  def monkey5(item: Long): (Long, Int) = {
    val op = reduceNum(item + 7)
    if (op % 5 == 0) (op, 0) else (op, 4)
  }

  def monkey6(item: Long): (Long, Int) = {
    val op = reduceNum(item + 1)
    if (op % 13 == 0) (op, 5) else (op, 2)
  } 

  def monkey7(item: Long): (Long, Int) = {
    val op = reduceNum(item + 5)
    if (op % 2 == 0) (op, 3) else (op, 6)
  } 

  val monkeys: List[(Long) => ((Long, Int))] = List(monkey0, monkey1, monkey2, monkey3, monkey4, monkey5, monkey6, monkey7)

  var inspections = ListBuffer[Long](0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)

  def main(args: Array[String]) = {
    for (i <- 0 to 9999){
        for (m <- 0 to 7){
            for (item <- items(m)){
                inspections(m) = inspections(m) + 1
                monkeys(m)(item) match {
                    case (l, i) => items(i) += l
                }
            }
            items(m) = ListBuffer[Long]()
        }
    }
    val sorted = inspections.toList.sorted.reverse
    println(sorted(0) * sorted(1))
  }
}