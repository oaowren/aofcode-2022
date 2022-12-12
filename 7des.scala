import scala.io.Source
import scala.collection.mutable.{ListBuffer, Map}

case class File(parent: Dir, name: String, size: Long)
case class Dir(parent: Option[Dir], name: String) {
  val files: ListBuffer[File] = ListBuffer()
  val dirs: Map[String, Dir] = Map.empty
  
  def size: Long = files.map(_.size).sum + dirs.values.map(_.size).sum
  
  def addFile(f: File): Unit = files += f
  def addDir(d: Dir): Option[Dir] = dirs.put(d.name, d)
  
  def getDir(name: String): Option[Dir] = dirs.get(name)
}

object SeventhDec {
  val day = 7

  val cdPattern = "\\$ cd ([a-z]+)".r
  val dirPattern = "dir ([a-z]+)".r
  val filePattern = "([0-9]+) (.+)".r

  val root = Dir(None, "/")
  var cwd = root

  def parseTerminalInput(line: String): Unit = {
    line match {
      case "$ cd /" => cwd = root
      case "$ cd .." => cwd = cwd.parent.getOrElse(cwd)
      case cdPattern(dirName) => {
        cwd.getDir(dirName) match {
          case Some(dir) => cwd = dir
          case None => {
            val newDir = Dir(Some(cwd), dirName)
            cwd.addDir(newDir)
            cwd = newDir
          }
        }
      }
      case "$ ls" =>
      case dirPattern(dirName) => cwd.getDir(dirName) match {
        case None => cwd.addDir(Dir(Some(cwd), dirName))
        case Some(_) =>
      }
      case filePattern(size, fileName) => cwd.addFile(File(cwd, fileName, size.toLong))
    }
  }

  def dirSums(dirs: List[Dir], maxSizeToCount: Long): Long = {
      dirs.foldLeft(0L) { (acc, dir) =>
        val dirSize = dir.size match {
          case s if s <= maxSizeToCount => s
          case _ => 0
        }
        
        val subdirsSize = dirSums(dir.dirs.values.toList, maxSizeToCount)
        
        acc + dirSize + subdirsSize
      }
    }

  def readFile(filename: String, cb: String => Unit): Unit = {
    for (line <- Source.fromFile(filename).getLines) {
      cb(line)
    }
  }
  
  def dirSizes(dirs: List[Dir]): List[Long] = {
      dirs.foldLeft[List[Long]](Nil) { (acc, dir) =>
        (dir.size :: acc) ::: dirSizes(dir.dirs.values.toList)
      }
    }

  def main(args: Array[String]) = {
    readFile(s"inputs_2022/day_${day}_large.txt", parseTerminalInput)
    println(dirSums(List(root), 100000L))
    val additionalSpaceNeeded = 30000000 - (70000000 - root.size)
    println(dirSizes(List(root)).sorted.collectFirst {
      case size if size > additionalSpaceNeeded => size
    })
    
  }
}