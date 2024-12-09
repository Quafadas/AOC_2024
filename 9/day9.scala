import pprint.*

import scala.Long

enum DiskBlock:
  case FakeFile(id: Long, length: Long) extends DiskBlock
  case Space(length: Long) extends DiskBlock

@main def day9 =
  val input = readTestInputForDay(9).mkString
  val makeBlocky = input.foldLeft[(List[DiskBlock], DiskBlock, Long)](List.empty[DiskBlock], DiskBlock.Space(0L), 0L) { case ((acc, prior, priorId), char) =>
    val (newBLock, newId) = prior match
      case DiskBlock.Space(_) =>
        println(char)
        (DiskBlock.FakeFile(1L + priorId, char.asDigit.toLong), 1L + priorId)
      case DiskBlock.FakeFile(_, _) =>
        (DiskBlock.Space(char.toLong), priorId)
    (acc :+ newBLock, newBLock, newId)
  }
  pprintln(makeBlocky)
