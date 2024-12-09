import pprint.*

import scala.Long

enum Direction:
  case Head, Tail

enum DiskBlock:
  case FakeFile(id: Long, length: Long) extends DiskBlock
  case Space(length: Long) extends DiskBlock
  case PartialSpace extends DiskBlock
  case PartialFile(parent: FakeFile) extends DiskBlock
  def blockLength =
    this match
      case FakeFile(_, length) => length
      case Space(length)       => length
      case PartialSpace        => 1L
      case PartialFile(parent) => 1L

@main def day9 =
  val input = readTestInputForDay(9).mkString
  val (makeBlocky, _, _) = input.foldLeft[(List[DiskBlock], DiskBlock, Long)](List.empty[DiskBlock], DiskBlock.Space(0L), 0L) { case ((acc, prior, priorId), char) =>
    val (newBLock, newId) = prior match
      case DiskBlock.Space(_) =>
        // println(char)
        (DiskBlock.FakeFile(priorId, char.asDigit.toLong), 1L + priorId)
      case DiskBlock.FakeFile(_, _) =>
        (DiskBlock.Space(char.asDigit.toLong), priorId)
      case _ => ???
    (acc :+ newBLock, newBLock, newId)
  }

  val diskSize = makeBlocky.foldLeft(0L) { case (acc, block) =>
    acc + block.blockLength
  }

  def reorder(headIdx: Int, tailIdx: Int, diskBlocks: List[DiskBlock], checksum: Long, direction: Direction): (Long, List[DiskBlock]) =
    pprintln(s"headIdx: $headIdx, tailIdx: $tailIdx, checksum: $checksum, direction: $direction")
    pprintln(diskBlocks)
    if (diskBlocks.isEmpty)
      (checksum, diskBlocks)
    else {
      // val (head_headSide, tail_headSide) = diskBlocks.splitAt(headIdx)
      val (head_tailSide, tail_tailSide) = diskBlocks.splitAt(tailIdx)

      direction match
        case Direction.Head =>
          diskBlocks.head match
            case DiskBlock.FakeFile(id, length) =>
              // Increment the headidx by 1, calcualte the checksum increment forwards
              val newHeadIdx = headIdx + length.toInt
              val newChecksum = checksum + ((headIdx until headIdx + length.toInt).sum * id).toLong
              reorder(newHeadIdx, tailIdx, diskBlocks.tail, newChecksum, Direction.Head)
            case DiskBlock.Space(length) =>
              val expandSpces = List.fill(length.toInt)(DiskBlock.PartialSpace) ++ diskBlocks.tail
              reorder(headIdx, tailIdx, expandSpces, checksum, Direction.Tail)
            case DiskBlock.PartialSpace =>
              diskBlocks.last match
                case DiskBlock.FakeFile(id, length) => reorder(headIdx, tailIdx, diskBlocks, checksum, Direction.Tail)
                case DiskBlock.Space(length)        => reorder(headIdx, tailIdx, diskBlocks, checksum, Direction.Tail)
                case DiskBlock.PartialSpace         => reorder(headIdx, tailIdx, diskBlocks, checksum, Direction.Tail)
                case DiskBlock.PartialFile(parent) =>
                  reorder(headIdx, tailIdx, diskBlocks.last +: diskBlocks.tail.dropRight(1), checksum, Direction.Head)

            case DiskBlock.PartialFile(parent) =>
              val newHeadIdx = headIdx + 1
              val newChecksum = checksum + parent.id * headIdx
              reorder(newHeadIdx, tailIdx, diskBlocks.tail, newChecksum, Direction.Head)

        case Direction.Tail =>
          diskBlocks.last match {
            case d: DiskBlock.FakeFile =>
              val expanded = diskBlocks.dropRight(1) ++ List.fill(d.length.toInt)(DiskBlock.PartialFile(d))
              reorder(headIdx, tailIdx, expanded, checksum, Direction.Head)
            case DiskBlock.Space(length) =>
              val dropTailSpace = diskBlocks.take(diskBlocks.length - 1)
              reorder(headIdx, tailIdx, dropTailSpace, checksum, Direction.Tail)
            case DiskBlock.PartialSpace =>
              val dropTailSpace = diskBlocks.take(diskBlocks.length - 1)
              reorder(headIdx, tailIdx, dropTailSpace, checksum, Direction.Tail)
            case DiskBlock.PartialFile(parent) =>
              reorder(headIdx, tailIdx, diskBlocks, checksum, Direction.Head)
          }

    }

  pprintln(makeBlocky)

  val part1 = reorder(0, 1, makeBlocky, 0L, Direction.Head)

  pprintln(part1)
