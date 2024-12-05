import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

enum RedNosedReactor:
  case Safe, Unsafe

def checkSafe(in: Array[Int]): Boolean =
  ((in > 0).all || (in < 0).all) && (in >= 1 && in <= 3).all || (in <= -1 && in >= -3).all

@main def day2(): Unit =

  val lines = readInputForDay(2)
    .map(_.split(" ").map(_.toInt).increments.tail)
    .zipWithIndex
  // .tapEach(a =>
  //   println(a._2.toString() + " " + a._1.mkString(" ") + checkSafe(a._1))
  // )

  println(lines.map(l => checkSafe(l._1)).toArray.trues)

@main def day2_2(): Unit =

  val lines = readInputForDay(2)
    .map(_.split(" ").map(_.toInt))

  val explodedLines = lines.map(l =>
    val len = l.length
    val checkedCombinations =
      for i <- 0 until len
      yield
        val dropOneElement = l.slice(0, i) ++ l.slice(i + 1, len)
        checkSafe(dropOneElement.increments.tail)
    (checkedCombinations :+ checkSafe(l)).toArray.any
  )
  println(explodedLines.toArray.trues)
