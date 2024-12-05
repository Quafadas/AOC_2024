import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

@main def day1(): Unit =
  val input = io.Source
    .fromFile("1/input.txt")
    .getLines()
    .map(l =>
      l.split("   ") match {
        case Array(a, b) => (a.toInt, b.toInt)
        case _: Array[?] => ???
      }
    )
    .toArray

  val nums1 = input.map(_._1).sorted
  val nums2 = input.map(_._2).sorted

  val diff = (nums1 - nums2).map(_.abs)

  println(diff.sum)

@main def day1_2 =
  val input = io.Source
    .fromFile("1/input.txt")
    .getLines()
    .map(l =>
      l.split("   ") match {
        case Array(a, b) => (a.toInt, b.toInt)
        case _: Array[?] => ???
      }
    )
    .toArray

  val nums2 = input.map(_._2).groupBy(identity).view.mapValues(_.size)
  val nums1 = input.map(_._1)

  val similarity = for (n <- nums1) yield {
    val boost = nums2.get(n).getOrElse(0)
    n * boost
  }

  println(similarity.sum)
