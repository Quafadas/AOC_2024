import pprint.*
@main def day7 =
  val input = readInputForDay(7)

  val startNumber = for {
    line <- input
    number = line.indexOf(':')
    numVal = line.take(number).toLong
    madeOf = line.drop(number + 2)
    nums = madeOf.split(" ").map(_.toLong)
  } yield (numVal, nums)

  val operators = List("*", "+")
  val operators2 = List("*", "+", "||")

  def operatorPaths(length: Int, operatorsIn: List[String]): Seq[Seq[String]] =
    length match
      case 0 => Seq(Seq.empty[String])
      case _ => operatorPaths(length - 1, operatorsIn).flatMap(seq => operatorsIn.map(op => seq :+ op))

  def expanded(operatorsIn: List[String], targets: Iterator[(Long, Array[Long])]) = for {
    (numVal, madeOf) <- targets
    _ = pprintln(s"numVal: $numVal, madeOf: ${madeOf.mkString(",")} length ${madeOf.length}")
    expanded = operatorPaths(madeOf.length - 1, operatorsIn)
    // _ = pprintln(s"expanded: ${expanded.mkString("\n")}")
    arg = for (path <- expanded) yield {
      path.zip(madeOf.tail).foldLeft(madeOf.head) { case (acc, together) =>
        if (acc > numVal) {
          acc
        } else {
          together match
            case ("+", b)  => acc + b
            case ("*", b)  => acc * b
            case ("||", b) =>
              // println(s"acc: $acc, b: $b")
              // println(s"acc.toString() + b.toString(): ${acc.toString() + b.toString()}")
              (acc.toString() + b.toString()).toLongOption.getOrElse(numVal + 1)
        }
      }

    }

  } yield {
    expanded.zip(arg).collectFirst({ case (a, b) if b == numVal => (numVal, a) })
  }

  println("part 1")
  pprintln(expanded(operators, startNumber).toArray.flatten.map(_._1).sum)
  println("part 2")
  pprintln(expanded(operators2, startNumber).toArray.flatten.map(_._1).sum)
