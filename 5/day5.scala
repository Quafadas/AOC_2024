import vecxt.all.*

@main def day5 =
  val (rules, dataWithBLank) =
    readInputForDay(5).toList.partition(_.contains("|"))
  val data = dataWithBLank.tail

  // println(rules)
  // println(data)

  def ruleCheck(a: Int, b: Int) = (in: String) => {
    val foundFirst = in.indexOf(a.toString())
    val foundSecond = in.indexOf(b.toString())

    (foundFirst, foundSecond) match
      case (-1, _) => true
      case (_, -1) => true
      case (_, _)  => foundFirst < foundSecond
  }

  def ruleFix(a: Int, b: Int) = (in: String) => {
    val foundFirst = in.indexOf(a.toString())
    val foundSecond = in.indexOf(b.toString())

    val newString =
      if foundFirst > foundSecond then
        in.substring(0, foundFirst) + in.substring(foundSecond, 3) + in
          .substring(foundFirst, foundFirst + 3)
          + in.substring(foundSecond + 3, in.length())
      else in.reverse

    if (ruleCheck(a, b)(newString)) newString
    else throw new Exception(s"Rule fix failed for $a, $b, $in")

  }

  val rulesInts =
    rules.map(_.split("\\|").map(_.trim.toInt) match {
      case Array(a, b) => ruleCheck(a, b)
    })

  val rulesCheck = data.map { a =>
    rulesInts.forall(r => r(a))
  }

  val checkedRules = rulesCheck.zip(data)

  val (passed, failed) = checkedRules.partition { _._1 }

  val middle = passed.map { a =>
    val arg = a._2.split(",")
    arg((arg.length + 1) / 2 - 1).toInt
  }

  println(middle)
  println(middle.sum)

  for ((a, b) <- failed) yield {
    println(a)
    println(b)
    ruleFix()
  }

  // val validCount =
  //   parsedData.map(pages => rulesInts.map(rule => rule(pages))).trues

  // println(validCount)
