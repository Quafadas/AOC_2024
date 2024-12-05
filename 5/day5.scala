import vecxt.all.*

@main def day5 =
  val (rules, data) = readInputStrings("day5").partition(_.contains("|"))

  def ruleCheck(a: Int, b: Int) = (in: List[Int]) => {
    val foundFirst = in.find(a)
    val foundSecond = in.find(b)

    (foundFirst, foundSecond) match
      case (-1, _) => false
      case (_, -1) => false
      case (_, _)  => a > b
  }

  val rulesInts =
    rules.map(_.split("|").map(_.toInt).map((a, b) => ruleCheck(a, b)))

  val parsedData = data.map(_.split(",").map(_.toInt).toList)
  val validCount = parsedData.map(ruleCheck).trues
