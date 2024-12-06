import vecxt.all.*

@main def day5 =
  val (rules, dataWithBLank) =
    readTestInputForDay(5).toList.partition(_.contains("|"))
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

    println("foundFirst: " + foundFirst)
    println(s"foundSecond : $foundSecond")

    println(s"a: $a")
    println(s"b: $b")

    println(s"fixing $in")
    val trimmed = in.trim()
    println(s"trimmed: ${trimmed.length()}")

    val newString =
      if foundFirst > foundSecond then {
        trimmed
          .substring(0, foundSecond)
          .concat(
            trimmed
              .substring(
                foundFirst,
                Math.min(foundFirst + 3, in.length())
              )
          )
          .+(
            trimmed.substring(
              foundSecond,
              foundSecond + 3
            )
          )
          .+(
            if ((foundFirst + 3) <= trimmed.length() - 1) then
              trimmed.substring(foundFirst + 3, trimmed.length())
            else ""
          )
      } else trimmed

    println(s"fixed $in to $newString")
    newString

  }

  val rulesInts = rules.map(_.split("\\|").map(_.trim.toInt))

  val fixRules = rulesInts collect { case Array(a, b) =>
    ruleFix(a, b)
  }

  val theRules =
    rulesInts.collect { case Array(a, b) =>
      ruleCheck(a, b)
    }

  val rulesCheck = data.map { a =>
    theRules.forall(r => r(a))
  }

  val checkedRules = rulesCheck.zip(data)

  val (passed, failed) = checkedRules.partition { _._1 }

  val middle = passed.map { a =>
    val arg = a._2.split(",")
    arg((arg.length + 1) / 2 - 1).toInt
  }

  println(middle)
  println(middle.sum)

  // val rulesWithFix = theRules.zip(fixRules)

  // def fixy(a: String, rule: String => Boolean, fix: String => String): String =
  //   if rule(a) then a
  //   else fix(a)

  // val answer = for ((_, failedCase) <- failed) yield {
  //   val newString = rulesWithFix.foldLeft(failedCase) {
  //     case (acc, (rule, fix)) =>
  //       println(acc)
  //       val out = if (!rule(acc)) then
  //         println("fix")
  //         var tmp: String = acc
  //         var stop = false
  //         while (stop) {
  //           tmp = fixy(acc, rule, fix)
  //           if rule(tmp) then stop = true
  //         }
  //         println(tmp)
  //         tmp
  //       else acc
  //       println(out)
  //       out
  //   }
  //   println(newString)
  //   val arg = newString.split(",")
  //   arg((arg.length + 1) / 2 - 1).toInt
  //   // ruleFix()
  // }

  // println(answer)

  // // val validCount =
  // //   parsedData.map(pages => rulesInts.map(rule => rule(pages))).trues

  // // println(validCount)
