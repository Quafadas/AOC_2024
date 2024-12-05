import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import fastparse.Parsed.Success
import scala.util.matching.Regex

def parseNums[$: P] =
  P(
    "(" ~/ CharIn("0-9").rep.!.map(_.toInt) ~ "," ~ CharIn(
      "0-9"
    ).rep.!.map(_.toInt) ~ ")"
  )

@main def day3(): Unit =
  val lines =
    readInputForDay(3).toArray
      .mkString("")
      .split("mul")
      .map(parse(_, parseNums(using _)))

  println(
    lines
      .filter(_.isSuccess)
      .map { parsed => parsed.get.value._1 * parsed.get.value._2 }
      .sum
  )

  // This solution is so much beter. from scala standard library.

  val mulPattern: Regex = """mul\((\d+),(\d+)\)""".r
  val multiplications = mulPattern
    .findAllIn(readInputForDay(3).mkString(""))
    .collect { case mulPattern(a, b) =>
      a.toInt * b.toInt
    }
    .sum

  println(multiplications)

enum DoDo:
  case Do, Dont

@main def day3_2(): Unit = ???
// println(
//   437 * 864 + 880 * 907 + 215 * 270 + 737 * 510 + 634 * 344 + 528 * 832 + 514 * 817 + 270 * 137 + 787 * 669 + 896 * 940 + 536 * 622
// )
// val startStr = readInputForDay(3).mkString("")

// def doIdx(str: String) = str.indexOf("do()") match
//   case -1 => None
//   case a  => Option(str.drop(a))

// def dontIdx(str: String) = str.indexOf("don't()") match
//   case -1 => None
//   case a  => Option((str.take(a), str.drop(a)))

// val check = doIdx(startStr)

// val firstString = dontIdx(startStr)

// def reduceString(
//     nextString: Option[String],
//     dodo: DoDo,
//     accumulator: Int = 0
// ): Int = {
//   println("+++ NEW ITERATION +++")
//   nextString match {
//     case Some(value) =>
//       dodo match
//         case DoDo.Do =>
//           // from dont => do ignore everything until next dont
//           val next = doIdx(value)
//           println("SKIP PHASE")
//           println(next)
//           println("accumulator: " + accumulator)

//           reduceString(next, DoDo.Dont, accumulator)
//         case DoDo.Dont =>
//           dontIdx(value) match
//             case None => reduceString(None, DoDo.Do, accumulator)
//             case Some(processMe, nextStr) =>
//               val processed = processMe
//                 .split("mul")
//                 .map(parse(_, parseNums(using _)))
//                 .filter(_.isSuccess)
//                 .map { parsed => parsed.get.value._1 * parsed.get.value._2 }
//                 .sum
//               println("PROCESS ME")
//               println(processMe)
//               println(processed)
//               println("NEXT")
//               println(nextStr)
//               println("acc: " + (accumulator + processed))
//               reduceString(Option(nextStr), DoDo.Do, accumulator + processed)
//     case None => accumulator // finished
//   }
// }

// // println("Final result:   " + reduceString(Some(startStr), DoDo.Dont))

// @main def parseLewarn =
//   def printParseResult(name: String, result: Parsed[?]) =
//     println(s"$name")
//     result match
//       case Parsed.Success(value, _) => println(value)
//       case Parsed.Failure(expected, failIndex, extra) =>
//         println(s"$expected ${failIndex} ${extra.trace().longAggregateMsg}")

//   def parse2[$: P] = Start ~ AnyChar.rep.? ~ &("mul")

//   def repoeats[$: P] = (Start | AnyChar) ~ parseA

//   val t2 = "mul(1123,2)+mul(4,5)"
//   val t1 = "amul(1123,2)+mul(4,5)"

//   // val b = parse(t2, parseA(using _))
//   val a = parse(t1, parse2(using _))
//   val c = parse(t2, parse2(using _))

//   printParseResult("a", a)
//   printParseResult("b", c)
