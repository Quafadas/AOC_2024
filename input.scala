def readInputForDay(day: Int): Iterator[String] = {
  scala.io.Source.fromFile(s"$day/input.txt").getLines()
}

def readTestInputForDay(day: Int): Iterator[String] = {
  scala.io.Source.fromFile(s"$day/testInput.txt").getLines()
}
