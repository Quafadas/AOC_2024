import vecxt.all.*
import vecxt.RowCol
import vecxt.Row
import vecxt.Col
import pprint._
import vecxt.BoundsCheck.DoBoundsCheck.yes

@main def day4 =

  val data = readInputForDay(4).toArray
  val asMat = Matrix.fromRows(data.map(_.toCharArray))
  val regex = "(?=(XMAS|SAMX))".r

  val rowsXmas = for (i <- 0 until asMat.rows) yield {
    val row = asMat.row(i).mkString("")
    regex.findAllIn(row).length
  }

  val colsXmas = for (i <- 0 until asMat.cols) yield {
    val col = asMat.col(i).mkString("")
    regex.findAllIn(col).length
  }

  val diagCounts =
    for (i <- 0 until asMat.rows - 3) yield {

      for (j <- 0 until asMat.cols - 3) yield {

        val grid = asMat(j to j + 3, i to i + 3)
        val diag1 = grid.diag.mkString
        val xxmas1 = diag1 == "XMAS" || diag1 == "SAMX"
        val diag2 =
          grid.diag(grid.cols - 1, Vertical.Top, Horizontal.Left).mkString
        val xxmas2 = diag2 == "XMAS" || diag2 == "SAMX"
        val out = (if (xxmas2) 1 else 0) + (if (xxmas1) 1 else 0)
        out
      }
    }

  val diagCountsMAS =
    for (i <- 0 until asMat.rows - 2) yield {
      for (j <- 0 until asMat.cols - 2) yield {
        val grid = asMat(j to j + 2, i to i + 2)
        val diag1 = grid.diag.mkString
        val xxmas1 = diag1 == "MAS" || diag1 == "SAM"
        val diag2 =
          grid.diag(grid.cols - 1, Vertical.Top, Horizontal.Left).mkString
        val xxmas2 = diag2 == "MAS" || diag2 == "SAM"
        val out = if (xxmas2 && xxmas1) 1 else 0
        out
      }
    }

  println("Done")

  println(diagCounts.flatten.sum)
  println(rowsXmas.sum)
  println(colsXmas.sum)

  println(diagCounts.flatten.sum + rowsXmas.sum + colsXmas.sum)

  println("part2")
  println(diagCountsMAS.flatten.sum)
