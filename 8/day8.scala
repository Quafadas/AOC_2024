import vecxt.all.*

import pprint.*

import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.{Row, Col, RowCol}

import scala.collection.mutable
import scala.collection.mutable.HashMap

extension (rc: RowCol)
  def moveDelta(delta: RowCol): RowCol =
    (rc._1 + delta._1, rc._2 + delta._2)

@main def day8 =

  enum ProjectionMethod:
    case Single, Coninuous
    def project(a: RowCol, b: RowCol, grid: Matrix[Char]): List[RowCol] =
      this match
        case Single =>
          val proj1 = a.moveDelta((a._1 - b._1, a._2 - b._2))
          val proj2 = b.moveDelta((-a._1 + b._1, -a._2 + b._2))
          List(proj1, proj2).filter(grid.inBounds)
        case Coninuous =>
          val proj1 = (a._1 - b._1, a._2 - b._2)
          val proj2 = (-a._1 + b._1, -a._2 + b._2)
          val direction1 = Iterator
            .unfold[RowCol, RowCol](a) { rc =>
              val newRC = rc.moveDelta(proj1)
              if grid.inBounds(newRC) then Some(newRC, newRC)
              else None
            }
            .toList
            .filter(grid.inBounds)

          val direction2 = Iterator
            .unfold[RowCol, RowCol](b) { rc =>
              val newRC = rc.moveDelta(proj2)
              if grid.inBounds(newRC) then Some(newRC, newRC)
              else None
            }
            .toList
            .filter(grid.inBounds)

          (direction1 ++ direction2 :+ a :+ b).toList

  @scala.annotation.tailrec
  def accumulateMatrixTailRecursive(
      matrix: Matrix[Char],
      criteria: Char => Boolean,
      result: mutable.HashMap[Char, List[RowCol]] = mutable.HashMap.empty,
      row: Int = 0,
      col: Int = 0
  ): mutable.HashMap[Char, List[RowCol]] = {
    // Base case: stop when the row exceeds the matrix bounds
    if (row >= matrix.rows) return result

    // Check the element and accumulate if it meets the criteria
    val value = matrix((row, col))
    if (criteria(value)) {
      val current = result.getOrElse(value, Nil)
      result(value) = (row, col) :: current
    }

    // Determine the next indices
    if (col + 1 < matrix.cols) {
      accumulateMatrixTailRecursive(matrix, criteria, result, row, col + 1) // Tail position
    } else {
      accumulateMatrixTailRecursive(matrix, criteria, result, row + 1, 0) // Tail position
    }
  }

  val m = Matrix.fromRows(readInputForDay(8).map(_.toCharArray()).toArray)

  val chars = m.raw.toSet.filterNot(_ == '.')

  val charMap = accumulateMatrixTailRecursive(m, chars.contains)

  def projections(charMap: HashMap[Char, List[RowCol]], grid: Matrix[Char], projectionMethod: ProjectionMethod) = charMap.map { case (char, coords) =>
    val charProj = coords.combinations(2).foldLeft(scala.collection.mutable.ListBuffer.empty[RowCol]) { (acc, list2) =>
      list2 match
        case a :: b :: Nil =>
          acc.append(projectionMethod.project(a, b, grid)*)
        case _ => ???
    }
    (char, charProj.toList)
  }

  val proj = projections(charMap, m, ProjectionMethod.Single)
  val projContinuous = projections(charMap, m, ProjectionMethod.Coninuous)

  // pprintln(m.shape)
  // pprintln(m.printMat)

  // pprintln(chars)

  pprintln(projContinuous)
  println(proj.values.toList.toSet.flatten.size)
  println(projContinuous.values.toList.toSet.flatten.size)
