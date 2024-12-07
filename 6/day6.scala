import vecxt.all.*
import vecxt.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import scala.collection.mutable
import scala.util.boundary, boundary.break

class Graph {
  // Adjacency list to store the graph
  val adjacencyList =
    mutable.Map.empty[Tuple2[Int, Int], List[Tuple2[Int, Int]]]

  // Add a node to the graph
  def addNode(node: (Int, Int)): Unit = {
    if (!adjacencyList.contains(node)) {
      adjacencyList(node) = List()
    }
  }

  // Add an edge between two nodes
  def addEdge(from: (Int, Int), to: (Int, Int)): Unit = {
    addNode(from)
    addNode(to)
    adjacencyList(from) = to :: adjacencyList(from)
  }

  // Check if the graph contains a cycle
  def isCyclic: Boolean = {
    val visited = scala.collection.mutable.Set[Tuple2[Int, Int]]()
    val recursionStack = scala.collection.mutable.Set[Tuple2[Int, Int]]()

    def dfs(node: (Int, Int)): Boolean = boundary {
      if (recursionStack.contains(node)) break(true) // Cycle detected
      if (visited.contains(node)) break(false) // Already processed

      visited += node
      recursionStack += node

      for (neighbor <- adjacencyList.getOrElse(node, List())) {
        if (dfs(neighbor)) break(true) // Propagate cycle detection
      }

      recursionStack -= node
      false
    }

    boundary {
      for (node <- adjacencyList.keys) {
        if (dfs(node)) break(true) // Exit immediately if a cycle is found
      }
      false // No cycle detected
    }
  }
}

enum Orientation(val nextRow: Row, val nextCol: Col):
  case Up extends Orientation(-1, 0)
  case Right extends Orientation(0, 1)
  case Down extends Orientation(1, 0)
  case Left extends Orientation(0, -1)
  def turn = this match
    case Up    => Right
    case Right => Down
    case Down  => Left
    case Left  => Up

extension (rc: RowCol)
  def move(orient: Orientation): RowCol =
    (rc._1 + orient.nextRow, rc._2 + orient.nextCol)

extension (grid: Matrix[Char])
  def inBounds(row: Row, col: Col): Boolean =
    row >= 0 && row < grid.rows && col >= 0 && col < grid.cols

  def inBounds(rc: RowCol): Boolean = inBounds(rc._1, rc._2)

@main def day6 =
  val grid = Matrix.fromRows(readInputForDay(6).map(_.toCharArray).toArray)

  val diagnosticGrid = Matrix.fromRows(readInputForDay(6).map(_.toCharArray).toArray)

  val guardPos = grid.raw.indexOf('^')
  val startPos = (guardPos % grid.cols: Row, guardPos / grid.cols: Col)

  val iterate = Iterator.unfold[(Row, Col), (Orientation, (Row, Col))](
    Orientation.Up,
    startPos
  ) { case (orient, rc) =>
    // println(s"rc: $rc, orient: $orient")
    val nextSquare = rc.move(orient)

    if (grid.inBounds(nextSquare)) {
      grid(nextSquare) match
        case '#' =>
          Some((rc, (orient.turn, rc)))

        case _ =>
          diagnosticGrid.update(rc, 'X')
          Some((nextSquare, (orient, nextSquare)))
    } else
      diagnosticGrid.update(rc, 'E')
      None // finished
  }

  // val path = iterate.take(3000).toSet
  val path = iterate.takeWhile(grid.inBounds).toSet
  // println(diagnosticGrid.printMat)

  println("part 1")
  println(path.size + 1) // we're missing the last update

  println("Part 2")

  val out =
    for (rowI <- 0 until grid.rows) yield for (colJ <- 0 until grid.cols) yield

      var firstTurn = true
      val checkBlockAt = (rowI, colJ)
      val currentTileAt = grid((rowI, colJ))
      println(s"$checkBlockAt current tile $currentTileAt")
      val out: (Int, Int, Boolean) = currentTileAt match
        case '#' =>
          (
            rowI,
            colJ,
            false
          ) // this is an existing block, so can't add a new cycle.
        case '^' =>
          (
            rowI,
            colJ,
            false
          ) // don't think this is valid.
        case _ =>
          if (!path.contains(checkBlockAt))
            // println("skipped")
            (
              rowI,
              colJ,
              false
            ) // this wasn't on the original happy, path, so we can't add a new cycle asit'll never be hit.
          else
            val newGrid = Matrix(grid.raw.clone(), grid.shape)
            // val diagnosticGrid = Matrix(grid.raw.clone(), grid.shape)
            newGrid.update(checkBlockAt, '#')
            // diagnosticGrid.update(checkBlockAt, '#')

            val newGraph = Graph()
            val iterate =
              Iterator
                .unfold[
                  (Row, Col, Boolean),
                  (Orientation, (Row, Col), (Row, Col))
                ](
                  Orientation.Up,
                  startPos,
                  startPos
                ) { case (orient, currentPosition, previousTurnAt) =>
                  // println("====")
                  // println(diagnosticGrid.printMat)
                  val nextSquare = currentPosition.move(orient)

                  if (newGrid.inBounds(nextSquare) && !newGraph.isCyclic) {
                    newGrid(nextSquare) match
                      case '#' =>
                        // println(s"nextSquare: ${grid(nextSquare)}")
                        // if (currentPosition != previousTurnAt)
                        // println(newGraph.adjacencyList)
                        // println(
                        //   s"adding edge $currentPosition -> $previousTurnAt"
                        // )
                        if (!firstTurn && currentPosition != previousTurnAt)
                          newGraph.addEdge(currentPosition, previousTurnAt)
                        firstTurn = false
                        // if (newGraph.isCyclic)
                        //   println("CYCLE DETECTED")
                        //   println(newGraph.adjacencyList)
                        //   println(diagnosticGrid.printMat)
                        Some(
                          (
                            (currentPosition._1, currentPosition._2, newGraph.isCyclic),
                            (orient.turn, currentPosition, (currentPosition._1, currentPosition._2))
                          )
                        )

                      case _ =>
                        // diagnosticGrid.update(currentPosition, 'X')

                        Some(
                          ((nextSquare._1, nextSquare._2, false), (orient, nextSquare, previousTurnAt))
                        )
                  } else
                    // diagnosticGrid.update(currentPosition, 'E')
                    // println(diagnosticGrid.printMat)
                    None // finished
                }

            val cyclicResult = iterate
              .takeWhile(_ => true)
              .toArray
              .last

            (rowI, colJ, cyclicResult._3)
      out

  println(out.flatten.filter(_._3).length)
