package com.github.pavlov99.deepgame

/** SameGame
 *  State is represented as a n*m board, stored column-wise (it is easier to manipulate).
 *  Each column is indexed from the bottom. So bottom left coordinate is (0, 0).
 *  First coordinate - column from left to right, second - cell from bottom to top.
 *  Each color is represented by scala.Byte, 0 - empty cell, positive - different colors.
 */
class SameGame(state: Seq[Seq[Byte]], moves: Seq[(Int, Int)] = Seq(), score: Int = 0){

  def width: Int = state.length
  def height: Int = state.head.length

  override def toString: String = state.transpose.reverse.map(_.mkString(" ")).mkString("\n")

  // Get connected elements to given cell.
  def getConnected(cell: (Int, Int)): Set[(Int, Int)] = {

    def dfs(cell: (Int, Int), visited: Set[(Int, Int)]): Set[(Int, Int)] =
      if (visited contains cell) {
        visited
      } else {
        getConnectedNeighbours(cell)
          .filterNot(visited.contains)
          .foldLeft(visited + cell)((a, b) => dfs(b, a))
      }

    dfs(cell, Set())
  }

  // Get connected cells to cell (x, y).
  def getNeighbours(cell: (Int, Int)): Seq[(Int, Int)] = {
    val (x, y) = cell

    for {
      (i, j) <- Seq((-1, 0), (1, 0), (0, -1), (0, 1));
      if 0 <= x + i && x + i < width && 0 <= y + j && y + j < height
    } yield (x + i, y + j)
  }

  // Get connected neightbours of the same color.
  def getConnectedNeighbours(cell: (Int, Int)): Seq[(Int, Int)] = {
    val (x, y) = cell
    getNeighbours(cell).filter({case (i, j) => state(i)(j) == state(x)(y)})
  }

  def move(cell: (Int, Int)): SameGame = {
    val connectedCells = getConnected(cell)
    val moveScore = (connectedCells.size - 2) * (connectedCells.size - 2)

    // Remove all of the connected cells to cell, append enought empty cells
    // to corresponding rows. Then check vertical columns: remove empty and append
    // empty columns to the end.
    val newVerticalState: Seq[Seq[Byte]] = state.zipWithIndex.map({
      case (values, i) => values.zipWithIndex.filterNot({
        case (value, j) => connectedCells contains (i, j)
      })
        .map(_._1)  // get value without index
    })
      // Fill empty cells with zeros from the top
      .map(values => values ++ Seq.fill[Byte](height - values.length)(0))
      .filterNot(_(0) == 0)

    val newState: Seq[Seq[Byte]] = newVerticalState ++ Seq.fill[Byte](
      width - newVerticalState.length, height)(0)

    new SameGame(newState, moves :+ cell, score + moveScore)
  }
}
