package com.github.pavlov99.deepgame

/** SameGame
 *  State is represented as a n*m board, stored column-wise (it is easier to manipulate).
 *  Each column is indexed from the bottom. So bottom left coordinate is (0, 0).
 *  First coordinate - column from left to right, second - cell from bottom to top.
 *  Each color is represented by scala.Byte, 0 - empty cell, positive - different colors.
 */
class SameGame(state: Seq[Seq[Byte]], moves: Seq[(Int, Int)] = Seq(), val score: Int = 0){

  val bonusScore = 1000
  def width: Int = state.length
  def height: Int = state.head.length
  def isComplete: Boolean = state.forall(_.forall(_ == 0))

  override def toString: String = {
    (
      s"[score: $score]" +: state.transpose.reverse.map(_.mkString(" "))
    ).mkString("\n")
  }

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
    val newScore = score + moveScore + (if (newState(0)(0) == 0) bonusScore else 0)

    new SameGame(newState, moves :+ cell, newScore)
  }

  def getAvailableMovesWithConnected(): Seq[((Int, Int), Set[(Int, Int)])] = {
    var discovered = Set[(Int, Int)]()
    var output = scala.collection.mutable.ListBuffer.empty[((Int, Int), Set[(Int, Int)])]

    for (i <- 0 until width; j <- 0 until height; if state(i)(j) != 0 && !discovered.contains((i, j))) {
      val connectedCells = getConnected((i, j))
      if (connectedCells.size > 1) {
        discovered ++= connectedCells
        output += (((i, j), connectedCells))
      }
    }

    output.toSeq
  }

  // Return sequence of available moves. Different moves might belong to the same
  // component, however, every move is valid.
  def getAvailableRandomMoves(): Seq[(Int, Int)] = {
    for {
      i <- 0 until width;
      j <- 0 until height;
      if state(i)(j) != 0 && getConnectedNeighbours((i, j)).length > 0
    } yield (i, j)
  }

  // Move randomly unless it is possible to make a move. Return game state at the end.
  def solveRandom(): SameGame = {
    val availableMoves = getAvailableRandomMoves()
    if (availableMoves.isEmpty) {
      this
    } else {
      val rand = new scala.util.Random(System.currentTimeMillis())
      val nextMove = availableMoves(rand.nextInt(availableMoves.length))
      move(nextMove).solveRandom()
    }
  }
}
