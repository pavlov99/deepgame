
object SameGame {
  def main(args: Array[String]) {
    println("Same Game!")

    val source = scala.io.Source.fromFile(
      "/home/pavlov99/git/deepgame/src/main/scala/SameGameInput3.txt")

    // Convert colors to numbers to match internal representation.
    val substitutionMap = Map[String, Byte](
      "R" -> 1, "B" -> 2, "Y" -> 3, "G" -> 4, "O" -> 5
    )
    val board: Seq[Seq[Byte]] = source.getLines.toSeq.map(
      _.trim.split("").toSeq.map(substitutionMap)
    )
    
    // Convert to internal representation
    val game = new com.github.pavlov99.deepgame.SameGame(
      board.transpose.toList.map(_.toList.reverse)
    )

    source.close()

    println(game)
    println(game.height, game.width)

    Seq((3, 2)).foreach(c => {
      println("Cell: " + c)
      println("Neighbours: " + game.getNeighbours(c))
      println(game.getConnected(c))
      println(game.move(c))
    })
  }
}
