import scala.io.StdIn.readInt

object Main extends App {

  def squareNumbersLessThan(n: Int): Seq[Int] = {
    (2 to math.sqrt(n).toInt).map(x => x * x)
  }

  def findUniquePairsFunctional(n: Int): Seq[(Int, Int)] = {
    (1 to n).map(x => (x, n - x)).filter { case (x, y) => x != y }.filter { case (x, y) => y != 0 && x != 0 }
  }

  val n: Int = readInt()

  val squares: Seq[Int] = squareNumbersLessThan(n * 2 -1)

  val allPairs = squares.flatMap(findUniquePairsFunctional)

  lazy val emptyMatrixPairs = createMatrix(n + 1)


  lazy val fullMatrixPairs = allPairs.foldLeft(emptyMatrixPairs) {
    case (agg, (x, y)) =>
      updateMatrixValue(agg, x, y, true)
  }

  printMatrix(fullMatrixPairs)

  val res = findAllChains(fullMatrixPairs).filter(_.size == n)

  println(res)
  println(res.size)

  def createMatrix(n: Int): Array[Array[Boolean]] = {
    Array.tabulate(n, n)((_, _) => false)
  }

  def printMatrix(matrix: Array[Array[Boolean]]): Unit = {
    matrix.foreach(row => println(row.map(if (_) "1" else "0").mkString(" | ")))
  }

  def updateMatrixValue(matrix: Array[Array[Boolean]], row: Int, col: Int, value: Boolean): Array[Array[Boolean]] = {
    val updatedMatrix = matrix.map(_.clone())
    if (row  >= 0 && row < updatedMatrix.length && col >= 0 && col < updatedMatrix(row).length) {
      updatedMatrix(row)(col) = value
    }
    updatedMatrix
  }

  def findTrueInRow(matrix: Array[Array[Boolean]], rowIndex: Int): Seq[(Int, Int)] = {
    if (rowIndex >= 0 && rowIndex < matrix.length) {
      matrix(rowIndex).zipWithIndex.collect {
        case (value, colIndex) if value => (rowIndex, colIndex)
      }
    } else {
      println(s"Row index $rowIndex is out of bounds!")
      Seq.empty
    }
  }

  def findAllChains(matrix: Array[Array[Boolean]]): List[Seq[Int]] = {
    val n = matrix.length

    def loop(cursor: Int, acc: Seq[Int], nums: Seq[Int]): List[Seq[Int]] = {

      val pairs = findTrueInRow(matrix, cursor).map(_._2).filter(num => nums.contains(num)).toList

      if (pairs.nonEmpty) {

        pairs.flatMap { num =>
          loop(num, acc ++ Seq(num), nums.filter(_ != num))
        }

      } else {
        List(acc)
      }

    }

    val fullNums = (1 until n).toList
    fullNums.flatMap(num => loop(num, Seq(num), fullNums.filter(_ != num)))

  }

}
