class Matrix(dimx: Int, dimy: Int, initFunction: Int => Int) {
  val data: Array[Int] = new Array[Int](dimx*dimy) map (initFunction)
  def apply(i: Int, j: Int) = data(i*dimy+j)
  def update(i: Int, j: Int, value: Int) = data(i*dimy+j) = value

  def numRows = dimx
  def numCols = dimy

  override def toString = {
    val str = new StringBuilder("\n")
    for (i <- 0 until dimx) {
      for (j <- 0 until dimy) {
        str.append("%d ".format(data(i*dimy+j)))
      }
      str.append("\n")
    }
    str.toString
  }

  def equals (that: Matrix) = {
    if (dimx != that.numRows)
      false
    else if (dimy != that.numCols)
      false
    else {
      for (i <- 0 until dimx) {
        for (j <- 0 until dimy) {
          if (this(i,j) != that(i,j))
            println("Mismatch at %d,%d : Expected %d, got %d".format(i,j,this(i,j),that(i,j)))
            false
        }
      }
      true
    }
  }
}

object MatMultBlocked {

  def matrixMult(args: Matrix*)(blockSizes: Int*): Matrix = {
    val A: Matrix = args(0)  // M x P
    val B: Matrix = args(1)  // P x N

    // Matrix sizes
    val M = A.numRows
    val P = A.numCols
    val N = B.numCols

    // Block sizes
    val m = blockSizes(0)
    val p = blockSizes(1)
    val n = blockSizes(2)

    // Number of blocks in each dimension
    val bm: Int = M / m
    val bp: Int = P / p
    val bn: Int = N / n 

    val C = new Matrix(M, N, x => 0)  // M x N

    for (blockm <- 0 until M by m) {
      for (blockn <- 0 until N by n) {
        for (blockp <- 0 until P by p) {
          // Perform the block matrix product C(blockm, blockn) += A(blockm, blockp) * B(blockp, blockn)
          for (rowIdx <- blockm until blockm+m) {
            for (colIdx <- blockn until blockn+n) {
              var acc = C(rowIdx, colIdx)
              for (tempIter <- blockp until blockp + p) {
                acc += A(rowIdx, tempIter) * B(tempIter, colIdx)
              }
              C(rowIdx, colIdx) = acc
            }
          }

        }
      }
    }

  C  
  }

  def main(args: Array[String]) = {
    def usage() = {
      println("MatMultBlocked <M> <P> <N> <m> <p> <n>")
      exit(-1)
    }

    if (args.length != 6) {
      usage
    }

    val M= args(0).toInt
    val P = args(1).toInt
    val N = args(2).toInt
    val m = args(3).toInt
    val p = args(4).toInt
    val n = args(5).toInt

    println("Initializing matrix A (%d x %d)".format(M, P))
    val A = new Matrix(M,P, x => scala.util.Random.nextInt)
    println("Initializing matrix B (%d x %d)".format(P, N))
    val B = new Matrix(P,N, x => scala.util.Random.nextInt)
    println("Matrix initialization complete")

   println("Beginning blocked matrix multiply")
    val t1 = System.nanoTime
    val C = matrixMult(A,B)(m, p, n)
    val t2 = System.nanoTime
    val matMultTime = t2 - t1
    println("Blocked matrix multiply complete, time = %d".format((matMultTime))) 
  }
}
