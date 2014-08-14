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

object Autotuner {

//  def autotune(f: (Any*, Any*) => Any, f_gold: Any* => Any)(input: Any*)(tunable: Any*): Array[Any] = {
//    var speedup = 0.0
//
//    
//    val t1 = System.nanoTime
//    f(input)(tunable)
//    val t2 = System.nanoTime
//    val score = t2 - t1    
//
//    val t3 = System.nanoTime
//    f_gold(input)
//    val t4 = System.nanoTime
//
//  }

  // Concise function to compute all factors of a number. Will be required to generate 
  // inputs for block sizes given the size of a matrix dimension
  def factors(n: Int) = {
    (1 to Math.sqrt(n).toInt) filter (n % _ == 0) flatMap { x => List(x,n/x) } sorted
  }

  def matrixMult_gold(args: Matrix*): Matrix = {
    val A: Matrix = args(0)  // M x P
    val B: Matrix = args(1)  // P x N

    val M = A.numRows
    val P = A.numCols
    val N = B.numCols
    val C = new Matrix(M, N, x => 0)  // M x N

    for (rowIdx <- 0 until M) {
      for (colIdx <- 0 until N) {
        var acc = 0
        for (iter <- 0 until P) {
          acc += A(rowIdx, iter) * B(iter, colIdx)
        }
        C(rowIdx, colIdx) = acc
      }
    }
    C
  }

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
//        println("Calculating block beginning at C(%d,%d)", blockm, blockn)
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
      println("Autotuner <squareMatrixSize> <blockSize>")
      exit(-1)
    }

    if (args.length != 2) {
      usage
    }

    val matrixSize = args(0).toInt
    val blockSize = args(1).toInt
    println("Initializing matrices (%d x %d)".format(matrixSize, matrixSize))
    val A = new Matrix(matrixSize,matrixSize, x => scala.util.Random.nextInt)
    val B = new Matrix(matrixSize,matrixSize, x => scala.util.Random.nextInt)
    println("Matrix initialization complete")

    val m = blockSize
    val n = blockSize
    val p = blockSize
    println("Beginning matrix multiply, block sizes %d, %d, %d".format(m,n,p))
    val t1 = System.nanoTime
    val C = matrixMult(A,B)(m,n,p)
    val t2 = System.nanoTime
    val matMultTime = t2 - t1
    println("Matrix multiply complete, time = %d".format((matMultTime))) 
 /* 
    println("Verifying results:")
    val t3 = System.nanoTime
    val C_gold = matrixMult_gold(A,B)
    val t4 = System.nanoTime
    val verifyTime = t4 - t3
    println("Verification complete, time = %d".format((verifyTime))) 
    if (C equals C_gold) {
      println("Matrix multiply PASSED")
    }
    else {
      println("Matrix multiply FAILED")
      println("Expected: ")
      println(C_gold)
      println("Got:")
      println(C)
    }

    println("Blocked matrix multiply is faster by %f".format(verifyTime.toDouble/matMultTime))
*/

  }
}
