import tunable._

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

  def autotune[ARGTYPE][RESTYPE](f: (ARGTYPE*)(T*) => RESTYPE, f_gold: ARGTYPE* => RESTYPE)
                                (input: ARGTYPE*)
                                (tunable: Tunable): Unit = {

    // Some autotuning constants
    val numGenerations = 100
    val populationSize = 10
    val invalidScore = -999

    // Population of tunables: map maintains score of each tunable
    var population = Map[T,Int]()
    var populationCache = Set[T]()

    // Initialize population with random 'tunable' parameters
    for (i <- 0 until 10) {
      var tunable = tunableGen
      while (!(populationCache contains tunable)) {
        tunable = tunableGen
      }
      populationCache += tunable
      population += (tunable -> invalidScore)
    }

    // Genetic search for 'numGenerations' generations
    for (gen <- 0 until numGenerations) {
      println("Generation %d".format(gen))

      // Profile run each of the tunables in population
      for (t <- population) {
        var elapsedTimes = List[Long]()
        for (i <- 0 to 5) {
          val t1 = System.nanoTime
          val res = f(input)(t)
          val t2 = System.nanoTime
          val elapsedTime = t2 - t1
          elapsedTimes ::= elapsedTime
        }
        val elapsedMedian = elapsedTimes.sorted.apply(elapsedTimes.length/2)
        population += (t -> elapsedMedian)
      }

      // Get sorted list of tunables by rank
      val popList = population.toList
      val sortedTupleList = popList sortBy { _._2 }
      val sortedTunables = sortedTupleList map { x => x_.1 }

      // Time for deletions - bottom half underperforming tunables just gets dropped
      var newPop = sortedTunables dropRight (sortedTunables.length / 2)

      // We need to make up 10 new members of population
      // - 5 by crossover of the top 10 in random order
      // - 3 by mutations of the top 5 in random order
      // - 2 completely random tunables




    }
    for (m <- factors(M); n <- factors(N); p <- factors(P)) {
      var multTimes = List[Long]()

      // Run matrix multiply 6 (arbitrarily chosen) times, take the median
      
      val matMultMedian = multTimes.sorted.apply(multTimes.length/2)
      perfMap += (matMultMedian -> (m,p,n))
    }

  }


  def main(args: Array[String]) = {
    def usage() = {
      println("Autotuner for block matrix multiplication of C(MxN) = A(MxP) * B(PxN)")
      println("Usage: Autotuner M P N")
      exit(-1)
    }

    if (args.length != 3) {
      usage
    }

    val M = args(0).toInt
    val P = args(1).toInt
    val N = args(2).toInt

    println("Initializing matrix A (%d x %d)".format(M, P))
    val A = new Matrix(M,P, x => scala.util.Random.nextInt)
    println("Initializing matrix B (%d x %d)".format(P, N))
    val B = new Matrix(P,N, x => scala.util.Random.nextInt)
    println("Matrix initialization complete")

    var perfMap = Map[Long, (Int,Int,Int)]()
    for (m <- factors(M); n <- factors(N); p <- factors(P)) {
      var multTimes = List[Long]()

      // Run matrix multiply 6 (arbitrarily chosen) times, take the median
      for (i <- 0 to 5) {
        println("(%d) Beginning matrix multiply, block sizes %d, %d, %d".format(i,m,n,p))
        val t1 = System.nanoTime
        val C = matrixMult(A,B)(m,p,n)
        val t2 = System.nanoTime
        val matMultTime = t2 - t1
        multTimes ::= matMultTime
      }

      val matMultMedian = multTimes.sorted.apply(multTimes.length/2)
      perfMap += (matMultMedian -> (m,p,n))
    }

    println("Best block sizes:")
    println(perfMap min)
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
