package tunable
import scala.util.Random

class Tunable[T](val tunable: List[T], val geneList: List[List[T]]) {
  def apply(x: Int) = { tunable(x) }

  def crossOver(that: Tunable[T]): Tunable[T] = {
    val temp = (tunable) dropRight (Math.abs(Random.nextInt) % (tunable.length) + 1)
    val ret = temp ::: (that.tunable drop temp.length)
    new Tunable[T](ret, geneList)
  }

  def mutate(): Tunable[T] = {
    // Some mutation constants
    val maxMutations = tunable.length

    // Pick number of mutations at random
    var numMutations = Math.abs(Random.nextInt) % maxMutations + 1
    
    var newTunable =  List[T]()
    for (i <- 0 until tunable.length) {
      if (numMutations > 0 && Random.nextInt%2 == 0) {
        numMutations -= 1
        if (geneList.length > 1) { // Multiple sets, use the set at geneList(i) to get replacement
          val replacementList = geneList(i)
          newTunable ::= replacementList(Math.abs(Random.nextInt)%replacementList.length)
        }
        else {
        val replacementList = geneList(0)
          newTunable ::= replacementList(Math.abs(Random.nextInt)%replacementList.length)
        }
      }
      else {
          newTunable ::= tunable(i)
      }
    }
    new Tunable[T](newTunable.reverse, geneList)
  }
  
  override def toString = tunable.toString
}

/** Example usage:
  def factors(n: Int) = {
      (1 to Math.sqrt(n).toInt) filter (n % _ == 0) flatMap { x => List(x,n/x) } sorted
    }                                               //> factors: (n: Int)scala.collection.immutable.IndexedSeq[Int]
    
    def getBlockSizes(M: Int) = {
      (for (i <- factors(M)) yield i).toList
    }                                               //> getBlockSizes: (M: Int)List[Int]
    
    val arr1 = getBlockSizes(64)                    //> arr1  : List[Int] = List(1, 2, 4, 8, 8, 16, 32, 64)
    val arr2 = getBlockSizes(33)                    //> arr2  : List[Int] = List(1, 3, 11, 33)
    val arr3 = getBlockSizes(77)                    //> arr3  : List[Int] = List(1, 7, 11, 77)
    
    val t = new Tunable(List(1,1,1), List(arr1, arr2, arr3))
                                                    //> t  : Tunable.Tunable[Int] = List(1, 1, 1)
   val t1 = t.mutate                                //> t1  : Tunable.Tunable[Int] = List(1, 3, 1)
   val t2 = t.mutate                                //> t2  : Tunable.Tunable[Int] = List(1, 33, 7)
   
   val t3 = t1.crossOver(t2)   
**/
