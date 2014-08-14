
object GeneticSearch {

  def search(population: Array[Array[Int], scoringFunction: Array[Int] => Int): Array[Int] = {
    val numGenerations = 500

    for (i <- 0 until numGenerations) {
      val scores = population map { p => (p -> scoringFunction(p)) }
      val sortedScores = scores.toList.sortBy(_._2)  // Returns a list of population elements sorted by scores, least to highest
      val sortedPop = sortedScores map { x => x._1 }


    }
  }
  def main(args: Array[String]) = {

  }
}
