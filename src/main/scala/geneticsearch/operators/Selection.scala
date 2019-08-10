package geneticsearch.operators

import geneticsearch.Types.SelectionOp
import geneticsearch.genotype.Genotype

import scala.util.Random


object Selection {

    /**
      * Returns a function which takes the top <numToSelect> genotypes from the population
      *
      * @param numToSelect The number of genotypes to be selected
      * @tparam T Type of the elements within the genotype
      * @return Selection operator
      */
    def selectBest[T](numToSelect: Int): SelectionOp[T] = {
        evalPop => {
            evalPop.sortWith(_._2 > _._2)
                    .take(numToSelect)
        }
    }

    /**
      * Returns a function which selects genotypes based on a tournament.
      * For each genotype to be selected two genotypes are chosen at random from current pop
      * and the genotype with higher fitness is added to the next pop
      *
      * @param numToSelect Number of genotypes to be selected
      * @param elitism Number of times the fittest genotype is guaranteed to be in the next pop (max <numToSelect>)
      * @tparam T Type of the elements within the genotype
      * @return Selection operator
      */
    def tournament[T](numToSelect: Int, elitism: Int = 0): SelectionOp[T] = {
        evalPop => {
            val popSize = evalPop.length
            val elitismToSelect = if (elitism >= numToSelect) {
                numToSelect
            } else {
                elitism
            }

            val initPop = if (elitismToSelect > 0) {
                val bestElem = evalPop.sortWith(_._2 > _._2).head
                (for (_ <- 0 until elitismToSelect) yield bestElem).toVector
            } else {
                Vector.empty[(Genotype[T], Double)]
            }

            (0 until (numToSelect - elitismToSelect)).foldLeft(initPop) { case (pop, _) =>
                val elem1 = evalPop(Random.nextInt(popSize))
                val elem2 = evalPop(Random.nextInt(popSize))

                if (elem1._2 > elem2._2) {
                    pop :+ elem1
                } else {
                    pop :+ elem2
                }
            }
        }
    }

}
