package geneticsearch.operators

import geneticsearch.Types.{CrossoverOp, GenotypePair, Population}
import geneticsearch.genotype.Genotype

import scala.collection.mutable.ListBuffer
import scala.util.Random


/*
Factory for crossover operators
 */
object Crossover {

    /**
      * Returns a crossover operator which chooses random pairs all of the genotypes
      * then splits each in half and crosses-over within each pair
      * An odd number of genotypes will result in one genotype left unchanged
      * @param splitIdx The index of each genotype at which to split
      * @tparam T Type of the elements within each genotype
      * @return Crossover operator
      */
    def randomPairs[T](splitIdx: Int = -1): CrossoverOp[T] = {
        pop: Population[T] => {
            val shuffle = shufflePop[T](pop)
            crossoverPop(shuffle, splitIdx)
        }
    }

    /**
      * Returns a crossover operator which pairs genotypes with their neighbours
      * If genotypes are passed in by ranking on fitness they will be paired with genotypes with similar fitness
      * An odd number of genotypes will result in one genotype left unchanged
      * @param splitIdx The index of each genotype at which to split
      * @tparam T Type of the elements within each genotype
      * @return Crossover operator
      */
    def fitnessPairs[T](splitIdx: Int = -1): CrossoverOp[T] = {
        pop: Population[T] => {
            crossoverPop(pop, splitIdx)
        }
    }

    /*
    Crossover a population by matching genotypes with their neighbours
    If <splitIdx> is -1 use mid point of left genotype in pair as split index
     */
    private def crossoverPop[T](pop: Population[T], splitIdx: Int): Population[T] = {
        val popIdx = pop.zipWithIndex
        val newPop = new ListBuffer[Genotype[T]]()
        for ((elem, i) <- popIdx) {
            val oddLength = pop.length % 2 == 1
            if (oddLength && pop.length == (i + 1)) {
                newPop += elem
            } else {
                if (i % 2 == 0) {
                    val nextElem = pop(i+1)
                    val (cross1, cross2) = crossover[T](elem, nextElem, splitIdx)
                    newPop += cross1
                    newPop += cross2
                }
            }
        }

        newPop.toVector
    }

    /*
    Shuffles the original pop by shuffling the indices and converting back to original elems
     */
    private def shufflePop[T](pop: Population[T]): Population[T] = {
        val indices = pop.map(elem => pop.indexOf(elem))
        val shuffle = Random.shuffle(indices)
        shuffle.map(idx => pop(idx))
    }

    /*
    Splits two genotypes and merges with the other
    If <splitIdx> is -1 use middle point of first genotype to split at
     */
    private def crossover[T](g1: Genotype[T], g2: Genotype[T], splitIdx: Int): GenotypePair[T] = {
        val split = if (splitIdx == -1) {
            g1.length / 2
        } else {
            splitIdx
        }

        val (g1Left, g1Right) = g1.split(split)
        val (g2Left, g2Right) = g2.split(split)

        (g1Left.merge(g2Right), g2Left.merge(g1Right))
    }

}
