package geneticsearch.algorithm

import scala.util.Random
import scala.collection.mutable.ListBuffer
import geneticsearch.genotype.Genotype
import geneticsearch.Types.Population


class GeneticAlgorithm[T](ops: GeneticAlgorithmOperators[T], numIter: Int) {

    def run(pop: Population[T]): Population[T] = {
        var curPop = pop
        for (_ <- 0 until numIter) {
            curPop = select(curPop)
            if (ops.containsCrossoverOp) {
                curPop = crossover(curPop)
            }
            if (ops.containsMutationOp) {
                curPop = mutate(curPop)
            }
        }
        selectFittest(curPop)
    }

    private def select(pop: Population[T]): Population[T] = {
        val eval = pop.map(genotype => (genotype, ops.fitnessOp(genotype)))
        ops.selectionOp(eval)
    }

    /**
      * Pairs all of the genotypes then splits each in half and crosses-over within each pair
      * An odd number of genotypes will result in one genotype left unchanged
      * @param pop The population
      * @return The new population with cross-over applied
      */
    private def crossover(pop: Population[T]): Population[T] = {
        val shuffle = shufflePop(pop).zipWithIndex.toVector
        val len = shuffle.length

        val newPop = new ListBuffer[Genotype[T]]()
        for ((elem, i) <- shuffle) {
            if (len == i) {
                newPop += elem
            } else {
                if (i % 2 == 0) {
                    val nextElem = shuffle(i+1)._1
                    val (cross1, cross2) = ops.crossoverOp(elem, nextElem)
                    newPop += cross1
                    newPop += cross2
                }
            }
        }

        newPop.toList
    }

    /*
    Shuffles the original pop by shuffling the indices and converting back to original elems
     */
    private def shufflePop(pop: Population[T]): Population[T] = {
        val vec = pop.toVector
        val indices = vec.map(elem => vec.indexOf(elem))
        val shuffle = Random.shuffle(indices)
        shuffle.map(idx => vec(idx)).toList
    }

    private def mutate(pop: Population[T]): Population[T] = {
        ops.mutationOp(pop)
    }

    private def selectFittest(pop: Population[T]): Population[T] = {
        pop.foldLeft((List.empty[Genotype[T]], 0.0))((curr, genotype) => {
            val (fittest, bestFitness) = curr
            val fitness = ops.fitnessOp(genotype)

            if (fittest.isEmpty) {
                (genotype :: fittest, fitness)
            } else {
                if (fitness > bestFitness) {
                    (genotype :: Nil, fitness)
                } else if (bestFitness == fitness) {
                    (genotype :: fittest, bestFitness)
                } else {
                    curr
                }
            }
        })._1
    }

}
