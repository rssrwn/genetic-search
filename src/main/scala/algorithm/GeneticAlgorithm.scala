package algorithm

import genotype.Genotype
import util.Types.Population


class GeneticAlgorithm[T](population: Population[T], numIter: Int, ops: GeneticAlgorithmOperators[T]) {

    def run(): Population[T] = {
        var curPop = population
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
        val eval = population.map(genotype => (genotype, ops.fitnessOp(genotype)))
        ops.selectionOp(eval)
    }

    private def crossover(pop: Population[T]): Population[T] = {

    }

    private def mutate(pop: Population[T]): Population[T] = {
        val r = scala.util.Random
        pop.map(genotype => {
            if (r.nextFloat() <= ops.mutationProb) {
                ops.mutationOp(genotype)
            } else {
                genotype
            }
        })
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
