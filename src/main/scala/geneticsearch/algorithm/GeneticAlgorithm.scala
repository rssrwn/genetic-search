package geneticsearch.algorithm

import geneticsearch.genotype.Genotype
import geneticsearch.Types.{EvalPopulation, Population}


class GeneticAlgorithm[T](ops: GeneticAlgorithmOperators[T], numIter: Int) {

    def run(pop: Population[T]): Population[T] = {
        var curPop = pop
        for (_ <- 0 until numIter) {
            val evalPop = select(curPop)
            if (ops.containsCrossoverOp) {
                curPop = crossover(evalPop)
            }
            if (ops.containsMutationOp) {
                curPop = mutate(curPop)
            }
        }
        selectFittest(curPop)
    }

    private def select(pop: Population[T]): EvalPopulation[T] = {
        val eval = pop.map(genotype => (genotype, ops.fitnessOp(genotype)))
        ops.selectionOp(eval)
    }

    private def crossover(pop: EvalPopulation[T]): Population[T] = {
        val sortedPop = pop.sortWith(_._2 > _._2).map(_._1)
        ops.crossoverOp(sortedPop)
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
        })._1.toVector
    }

}
