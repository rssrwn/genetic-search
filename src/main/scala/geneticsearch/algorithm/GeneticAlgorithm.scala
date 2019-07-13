package geneticsearch.algorithm

import geneticsearch.genotype.Genotype
import geneticsearch.Types.{EvalPopulation, Population}

import util.control.Breaks._


class GeneticAlgorithm[T](ops: GeneticAlgorithmOperators[T], numIter: Int) {

    def run(pop: Population[T]): Population[T] = {
        var curPop = pop
        for (_ <- 0 until numIter) {
            val evalPop = evaluate(curPop)
            if (ops.containsCompletionOp) {
                val completed = ops.completionOp(evalPop)
                if (completed) break
            }

            val selectedEvalPop = select(evalPop)
            if (ops.containsCrossoverOp) {
                curPop = crossover(selectedEvalPop)
            }

            if (ops.containsMutationOp) {
                curPop = mutate(curPop)
            }
        }

        selectFittest(curPop)
    }

    private def evaluate(pop: Population[T]): EvalPopulation[T] = {
        pop.map(genotype => (genotype, ops.fitnessOp(genotype)))
    }

    private def select(pop: EvalPopulation[T]): EvalPopulation[T] = {
        ops.selectionOp(pop)
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
