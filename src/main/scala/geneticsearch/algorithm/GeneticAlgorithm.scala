package geneticsearch.algorithm

import geneticsearch.genotype.Genotype
import geneticsearch.Types.{EvalPopulation, Population}

import util.control.Breaks._


/**
  * A class for running a genetic search
  * The search will stop either when numIters is over or when completionOp returns true, whichever is first
  * @param ops An object for holding the operators the genetic algorithm uses
  * @param numIter Number of iterations of the algorithm
  * @tparam T Type of element within each genotype
  */
class GeneticAlgorithm[T](ops: GeneticAlgorithmOperators[T], numIter: Int) {

    /**
      * Initiate a genetic search on a population
      * @param initPop Initial population
      * @param logging Describes the level of logging: -1 for off, 0 for logging just at start and end, other numbers
      *                will log the same as 0 but print the genotype with highest fitness every <logging> iterations
      * @return The genotype with the highest fitness (or more than one if joint highest)
      */
    def run(initPop: Population[T], logging: Int = 0): Population[T] = {
        var curEvalPop: EvalPopulation[T] = evaluate(initPop)
        var curPop: Population[T] = curEvalPop.map(_._1)

        if (logging >= 0) {
            println("Starting genetic search...")
            println("Current population size is " + curPop.length)
        }

        var completed = false

        breakable {
            for (iter <- 0 until numIter) {
                curEvalPop = select(curEvalPop)

                if (ops.containsCompletionOp) {
                    if (complete(curEvalPop)) {
                        if (logging >= 0) {
                            println("Completed genetic search in " + (iter + 1) + " iterations")
                        }
                        completed = true
                        break
                    }
                }

                curPop = curEvalPop.map(_._1)

                if (ops.containsCrossoverOp) {
                    curPop = crossover(curEvalPop)
                }

                if (ops.containsMutationOp) {
                    curPop = mutate(curPop)
                }

                if (logging > 0 && iter % logging == 0) {
                    val fittest = selectFittest(curPop)
                    println("The following are the fittest genotypes on iteration " + (iter + 1) + ":")
                    for (genotype <- fittest) {
                        println(genotype)
                    }
                }

                curEvalPop = evaluate(curPop)
            }
        }

        val fittest = selectFittest(curPop)

        if (logging >= 0) {
            if (!completed) {
                println("Stopping genetic search due to iteration timeout")
            }

            println("Final population size is " + curPop.length)
            println("The current fittest genotypes are:")
            for (genotype <- fittest) {
                println(genotype)
            }
        }

        fittest
    }

    private def evaluate(pop: Population[T]): EvalPopulation[T] = {
        pop.map { genotype =>
            Executor.submit(genotype, ops.fitnessOp(genotype))
        }.map { future =>
            // This will wait for future to return (possibly forever)
            Executor.await(future)
        }
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

    private def complete(pop: EvalPopulation[T]): Boolean = {
        ops.completionOp(pop)
    }

    private def selectFittest(pop: Population[T]): Population[T] = {
        pop.foldLeft((Set.empty[Genotype[T]], Double.MinValue: Double))((curr, genotype) => {
            val (fittest, bestFitness) = curr
            val fitness = ops.fitnessOp(genotype)

            if (fittest.isEmpty) {
                (fittest + genotype, fitness)
            } else {
                if (fitness > bestFitness) {
                    (Set(genotype), fitness)
                } else if (bestFitness == fitness) {
                    (fittest + genotype, bestFitness)
                } else {
                    curr
                }
            }
        })._1.toVector
    }

}
