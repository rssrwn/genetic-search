package algorithm

import genotype.Genotype
import util.Types.Population


class GeneticAlgorithm[T](population: Population[T], numIter: Int, ops: GeneticAlgorithmOperators[T]) {

    def run(): List[Genotype[T]] = {
        var curPop = population
        for (_ <- 0 until numIter) {
            curPop = ops.selectionOp(evalPopulation(curPop))
        }
        selectFittest(curPop)
    }

    private def evalPopulation(pop: List[Genotype[T]]): List[(Genotype[T], Float)] = {
        population.map(genotype => (genotype, ops.fitnessOp(genotype)))
    }

    private def selectFittest(pop: List[Genotype[T]]): List[Genotype[T]] = {
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
