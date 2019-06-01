package algorithm

import genotype.Genotype


class GeneticAlgorithm[T] private(population: List[Genotype[T]],
                                  numIter: Int,
                                  fitnessOp: Genotype[T] => Float,
                                  selectionOp: List[(Genotype[T], Float)] => List[Genotype[T]],
                                  crossOverOp: (Genotype[T], Genotype[T]) => (Genotype[T], Genotype[T]) = null,
                                  mutationOp: Genotype[T] => Genotype[T] = null,
                                  mutationProb: Float = 1) {

    def run(): List[Genotype[T]] = {
        var curPop = population
        for (_ <- 0 until numIter) {
            curPop = selectionOp(evalPopulation(curPop))
        }
        selectFittest(curPop)
    }

    private def evalPopulation(pop: List[Genotype[T]]): List[(Genotype[T], Float)] = {
        population.map(genotype => (genotype, fitnessOp(genotype)))
    }

    private def selectFittest(pop: List[Genotype[T]]): List[Genotype[T]] = {
        pop.foldLeft((List.empty[Genotype[T]], 0.0))((curr, genotype) => {
            val (fittest, bestFitness) = curr
            val fitness = fitnessOp(genotype)

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
