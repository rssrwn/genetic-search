package algorithm

import genotype.Genotype


class GeneticAlgorithm private(population: List[Genotype],
                               numIter: Int,
                               fitnessOp: Genotype => Float,
                               selectionOp: List[(Genotype, Float)] => List[Genotype],
                               crossOverOp: (Genotype, Genotype) => (Genotype, Genotype) = null,
                               mutationOp: Genotype => Genotype = null,
                               mutationProb: Float = 1) {

    def run(): List[Genotype] = {
        var curPop = population
        for (i <- 0 until numIter) {
            curPop = selectionOp(evalPopulation(curPop))
        }
        selectFittest(curPop)
    }

    private def evalPopulation(pop: List[Genotype]): List[(Genotype, Float)] = {
        population.map(genotype => (genotype, fitnessOp(genotype)))
    }

    private def selectFittest(pop: List[Genotype]): List[Genotype] = {
        pop.foldLeft((List.empty[Genotype], 0.0))((curr, genotype) => {
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


