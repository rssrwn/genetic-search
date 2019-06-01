package algorithm

import genotype.Genotype


class GeneticAlgorithm private(population: List[Genotype],
                               numIters: Int,
                               fitnessOp: Genotype => Float,
                               selectionOp: List[(Genotype, Float)] => List[Genotype],
                               crossOverOp: (Genotype, Genotype) => (Genotype, Genotype) = null,
                               mutationOp: Genotype => Genotype = null,
                               mutationProb: Float = 1) {

    def run(): List[Genotype] = {
        var curPop = population
        for (i <- 0 until numIters) {
            curPop = selectionOp(evalPopulation(curPop))
        }
        selectFittest(curPop)
    }

    private def evalPopulation(pop: List[Genotype]): List[(Genotype, Float)] = {
        population.map(genotype => (genotype, fitnessOp(genotype)))
    }

    private def selectFittest(pop: List[Genotype]): List[Genotype] = {
        evalPopulation(pop).fold(null)((g1, g2) => {
            if (g1._2 > g2._2) {
                g1
            } else {
                g2
            }
        })._1
        // TODO
    }

}


