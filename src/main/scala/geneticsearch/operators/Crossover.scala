package geneticsearch.operators

import geneticsearch.Types.{CrossoverOp, GenotypePair}
import geneticsearch.Util.extractSuccess


/*
Factory for crossover operators
 */
object Crossover {

    // TODO make crossover entire pop? or allow to order pop by fitness
    /**
      * Builds a crossover function which splits at an arbitrary index
      * @param splitIndex index to split at
      * @tparam T Type of the elements within the genotype
      * @return Crossover function
      */
    def onePointCrossover[T](splitIndex: Int): CrossoverOp[T] = {
        (g1, g2) => {
            val (g1Left, g1Right) = extractSuccess(g1.split(splitIndex))
            val (g2Left, g2Right) = extractSuccess(g2.split(splitIndex))

            (g1Left.merge(g2Right), g2Left.merge(g1Right))
        }
    }

    /**
      * Builds a crossover function which splits at the midpoint
      * Returned function enforced that genotypes are the same size
      * @tparam T Type of the elements within the genotype
      * @return Crossover function
      */
    def midPointCrossover[T]: CrossoverOp[T] = {
        (g1, g2) => {
            if (g1.length != g2.length) {
                System.err.println("The lengths of the two genotypes must be the same")
                sys.exit(1)
            }
            val mid = g1.length / 2

            val (g1Left, g1Right) = extractSuccess(g1.split(mid))
            val (g2Left, g2Right) = extractSuccess(g2.split(mid))

            (g1Left.merge(g2Right), g2Left.merge(g1Right))
        }
    }

}
