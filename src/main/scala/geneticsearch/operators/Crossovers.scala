package geneticsearch.operators

import geneticsearch.Types.{CrossoverOp, GenotypePair}
import geneticsearch.genotype.BinaryString


/*
Factory for crossover operators
 */
object Crossovers {

    // TODO allow to be non-BinaryString genotype
    // AND remove midPoint specific code from splitting and merging BinaryString (take args)
    // Will need to force genotype to extend Seq
    val midPointCrossover: CrossoverOp[BinaryString] = {
        (g1, g2) => {
            val (g1Left, g1Right) = g1.split
            val (g2Left, g2Right) = g2.split

            (g1Left.merge(g2Right), g2Left.merge(g1Right))
        }
    }

}
