package geneticsearch.operators

import geneticsearch.Types.{MutationOp, Population}
import geneticsearch.genotype.{BinaryString, Genotype}


/*
Factory for mutation operators
 */
object Mutations {

    def randBitFlip(flipProb: Float): MutationOp[BinaryString] = {
        genotype: Genotype[BinaryString] => {
            genotype.mutate(flipProb)
        }
    }

}
