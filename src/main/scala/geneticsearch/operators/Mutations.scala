package geneticsearch.operators

import geneticsearch.Types.{MutationOp, Population}
import geneticsearch.genotype.{BinaryString, Genotype}


/*
Factory for mutation operators
 */
object Mutations {

    // TODO move flipProb into mutate func
    val randBitFlip: MutationOp[BinaryString] = {
        genotype: Genotype[BinaryString] => {
            genotype.mutate
        }
    }

}
