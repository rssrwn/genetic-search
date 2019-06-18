package geneticsearch.operators

import geneticsearch.Types.{MutationOp, Population}
import geneticsearch.genotype.{BinaryString, Genotype}


/*
Factory for mutation operators
 */
object Mutations {

    // TODO move flipProb into mutate func
    private def randBitFlip[T]: MutationOp[T] = {
        genotype: Genotype[T] => {
            genotype.mutate
        }
    }

    val randBitFlip: MutationOp[BinaryString] = randBitFlip[BinaryString]

}
