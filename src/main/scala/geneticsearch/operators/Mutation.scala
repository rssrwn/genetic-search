package geneticsearch.operators

import geneticsearch.Types.MutationOp
import geneticsearch.genotype.Genotype


/*
Factory for mutation operators
 */
object Mutations {

    def randBitFlip[T](flipProb: Float): MutationOp[T] = {
        genotype: Genotype[T] => {
            genotype.mutate(flipProb)
        }
    }

}
