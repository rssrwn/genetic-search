package geneticsearch.operators

import geneticsearch.Types.MutationOp
import geneticsearch.genotype.Genotype


/*
Factory for mutation operators
 */
object Mutation {

    // TODO make mutationOp take a pop
    /**
      * Returns a function which will flip each element of the genotype with <flipProb>
      * @param flipProb Probability with which to flip each element of the genotype
      * @tparam T Type of the elements within the genotype
      * @return Bit flip mutation function
      */
    def randBitFlip[T](flipProb: Float): MutationOp[T] = {
        genotype: Genotype[T] => {
            genotype.mutate(flipProb)
        }
    }

}
