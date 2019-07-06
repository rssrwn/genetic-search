package geneticsearch.operators

import scala.util.Random

import geneticsearch.Types.{MutationOp, Population}


/*
Factory for mutation operators
 */
object Mutation {

    /**
      * Returns a function which will choose to randomly mutate genotypes with proability <mutationProb>
      * Mutated genotypes are then appended to the existing population
      * @param mutationProb Probability of mutating a genotype in the population
      * @tparam T Type of the elements within each genotype
      * @return Mutation operator
      */
    def appendMutatedPop[T](mutationProb: Float): MutationOp[T] = {
        pop: Population[T] => {
            val mutated = pop.flatMap { genotype =>
                if (Random.nextFloat() <= mutationProb) {
                    Some(genotype.mutate())
                } else {
                    None
                }
            }

            pop ++ mutated
        }
    }

//    def appendMutatedPop[T](numToMutate: Int): MutationOp[T] = {
//        pop: Population[T] => {
//
//        }
//    }

    /** Returns a function which will choose to randomly mutate genotypes with probability <mutationProb>
      * Mutated genotypes replace their initial genotypes in the new population
      * @param mutationProb Probability of mutating a genotype in the population
      * @tparam T Type of the elements within each genotype
      * @return Mutation operator
      */
    def replaceWithMutated[T](mutationProb: Float): MutationOp[T] = {
        pop: Population[T] => {
            pop.map { genotype =>
                if (Random.nextFloat() <= mutationProb) {
                    genotype.mutate()
                } else {
                    genotype
                }
            }
        }
    }

}
