package geneticsearch.operators

import scala.util.Random

import geneticsearch.Types.{MutationOp, Population}


/*
Factory for mutation operators
 */
object Mutation {

    /**
      * Returns a function which will randomly select <numToMutate> genotypes, generate a new genotype by
      * mutating each selected genotype and then append the new genotype to the existing population
      * @param numToMutate Number of genotypes in the population to mutate
      * @param mutationProb Probability of mutating a genotype in the population
      * @tparam T Type of the elements within each genotype
      * @return Mutation operator
      */
    def appendMutatedPop[T](numToMutate: Int, mutationProb: Float): MutationOp[T] = {
        pop: Population[T] => {
            val mutated = pop.map { genotype =>
                if (Random.nextFloat() <= mutationProb) {
                    genotype.mutate()
                } else {
                    genotype
                }
            }

            pop ++ mutated
        }
    }

}
