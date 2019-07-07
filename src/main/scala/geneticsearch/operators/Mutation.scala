package geneticsearch.operators

import scala.util.Random
import geneticsearch.Types.{EvalPopulation, MutationOp, Population}


/*
Factory for mutation operators
 */
object Mutation {

    /**
      * Returns a function which will choose to randomly mutate genotypes with probability <mutationProb>
      * Mutated genotypes are appended to the existing population
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

    /** Returns a function which will choose <numToMutate> genotypes randomly and mutate them
      *  Mutated genotypes are appended to the existing population
      * @param numToMutate Number of genotypes to be mutated (if greater than size of pop, all genotypes will be mutated)
      * @tparam T Type of the elements within each genotype
      * @return Mutation operator
      */
    def appendMutatedPop[T](numToMutate: Int): MutationOp[T] = {
        pop: Population[T] => {
            val idxs = for (_ <- 0 until numToMutate) yield Random.nextInt(numToMutate - 1)
            val mutated = pop.zipWithIndex.flatMap { case(genotype, idx) =>
                if (idxs.contains(idx)) {
                    Some(genotype.mutate())
                } else {
                    None
                }
            }

            pop ++ mutated
        }
    }

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

    /** Returns a function which will choose <numToMutate> genotypes randomly and mutate them
      * Mutated genotypes replace their initial genotypes in the new population
      * @param numToMutate Number of genotypes to be mutated (if greater than size of pop, all genotypes will be mutated)
      * @tparam T Type of the elements within each genotype
      * @return Mutation operator
      */
    def replaceWithMutated[T](numToMutate: Int): MutationOp[T] = {
        pop: Population[T] => {
            val idxs = for (_ <- 0 until numToMutate) yield Random.nextInt(numToMutate - 1)
            pop.zipWithIndex.map { case(genotype, idx) =>
                if (idxs.contains(idx)) {
                    genotype.mutate()
                } else {
                    genotype
                }
            }
        }
    }

}
