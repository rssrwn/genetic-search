package geneticsearch.genotype

import geneticsearch.Types.GenotypePair


trait Genotype[T] extends Seq[T] {

    /**
      * Split a single genotype into two new genotypes, left and right
      * @return Pair of genotypes
      */
    def split: GenotypePair[T]

    /**
      * Create a new full genotype from two half genotypes. 'this' acts as the left half.
      * This function should only be called on split genotypes
      * @param that The right half of the new genotype
      * @return A full genotype created from two halves
      */
    def merge(that: Genotype[T]): Genotype[T]

    /**
      * Mutate this particular genotype
      * Default implementation does nothing
      * @param prob Probability of mutating an element of the Seq
      * @return Mutated genotype
      */
    def mutate(prob: Float): Genotype[T] = {
        this
    }

}
