package geneticsearch.genotype


trait Genotype[T] {

    /**
      * Split a single genotype into two new genotypes, left and right
      * @return Pair of genotypes
      */
    def split: (T, T)

    /**
      * Create a new full genotype from two half genotypes. 'this' acts as the left half.
      * This function should only be called on split genotypes
      * @param that The right half of the new genotype
      * @return A full genotype created from two halves
      */
    def merge(that: T): T

    /**
      * Mutate this particular genotype
      * Default implementation does nothing
      * @return Mutated genotype
      */
    def mutate: Genotype[T] = {
        this
    }

}
