package geneticsearch.genotype

import geneticsearch.Types.GenotypePair

import scala.util.Try


trait Genotype[T] extends Seq[T] {

    /**
      * Split a single genotype into two new genotypes, left and right
      * @param index The index to split on (the index is included in the left genotype)
      * @return Pair of genotypes
      */
    def split(index: Int): Try[GenotypePair[T]]

    /**
      * Create a new full genotype from two half genotypes. 'this' acts as the left half.
      * This function should only be called on split genotypes
      * @param that The right half of the new genotype
      * @return A full genotype created from two halves
      */
    def merge(that: Genotype[T]): Genotype[T]

    // TODO remove default behaviour?
    /**
      * Mutate this particular genotype
      * Default implementation does nothing
      * @return Mutated genotype
      */
    def mutate(): Genotype[T] = {
        this
    }

    /**
      * Computes the distance to the genotype passed in
      * @param that Genotype to compare to
      * @return Distance to that genotype
      */
    def distance(that: Genotype[T]): Try[Float]

}
