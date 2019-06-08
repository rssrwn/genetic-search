package genotype

import util.Types.GenotypePair


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

    override def length: Int

    override def apply(idx: Int): T

    override def iterator: Iterator[T]

}
