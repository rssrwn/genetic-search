package geneticsearch.genotype

import geneticsearch.Types.MutationFunc
import geneticsearch.Util

import scala.util.Random


/**
  * Factory for genotype mutation functions
  */
object Mutation {

    // TODO add gaussian mutation of each elem of genotype

    /**
      * Returns a function for mutating binary strings by flipping bits
      * @param flipProb probability of each bit being flipped
      * @return Mutation function
      */
    def bitFlip(flipProb: Float): MutationFunc[Sequence[Boolean]] = {
        genotype => {
            val elems = genotype.map { bit =>
                val rand = Random.nextFloat()
                if (rand <= flipProb) {
                    if (bit) {
                        false
                    } else {
                        true
                    }
                } else {
                    bit
                }
            }
            genotype.withElems(elems)
        }
    }

    /**
      * Returns a function for mutating a list of numbers by either adding or subtracting a small amount
      * @param mutateProb The probability of each element being mutated
      * @param mult The amount each element is multiplied by
      * @return Mutation function
      */
    def multiplierMutation(mutateProb: Float, mult: Float): MutationFunc[Sequence[Double]] = {
        genotype => {
            val elems = genotype.map { elem =>
                val rand = Random.nextFloat()
                if (rand <= mutateProb) {
                    val amt = elem * mult
                    val inc = Random.nextFloat() >= 0.05
                    if (inc) {
                        elem + amt
                    } else {
                        elem - amt
                    }
                } else {
                    elem
                }
            }

            genotype.withElems(elems)
        }
    }

    /**
      * Returns a function for mutating a Sequence[Int] by either adding or subtracting one (with equal probability)
      * @param min The ints are kept above or equal to this number
      * @param max The ints are kept below or equal to this number
      * @param mutationProb The probability of each element in the genotype being mutated
      * @return Mutation function
      */
    def incDecMutation(min: Int, max: Int, mutationProb: Float): MutationFunc[Sequence[Int]] = {
        genotype => {
            val newElems = genotype.map { elem =>
                if (Random.nextFloat() <= mutationProb) {
                    incDecMutateElem(elem, min, max)
                } else {
                    elem
                }
            }

            genotype.withElems(newElems)
        }
    }

    /**
      * Returns a function for mutating a Sequence[Int] by either adding or subtracting one (with equal probability)
      * @param min The ints are kept above or equal to this number
      * @param max The ints are kept below or equal to this number
      * @param numToMutate The number of elements to be mutated
      * @return Mutation function
      */
    def incDecMutation(min: Int, max: Int, numToMutate: Int): MutationFunc[Sequence[Int]] = {
        genotype => {
            val idxs = Util.randPick(genotype.indices.toVector, numToMutate)
            val newElems = genotype.zipWithIndex.map { case(elem, idx) =>
                if (idxs.contains(idx)) {
                    incDecMutateElem(elem, min, max)
                } else {
                    elem
                }
            }

            genotype.withElems(newElems)
        }
    }

    private def incDecMutateElem(elem: Int, min: Int, max: Int): Int = {
        if (elem == min) {
            elem + 1
        } else if (elem == max) {
            elem - 1
        } else {
            val inc = Random.nextFloat() >= 0.05
            if (inc) {
                elem + 1
            } else {
                elem - 1
            }
        }
    }

}
