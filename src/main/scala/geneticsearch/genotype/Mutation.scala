package geneticsearch.genotype

import geneticsearch.Types.MutationFunc

import scala.util.Random


/*
Factory for genotype mutation functions
 */
object Mutation {

    // TODO use BinaryString if created
    // TODO use seq of bool for bin str?
    /*
    Returns a function for mutating binary strings by flipping bits with probability <flipProb>
     */
    def bitFlip(flipProb: Float): MutationFunc[Sequence[Int]] = {
        genotype => {
            val elems = genotype.map { bit =>
                val rand = Random.nextFloat()
                if (rand <= flipProb) {
                    if (bit == 0) {
                        1
                    } else {
                        0
                    }
                } else {
                    bit
                }
            }
            genotype.withElems(elems)
        }
    }

    /*
    Returns a function for mutating a list of numbers by either adding or subtracting a small amount
    The amount is the element multiplied by <mult>
    Each element is mutated with probability <mutateProb>
     */
    def mutateDouble(mutateProb: Float, mult: Float): MutationFunc[Sequence[Double]] = {
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

}
