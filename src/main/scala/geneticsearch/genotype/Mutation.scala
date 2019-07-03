package geneticsearch.genotype

import geneticsearch.Types.MutationFunc

import scala.util.Random


/*
Factory for genotype mutation functions
 */
object Mutation {

    // TODO use BinaryString if created
    def bitFlip(flipProb: Float): MutationFunc[Sequence[Int]] = {
        genotype => {
            val elems = genotype.map(bit => flipBit(bit, flipProb))
            genotype.withElems(elems)
        }
    }

    /*
    Flips bit with probability
    Bit must be either 0 or 1
     */
    private def flipBit(bit: Int, prob: Float): Int = {
        val rand = Random.nextFloat()
        if (rand <= prob) {
            if (bit == 0) {
                1
            } else {
                0
            }
        } else {
            bit
        }
    }

}
