package geneticsearch.genotype

import Mutation.bitFlip

import org.scalatest.FunSuite


class MutationTests extends FunSuite {

    private val binStr = new Sequence[Int](Seq(0,0,1,0,1), null, null)

    test("bitFlip should do nothing when flipProb is zero") {
        val flipFunc = bitFlip(0.0f)

        val mutated = flipFunc(binStr)

        assert(binStr.equals(mutated))
    }

    test("bitFlip should flip all bits when flipProb is one") {
        val flipFunc = bitFlip(1.0f)

        val mutated = flipFunc(binStr)

        val expected = Seq(1,1,0,1,0)

        assertResult(expected)(mutated)
    }

    test("bitFlip flips approx the correct number of bits") {
        val str = "10" * 5000
        val elems = Seq.tabulate(5000)(idx => str(idx).toInt)
        val binStr = new Sequence[Int](elems, null, null)
        val length = binStr.length

        val flipProb = 0.2f
        val flipFunc = bitFlip(flipProb)
        val mutated = flipFunc(binStr)

        val errorRate = 0.02f
        val acceptanceRange = ((flipProb - errorRate) * length, (flipProb + errorRate) * length)

        val numMutations: Int = binStr.zipWithIndex.map { case(elem, idx) =>
            if (elem == mutated(idx)) {
                0
            } else {
                1
            }
        }.sum

        assert(numMutations >= acceptanceRange._1)
        assert(numMutations <= acceptanceRange._2)
    }

}
