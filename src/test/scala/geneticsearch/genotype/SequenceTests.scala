package geneticsearch.genotype

import geneticsearch.Util.extractSuccess

import org.scalatest.FunSuite


class SequenceTests extends FunSuite {

    private val flipProb = 0.2f

    test("A Sequence has the expected length") {
        val binStr = new BinaryString("00101", flipProb)
        val expectedLength = 5

        assertResult(expectedLength)(binStr.length)
    }

    test("Splitting a BinaryString creates two BinaryStrings correctly") {
        val binStr = new BinaryString("00101", flipProb)
        val (left: BinaryString, right) = extractSuccess(binStr.split(1))

        val expectedLeftLen = 2
        val expectedRightLen = 3

        val indexOne = 0
        val indexFour = 1

        assertResult(expectedLeftLen)(left.length)
        assertResult(expectedRightLen)(right.length)

        assertResult(indexOne)(left.apply(1))
        assertResult(indexFour)(right.apply(2))
    }

    test("Merging a split BinaryString creates an equivalent BinaryString") {
        val binStr = new BinaryString("00101", flipProb)
        val (left, right) = extractSuccess(binStr.split(1))

        val newBinStr = left.merge(right)

        assert(binStr.equals(newBinStr))
    }

    test("Mutating a binaryString should do nothing when flipProb is zero") {
        val binStr = new BinaryString("00101", 0)

        val mutated = binStr.mutate()

        assert(binStr.equals(mutated))
    }

    test("Mutating a binaryString should flip all bits when flipProb is one") {
        val binStr = new BinaryString("00101", 1)

        val mutated = binStr.mutate().asInstanceOf[BinaryString]

        val expected = "11010"

        assertResult(expected)(mutated.str)
    }

    test("Mutating a binaryString flips approx the correct number of bits") {
        val str = "10" * 5000
        val binStr = new BinaryString(str, flipProb)
        val length = binStr.length

        val mutated = binStr.mutate().asInstanceOf[BinaryString]

        val errorRate = 0.02f
        val acceptanceRange = ((flipProb - errorRate) * length, (flipProb + errorRate) * length)

        val numMutations: Float = binStr.zipWithIndex.map { case(elem, idx) =>
            if (elem == mutated(idx)) {
                0.0f
            } else {
                1.0f
            }
        }.sum

        assert(numMutations >= acceptanceRange._1)
        assert(numMutations <= acceptanceRange._2)
    }

}
