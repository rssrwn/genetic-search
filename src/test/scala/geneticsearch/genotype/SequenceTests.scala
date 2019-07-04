package geneticsearch.genotype

import geneticsearch.Util.extractSuccess

import org.scalatest.FunSuite


class SequenceTests extends FunSuite {

    private val mutFunc = Mutation.bitFlip(0.2f)

    private val distFunc = Distance.euclideanInt()

    val genotype = new Sequence[Int](Seq(0,0,1,0,1), mutFunc, distFunc)

    test("A Sequence has the expected length") {
        val expectedLength = 5

        assertResult(expectedLength)(genotype.length)
    }

    test("Splitting a Sequence creates two BinaryStrings correctly") {
        val (left, right) = extractSuccess(genotype.split(1))

        val expectedLeftLen = 2
        val expectedRightLen = 3

        val indexOne = 0
        val indexFour = 1

        assertResult(expectedLeftLen)(left.length)
        assertResult(expectedRightLen)(right.length)

        assertResult(indexOne)(left.apply(1))
        assertResult(indexFour)(right.apply(2))
    }

    test("Merging a split Sequence creates an equivalent Sequence") {
        val (left, right) = extractSuccess(genotype.split(1))

        val newSeq = left.merge(right)

        assert(genotype.equals(newSeq))
    }

}
