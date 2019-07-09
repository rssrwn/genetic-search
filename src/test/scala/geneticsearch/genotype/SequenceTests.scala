package geneticsearch.genotype

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
        val (left, right) = genotype.split(1)

        val expectedLeftLen = 1
        val expectedRightLen = 4

        val indexZero = 0
        val indexFour = 1

        assertResult(expectedLeftLen)(left.length)
        assertResult(expectedRightLen)(right.length)

        assertResult(indexZero)(left.head)
        assertResult(indexFour)(right.last)
    }

    test("Merging a split Sequence creates an equivalent Sequence") {
        val (left, right) = genotype.split(1)

        val newSeq = left.merge(right)

        assert(genotype.equals(newSeq))
    }

}
