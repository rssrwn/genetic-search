package geneticsearch.genotype

import org.scalatest.FunSuite


class SequenceTests extends FunSuite {

    val genotype = new Sequence[Boolean](Seq(false, false, true, false, true), null, null)

    test("A Sequence has the expected length") {
        val expectedLength = 5

        assertResult(expectedLength)(genotype.length)
    }

    test("Splitting a Sequence creates two BinaryStrings correctly") {
        val (left, right) = genotype.split(1)

        val expectedLeftLen = 1
        val expectedRightLen = 4

        val indexZero = false
        val indexFour = true

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
