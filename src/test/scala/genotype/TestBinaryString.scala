package genotype

import org.scalatest.FunSuite


class TestBinaryString extends FunSuite {

    test("A BinaryString has the expected length") {
        val binStr = new BinaryString("00101")
        val expectedLength = 5

        assertResult(expectedLength)(binStr.length)
    }

    test("A BinaryString ignores non-binary chars") {
        val binStr = new BinaryString("00a10b1")
        val expectedLength = 5
        val intAtIndexTwo = 1

        assertResult(expectedLength)(binStr.length)
        assertResult(intAtIndexTwo)(binStr.apply(2))
    }

    test("Splitting a BinaryString creates two BinaryStrings correctly") {
        val binStr = new BinaryString("00101")
        val (left, right) = binStr.split

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
        val binStr = new BinaryString("00101")
        val (left, right) = binStr.split

        val newBinStr = left.merge(right)

        assert(binStr.iterator.toList.equals(newBinStr.iterator.toList))
    }

}
