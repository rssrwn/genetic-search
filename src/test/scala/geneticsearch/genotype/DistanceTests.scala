package geneticsearch.genotype

import org.scalatest.FunSuite


class DistanceTests extends FunSuite {

    private val binStr = new Sequence[Int](Seq(0,0,1,0,1), null, null)

    private val testSeqFloat = new Sequence[Double](Seq(2.2, 7.1, -5, 2.5), null, null)

    private val testSeqInt = new Sequence[Int](Seq(2, 7, -5, 3, -9), null, null)

    test("hamming distance between two equal binary strings is zero") {
        val equivStr = new Sequence[Int](Seq(0,0,1,0,1), null, null)

        val hamming = Distance.hamming()

        val dist = hamming(binStr, equivStr)

        val expectedDist = 0

        assertResult(expectedDist)(dist)
    }

    test("hamming distance function gives expected distances") {
        val otherStr = new Sequence[Int](Seq(1,0,1,1,1), null, null)
        val inverseStr = new Sequence[Int](Seq(1,1,0,1,0), null, null)

        val hamming = Distance.hamming()

        val dist1 = hamming(binStr, otherStr)
        val dist2 = hamming(binStr, inverseStr)

        val expectedDist = 2
        val expInvserseDist = 5

        assertResult(expectedDist)(dist1)
        assertResult(expInvserseDist)(dist2)
    }

    test("euclidean distance is zero for two identical Sequences") {
        val equivSeqFloat = new Sequence[Double](Seq(2.2, 7.1, -5, 2.5), null, null)
        val equivSeqInt = new Sequence[Int](Seq(2, 7, -5, 3, -9), null, null)

        val euclideanFloat = Distance.euclideanFloat()
        val euclideanInt = Distance.euclideanInt()

        val distFloat = euclideanFloat(testSeqFloat, equivSeqFloat)
        val distInt = euclideanInt(testSeqInt, equivSeqInt)

        val expectedDist = 0

        assertResult(expectedDist)(distFloat)
        assertResult(expectedDist)(distInt)
    }

    test("euclidean distance gives expected distances") {
        val equivSeqFloat = new Sequence[Double](Seq(1.2, 7.5, 3, 5.5), null, null)
        val equivSeqInt = new Sequence[Int](Seq(5, -1, -7, 1, -4), null, null)

        val euclideanFloat = Distance.euclideanFloat()
        val euclideanInt = Distance.euclideanInt()

        val distFloat = euclideanFloat(testSeqFloat, equivSeqFloat)
        val distInt = euclideanInt(testSeqInt, equivSeqInt)

        val expectedDistFloat = 8.611619949
        val expectedDistInt = 10.295630141

        val epsilon = 0.0001
        assert(floatEqual(expectedDistFloat, distFloat, epsilon))
        assert(floatEqual(expectedDistInt, distInt, epsilon))
    }

    private def floatEqual(num1: Double, num2: Double, epsilon: Double): Boolean = {
        val diff = Math.abs(num1 - num2)
        diff <= epsilon
    }

}
