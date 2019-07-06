package geneticsearch.genotype

import org.scalatest.FunSuite


class DistanceTests extends FunSuite {

    private val binStr = new Sequence[Int](Seq(0,0,1,0,1), null, null)

    test("hamming distance between two equal binary strings is zero") {
        val otherStr = new Sequence[Int](Seq(0,0,1,0,1), null, null)

        val hamming = Distance.hamming()

        val dist = hamming(binStr, otherStr)

        val expectedDist = 0

        assertResult(expectedDist)(dist)
    }

    test("hamming distance function gives correct distances") {
        val otherStr = new Sequence[Int](Seq(1,0,1,1,1), null, null)
        val inverseStr = new Sequence[Int](Seq(1,1,0,1,0), null, null)

        val hamming = Distance.hamming()

        val dist1 = hamming(binStr, otherStr)
        val dist2 = hamming(binStr, inverseStr)

        val expectedDist = 2
        val invserseDist = 5

        assertResult(expectedDist)(dist1)
        assertResult(invserseDist)(dist2)
    }

    

}
