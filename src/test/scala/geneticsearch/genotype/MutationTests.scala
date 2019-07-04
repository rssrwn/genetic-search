package geneticsearch.genotype


class MutationTests {

    // TODO

//    test("Mutating a Sequence should do nothing when flipProb is zero") {
//        val binStr = new BinaryString("00101", 0)
//
//        val mutated = binStr.mutate()
//
//        assert(binStr.equals(mutated))
//    }
//
//    test("Mutating a binaryString should flip all bits when flipProb is one") {
//        val binStr = new BinaryString("00101", 1)
//
//        val mutated = binStr.mutate().asInstanceOf[BinaryString]
//
//        val expected = "11010"
//
//        assertResult(expected)(mutated.str)
//    }
//
//    test("Mutating a binaryString flips approx the correct number of bits") {
//        val str = "10" * 5000
//        val binStr = new BinaryString(str, flipProb)
//        val length = binStr.length
//
//        val mutated = binStr.mutate().asInstanceOf[BinaryString]
//
//        val errorRate = 0.02f
//        val acceptanceRange = ((flipProb - errorRate) * length, (flipProb + errorRate) * length)
//
//        val numMutations: Float = binStr.zipWithIndex.map { case(elem, idx) =>
//            if (elem == mutated(idx)) {
//                0.0f
//            } else {
//                1.0f
//            }
//        }.sum
//
//        assert(numMutations >= acceptanceRange._1)
//        assert(numMutations <= acceptanceRange._2)
//    }

}
