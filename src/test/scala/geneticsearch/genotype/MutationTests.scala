package geneticsearch.genotype

import Mutation.{bitFlip, multiplierMutation}
import org.scalatest.FunSuite

import scala.util.Random


class MutationTests extends FunSuite {

    private val binStr = new Sequence[Boolean](Seq(false, false, true, false, true), null, null)

    test("bitFlip should do nothing when flipProb is zero") {
        val flipFunc = bitFlip(0.0f)

        val mutated = flipFunc(binStr)

        assert(binStr.equals(mutated))
    }

    test("bitFlip should flip all bits when flipProb is one") {
        val flipFunc = bitFlip(1.0f)

        val mutated = flipFunc(binStr)

        val expected = Seq(true, true, false, true, false)

        assertResult(expected)(mutated)
    }

    test("bitFlip flips approx the correct number of bits") {
        val length = 5000
        val str = "10" * length
        val elems = Seq.tabulate(5000) { idx =>
            val int = str(idx).toInt
            if (int == 0) {
                false
            } else {
                true
            }
        }

        val binStr = new Sequence[Boolean](elems, null, null)

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

    private val seq = new Sequence[Double](Seq(0.2, 0.45, 0.1, 0.7), null, null)

    test("multiplierMutation should do nothing when mutationProb is zero") {
        val mutFunc = multiplierMutation(0.0f, 0.1f)

        val mutated = mutFunc(seq)

        assert(seq.equals(mutated))
    }

    test("multiplierMutation should change all elems by the correct amount when mutationProb is one") {
        val mult = 0.1f
        val mutFunc = multiplierMutation(1.0f, mult)

        val mutated = mutFunc(seq)

        seq.zip(mutated).map { case(orig, mut) =>
            val diff = orig * mult
            if (mut > orig) {
                assertResult(mut)(orig + diff)
            } else {
                assertResult(mut)(orig - diff)
            }
        }
    }

    test("multiplierMutation should alter approx the correct number of elements") {
        val length = 5000
        val elems = Seq.tabulate(length)(_ => Random.nextDouble())
        val seq = new Sequence[Double](elems, null, null)

        val mutProb = 0.2f
        val mult = 0.1f
        val mutFunc = multiplierMutation(mutProb, mult)
        val mutated = mutFunc(seq)

        val errorRate = 0.02f
        val acceptanceRange = ((mutProb - errorRate) * length, (mutProb + errorRate) * length)

        val numMutations = seq.zip(mutated).map { case(orig, mut) =>
            if (orig == mut) {
                0
            } else {
                1
            }
        }.sum

        assert(numMutations >= acceptanceRange._1)
        assert(numMutations <= acceptanceRange._2)
    }

    private val intSeq = new Sequence[Int](Seq(1,2,3,4,5), null, null)

    test("incDecMutation mutates the correct number of values") {
        val numToMutate = 3
        val mutFunc = Mutation.incDecMutation(0, 5, numToMutate)

        val mutated = mutFunc(intSeq)

        val numMutations = intSeq.zip(mutated).map { case(orig, elem) =>
            if (orig == elem) {
                0
            } else {
                1
            }
        }.sum

        assertResult(numToMutate)(numMutations)
    }

    test("incDecMutation keeps values between min and max") {
        val binSeq = new Sequence[Int](Seq(1,1,0,1,0,1,1,0,0,1), null, null)

        val mutFunc = Mutation.incDecMutation(0, 1, 10)

        val mutated = mutFunc(binSeq)

        for (elem <- mutated) {
            assert(elem == 0 || elem == 1)
        }
    }

    test("incDecMutation should mutate approx the correct number of elements") {
        val min = 0
        val max = 10
        val length = 5000
        val elems = Seq.tabulate(length)(_ => Random.nextInt(max + 1))

        val seq = new Sequence[Int](elems, null, null)

        val mutProb = 0.2f
        val mutFunc = Mutation.incDecMutation(min, max, mutProb)
        val mutated = mutFunc(seq)

        val errorRate = 0.02f
        val acceptanceRange = ((mutProb - errorRate) * length, (mutProb + errorRate) * length)

        val numMutations = seq.zip(mutated).map { case(orig, mut) =>
            if (orig == mut) {
                0
            } else {
                1
            }
        }.sum

        assert(numMutations >= acceptanceRange._1)
        assert(numMutations <= acceptanceRange._2)
    }

}
