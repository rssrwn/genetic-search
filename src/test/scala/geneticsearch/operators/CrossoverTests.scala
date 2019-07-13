package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class CrossoverTests extends FunSuite {

    private val pop = (new Sequence(Seq(1,2,3,4,5), null, null) ::
            new Sequence(Seq(6,7,8,9,10), null, null) ::
            new Sequence(Seq(11,12,13,14,15), null, null) ::
            new Sequence(Seq(16,17,18,19,20), null, null) :: Nil).toVector

    test("fitnessPairs crossover operator crosses neighbouring genotypes") {
        val crossoverOp = Crossover.fitnessPairs[Int](4)
        val crossedPop = crossoverOp(pop)

        val seqs = crossedPop.map(genotype => genotype.elems)

        val expectedHead = Seq(1,2,3,4,10)
        val expectedLast = Seq(16,17,18,19,15)

        assertResult(expectedHead)(seqs.head)
        assertResult(expectedLast)(seqs.last)
    }

    test("fitnessPairs crossover operator splits at default value of half length of genotype") {
        val pop = (new Sequence(Seq(1,2,3,4,5,6), null, null) ::
                new Sequence(Seq(7,8,9,10), null, null) :: Nil).toVector

        val crossoverOp = Crossover.fitnessPairs[Int]()
        val crossedPop = crossoverOp(pop)

        val seqs = crossedPop.map(genotype => genotype.elems)

        val expectedHead = Seq(1,2,3,10)
        val expectedLast = Seq(7,8,9,4,5,6)

        assertResult(expectedHead)(seqs.head)
        assertResult(expectedLast)(seqs.last)
    }

    test("randomPairs crossover operator crosses with only one other genotype") {
        val crossoverOp = Crossover.randomPairs[Int](3)
        val crossedPop = crossoverOp(pop)

        val seqs = crossedPop.map(genotype => genotype.elems)
        val sameStart = startsWith[Int](seqs, Seq(1,2,3))

        val expectedLength = 1

        assertResult(expectedLength)(sameStart.length)
    }

    test("randomPairs crossover operator crosses with other genotypes with equal probability") {
        def pop: Vector[Sequence[Int]] = (new Sequence(Seq(1,2,3,4,5), null, null) ::
                new Sequence(Seq(6,7,8,9,10), null, null) ::
                new Sequence(Seq(11,12,13,14,15), null, null) :: Nil).toVector

        val numPops = 10000
        val pops = Seq.fill(numPops)(pop)
        val crossoverOp = Crossover.randomPairs[Int](3)

        val crossedPops = pops.map(crossoverOp).map { pop =>
            pop.map(genotype => genotype.elems)
        }

        val crossZeroOne = Seq(1,2,3,9,10)
        val crossOneTwo = Seq(6,7,8,14,15)

        val numZeroOne = crossedPops.map { pop =>
            if (pop.contains(crossZeroOne)) 1 else 0
        }.sum
        val zeroOneCrossProb = numZeroOne / numPops.toDouble

        val numOneTwo = crossedPops.map { pop =>
            if (pop.contains(crossOneTwo)) 1 else 0
        }.sum
        val oneTwoCrossProb = numOneTwo / numPops.toDouble

        val error = 0.05
        val expectedProb = (2.0 / 3.0) * (1.0 / 2.0)
        val probRange = (expectedProb * (1 - error), expectedProb * (1 + error))

        assert(zeroOneCrossProb >= probRange._1 && zeroOneCrossProb <= probRange._2)
        assert(oneTwoCrossProb >= probRange._1 && oneTwoCrossProb <= probRange._2)
    }

    /*
    Find all Seq[T] that starts with <start> elems
     */
    private def startsWith[T](seqs: Seq[Seq[T]], start: Seq[T]): Seq[Seq[T]] = {
        val lenStart = start.length
        seqs.flatMap { seq =>
            val seqStart = seq.take(lenStart)
            if (seqStart == start) {
                Some(seq)
            } else {
                None
            }
        }
    }

}
