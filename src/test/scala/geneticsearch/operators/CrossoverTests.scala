package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class CrossoverTests extends FunSuite {

    private val pop = new Sequence(Seq(1,2,3,4,5), null, null) ::
            new Sequence(Seq(6,7,8,9,10), null, null) ::
            new Sequence(Seq(11,12,13,14,15), null, null) ::
            new Sequence(Seq(16,17,18,19,20), null, null) :: Nil

    test("fitnessPairs crossover operator crosses neighbouring genotypes") {
        val crossoverOp = Crossover.fitnessPairs[Int](4)
        val crossedPop = crossoverOp(pop)

        val seqs = crossedPop.map(genotype => genotype.asInstanceOf[Sequence[Int]].elems)

        val expectedHead = Seq(1,2,3,4,10)
        val expectedLast = Seq(16,17,18,19,15)

        assertResult(expectedHead)(seqs.head)
        assertResult(expectedLast)(seqs.last)
    }

    test("fitnessPairs crossover operator splits at default value of half length of genotype") {
        val pop = new Sequence(Seq(1,2,3,4,5,6), null, null) ::
                new Sequence(Seq(7,8,9,10), null, null) :: Nil

        val crossoverOp = Crossover.fitnessPairs[Int]()
        val crossedPop = crossoverOp(pop)

        val seqs = crossedPop.map(genotype => genotype.asInstanceOf[Sequence[Int]].elems)

        val expectedHead = Seq(1,2,3,10)
        val expectedLast = Seq(7,8,9,4,5,6)

        assertResult(expectedHead)(seqs.head)
        assertResult(expectedLast)(seqs.last)
    }

}
