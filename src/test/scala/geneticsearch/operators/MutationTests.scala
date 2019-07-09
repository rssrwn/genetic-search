package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class MutationTests extends FunSuite {

    private val mutFunc = geneticsearch.genotype.Mutation.multiplierMutation(1.0f, 0.1f)

    private val pop = new Sequence[Double](Seq(1,2,3,4,5), mutFunc, null) ::
            new Sequence[Double](Seq(6,7,8,9,10), mutFunc, null) ::
            new Sequence[Double](Seq(11,12,13,14,15), mutFunc, null) ::
            new Sequence[Double](Seq(16,17,18,19,20), mutFunc, null) :: Nil

    test("appendMutated adds mutations of all genotypes when mutateProb is one") {
        val mutateProb = 1.0f
        val mutationOp = Mutation.appendMutatedPop[Double](mutateProb)

        val mutatedPop = mutationOp(pop)

        val vecs = mutatedPop.map(genotype => genotype.elems)

        val expectedLength = 8
        val expectedCount = 1

        assertResult(expectedLength)(mutatedPop.length)

        // Assert that each genotype has been mutated
        for (vec <- vecs) {
            val count = vecs.count(elems => elems == vec)
            assertResult(expectedCount)(count)
        }
    }

    test("appendMutate adds the correct number of mutations on average") {
        def pop: List[Sequence[Double]] = new Sequence[Double](Seq(1,2,3,4,5), mutFunc, null) ::
                new Sequence[Double](Seq(6,7,8,9,10), mutFunc, null) ::
                new Sequence[Double](Seq(11,12,13,14,15), mutFunc, null) ::
                new Sequence[Double](Seq(16,17,18,19,20), mutFunc, null) :: Nil

        val numPops = 10000
        val pops = Seq.fill(numPops)(pop)
        val mutateProb = 0.2f
        val mutationOp = Mutation.appendMutatedPop[Double](mutateProb)

        val mutatedPops = pops.map(mutationOp)

        val startingPopSize = 4
        val numGenotypes = startingPopSize * numPops
        val numMutations = mutatedPops.map { pop =>
            pop.length - startingPopSize
        }.sum

        val error = 0.05f
        val expectedRange = (numGenotypes * (mutateProb - error), numGenotypes * (mutateProb + error))

        assert(numMutations >= expectedRange._1 && numMutations <= expectedRange._2)
    }

}
