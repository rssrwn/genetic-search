package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class MutationTests extends FunSuite {

    private val mutFunc = geneticsearch.genotype.Mutation.multiplierMutation(0.5f, 0.1f)

    private val pop = (new Sequence[Double](Seq(1,2,3,4,5,6,7,8,9,10), mutFunc, null) ::
            new Sequence[Double](Seq(11,12,13,14,15,16,17,18,19,20), mutFunc, null) ::
            new Sequence[Double](Seq(21,22,23,24,25,26,27,28,29,30), mutFunc, null) ::
            new Sequence[Double](Seq(31,32,33,34,35,36,37,38,39,40), mutFunc, null) :: Nil).toVector

    test("appendMutatedPop adds mutations of all genotypes when mutateProb is one") {
        val nonMutatedVecs = pop.map(genotype => genotype.elems)

        val mutateProb = 1.0f
        val mutationOp = Mutation.appendMutatedPop[Double](mutateProb)

        val mutatedPop = mutationOp(pop)

        val vecs = mutatedPop.map(genotype => genotype.elems)
        val numMutated = vecs.map { vec =>
            if (nonMutatedVecs.contains(vec)) {
                0
            } else {
                1
            }
        }.sum

        val expectedLength = 8
        val expectedCount = 4

        assertResult(expectedLength)(mutatedPop.length)
        assertResult(expectedCount)(numMutated)
    }

    test("appendMutatedPop adds the correct number of mutations on average") {
        def pop: Vector[Sequence[Double]] = (new Sequence[Double](Seq(1,2,3,4,5), mutFunc, null) ::
                new Sequence[Double](Seq(6,7,8,9,10), mutFunc, null) ::
                new Sequence[Double](Seq(11,12,13,14,15), mutFunc, null) ::
                new Sequence[Double](Seq(16,17,18,19,20), mutFunc, null) :: Nil).toVector

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

    test("appendMutatedPob adds the correct number of mutations") {
        val nonMutatedVecs = pop.map(genotype => genotype.elems)

        val numToMutate = 3
        val mutationOp = Mutation.appendMutatedPop[Double](numToMutate)

        val mutatedPop = mutationOp(pop)

        val vecs = mutatedPop.map(genotype => genotype.elems)
        val numMutated = vecs.map { vec =>
            if (nonMutatedVecs.contains(vec)) {
                0
            } else {
                1
            }
        }.sum

        val expectedLength = 7

        assertResult(expectedLength)(mutatedPop.length)
        assertResult(numToMutate)(numMutated)
    }

    test("replaceWithMutated replaces all genotypes when mutateProb is one") {
        val mutateProb = 1.0f
        val mutationOp = Mutation.replaceWithMutated[Double](mutateProb)

        val mutatedPop = mutationOp(pop)

        val vecs = mutatedPop.map(genotype => genotype.elems)

        val expectedLength = 4
        val expectedCount = 1

        assertResult(expectedLength)(mutatedPop.length)

        // Assert that each genotype has been mutated and is unique
        for (vec <- vecs) {
            val count = vecs.count(elems => elems == vec)
            assertResult(expectedCount)(count)
        }
    }

    test("replaceWithMutated mutates the correct number on average") {
        def pop: Vector[Sequence[Double]] = (new Sequence[Double](Seq(1,2,3,4,5), mutFunc, null) ::
                new Sequence[Double](Seq(6,7,8,9,10), mutFunc, null) ::
                new Sequence[Double](Seq(11,12,13,14,15), mutFunc, null) :: Nil).toVector

        val nonMutatedVecs = pop.map(genotype => genotype.elems)

        val numPops = 10000
        val pops = Seq.fill(numPops)(pop)
        val mutateProb = 0.2f
        val mutationOp = Mutation.replaceWithMutated[Double](mutateProb)

        val mutatedPops = pops.map(mutationOp)

        val startingPopSize = 3
        val numGenotypes = startingPopSize * numPops
        val numMutations = mutatedPops.map { pop =>
            pop.map { genotype =>
                if (nonMutatedVecs.contains(genotype.elems)) {
                    0
                } else {
                    1
                }
            }.sum
        }.sum

        val error = 0.05f
        val expectedRange = (numGenotypes * (mutateProb - error), numGenotypes * (mutateProb + error))

        assert(numMutations >= expectedRange._1 && numMutations <= expectedRange._2)
    }

    test("replaceWithMutated adds the correct number of mutations") {
        val nonMutatedVecs = pop.map(genotype => genotype.elems)

        val numToMutate = 3
        val mutationOp = Mutation.replaceWithMutated[Double](numToMutate)

        val mutatedPop = mutationOp(pop)

        val vecs = mutatedPop.map(genotype => genotype.elems)
        val numMutated = pop.zip(vecs).map { case(orig, vec) =>
            if (orig.elems == vec) {
                0
            } else {
                1
            }
        }.sum

        val expectedLength = 4

        assertResult(expectedLength)(mutatedPop.length)
        assertResult(numToMutate)(numMutated)
    }

}
