package geneticsearch.algorithm

import geneticsearch.genotype.{Distance, Sequence}
import geneticsearch.operators.{Completion, Fitness, Mutation, Selection}
import org.scalatest.FunSuite

import scala.util.Random


class GeneticAlgorithmTests extends FunSuite {

    /*
    An implementation of the mastermind game, solved by a genetic algorithm
    Assume:
    0 = blue
    1 = red
    2 = green
    3 = purple
    4 = orange
    5 = yellow
     */
    test("GeneticAlgorithm runs and gives the correct answer") {
        val target = new Sequence(Seq(2,0,5,3), null, null)

        val popSize = 25

        val numToSelect = 5
        val numToMutate = 20
        val numColours = 6
        val seqLength = 4
        val mutFunc = geneticsearch.genotype.Mutation.incDecMutation(0, 5, 1)
        val distFunc = Distance.euclideanInt()

        val pop = for (_ <- 0 until popSize) yield new Sequence[Int](randSeq(numColours, seqLength), mutFunc, distFunc)

        val algo = new GeneticAlgorithmBuilder[Int]
                .withMutationOp(Mutation.appendMutatedPop(numToMutate))
                .withSelectionOp(Selection.selectBest(numToSelect))
                .withFitnessOp(Fitness.targetProximity(target))
                .withCompletionOp(Completion.containsTarget(target))
                .withNumIterations(100)
                .build()

        val result = algo.run(pop.toVector)

        val expected = target.elems

        assertResult(expected)(result.head.elems)
    }

    private def randSeq(max: Int, length: Int): Seq[Int] = {
        for (_ <- 0 until length) yield Random.nextInt(max)
    }

}
