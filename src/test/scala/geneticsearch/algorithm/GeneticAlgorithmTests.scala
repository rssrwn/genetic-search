package geneticsearch.algorithm

import geneticsearch.Types.MutationFunc
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
        val mutFunc = intMutation(0, 5)
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

    private def intMutation(min: Int, max: Int): MutationFunc[Sequence[Int]] = {
        genotype => {
            val elems = genotype.elems
            val length = elems.length
            val rand = Random.nextInt(length)
            val newElems = elems.zipWithIndex.map { case(elem, idx) =>
                if (idx == rand) {
                    if (elem == min) {
                        elem + 1
                    } else if (elem == max) {
                        elem - 1
                    } else {
                        val inc = Random.nextFloat() >= 0.05
                        if (inc) {
                            elem + 1
                        } else {
                            elem - 1
                        }
                    }
                } else {
                    elem
                }
            }

            genotype.withElems(newElems)
        }
    }

}
