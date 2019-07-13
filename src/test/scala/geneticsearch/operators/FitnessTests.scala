package geneticsearch.operators

import geneticsearch.genotype.{Distance, Sequence}
import org.scalatest.FunSuite


class FitnessTests extends FunSuite {

    private val hamming = Distance.hamming()
    private val target = new Sequence[Int](Seq(1,0,1,0,0,1), null, hamming)
    private val fitnessOp = Fitness.targetProximity[Int](target)

    test("targetProximity produces 1 - distance") {
        val genotype = new Sequence[Int](Seq(0,0,1,0,1,1), null, hamming)

        val fitness = fitnessOp(genotype)

        val expectedFitness = -1.0

        assertResult(expectedFitness)(fitness)
    }

}
