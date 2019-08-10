package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class CompletionTests extends FunSuite {

    private val pop = (new Sequence[Int](Seq(1,2,3), null, null) ::
            new Sequence[Int](Seq(11,12,13), null, null) ::
            new Sequence[Int](Seq(21,22,23), null, null) :: Nil).toVector

    private val target = new Sequence[Int](Seq(1,2,3), null, null)
    private val containsTarget = Completion.containsTarget(target)

    test("containsTarget returns true when pop contains target genotype") {
        val eval = Seq(1.0,2.0,3.0)
        val evalPop = pop.zip(eval)

        val complete = containsTarget(evalPop)

        val expectedComplete = true

        assertResult(expectedComplete)(complete)
    }

    test("containsTarget returns false when pop does not contain target") {
        val pop = (new Sequence[Int](Seq(6,7,8), null, null) ::
                new Sequence[Int](Seq(11,12,13), null, null) ::
                new Sequence[Int](Seq(21,22,23), null, null) :: Nil).toVector

        val eval = Seq(1.0,2.0,3.0)
        val evalPop = pop.zip(eval)

        val complete = containsTarget(evalPop)

        val expectedComplete = false

        assertResult(expectedComplete)(complete)
    }

    test("scoreThreshold returns true when pop contains genotype with fitness > threshold") {
        val eval = Seq(1.0,2.0,3.0)
        val evalPop = pop.zip(eval)

        val threshold = 2.5
        val scoreThreshold = Completion.scoreThreshold[Int](threshold)

        val complete = scoreThreshold(evalPop)

        val expected = true

        assertResult(expected)(complete)
    }

    test("scoreThreshold returns true when pop contains genotype with fitness == threshold") {
        val eval = Seq(1.0,2.0,3.0)
        val evalPop = pop.zip(eval)

        val threshold = 3.0
        val scoreThreshold = Completion.scoreThreshold[Int](threshold)

        val complete = scoreThreshold(evalPop)

        val expected = true

        assertResult(expected)(complete)
    }

    test("scoreThreshold returns false when pop only contains genotype with fitness < threshold") {
        val eval = Seq(1.0,2.0,3.0)
        val evalPop = pop.zip(eval)

        val threshold = 3.1
        val scoreThreshold = Completion.scoreThreshold[Int](threshold)

        val complete = scoreThreshold(evalPop)

        val expected = false

        assertResult(expected)(complete)
    }

}
