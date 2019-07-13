package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class CompletionTests extends FunSuite {

    private val pop = (new Sequence[Int](Seq(1,2,3), null, null) ::
            new Sequence[Int](Seq(11,12,13), null, null) ::
            new Sequence[Int](Seq(21,22,23), null, null) :: Nil).toVector

    private val target = new Sequence[Int](Seq(1,2,3), null, null)
    private val completion = Completion.containsTarget(target)

    test("containsTarget returns true when pop contains target genotype") {
        val pop = (new Sequence[Int](Seq(1,2,3), null, null) ::
                new Sequence[Int](Seq(11,12,13), null, null) ::
                new Sequence[Int](Seq(21,22,23), null, null) :: Nil).toVector

        val eval = Seq(1.0,2.0,3.0)
        val evalPop = pop.zip(eval)

        val complete = completion(evalPop)

        val expectedComplete = true

        assertResult(expectedComplete)(complete)
    }

    test("containsTarget returns false when pop does not contain target") {
        val pop = (new Sequence[Int](Seq(6,7,8), null, null) ::
                new Sequence[Int](Seq(11,12,13), null, null) ::
                new Sequence[Int](Seq(21,22,23), null, null) :: Nil).toVector

        val eval = Seq(1.0,2.0,3.0)
        val evalPop = pop.zip(eval)

        val complete = completion(evalPop)

        val expectedComplete = false

        assertResult(expectedComplete)(complete)
    }

}
