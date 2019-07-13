package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class SelectionTests extends FunSuite {

    private val numToSelect = 2
    private val selection = Selection.selectBest[Int](numToSelect)

    private val pop = (new Sequence[Int](Seq(1,2,3), null, null) ::
            new Sequence[Int](Seq(11,12,13), null, null) ::
            new Sequence[Int](Seq(21,22,23), null, null) :: Nil).toVector

    test("selectBest returns a pop of correct size") {
        val eval = Seq(3.0,2.0,6.0)
        val evalPop = pop.zip(eval)

        val selected = selection(evalPop)

        val expectedBest = (Seq(21,22,23),6.0)
        val expectedSecond = (Seq(1,2,3),3.0)

        assertResult(expectedBest)(selected.head)
        assertResult(expectedSecond)(selected(1))
    }

}
