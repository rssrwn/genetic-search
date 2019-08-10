package geneticsearch.operators

import geneticsearch.genotype.Sequence
import org.scalatest.FunSuite


class SelectionTests extends FunSuite {

    private val numToSelect = 2
    private val elitism = 2
    private val selectBest = Selection.selectBest[Int](numToSelect)
    private val tournament = Selection.tournament[Int](numToSelect)
    private val tournamentWithElitism = Selection.tournament[Int](numToSelect, elitism)

    private val pop = (new Sequence[Int](Seq(1,2,3), null, null) ::
            new Sequence[Int](Seq(11,12,13), null, null) ::
            new Sequence[Int](Seq(21,22,23), null, null) :: Nil).toVector

    test("selectBest returns a pop of correct size") {
        val eval = Seq(3.0,2.0,6.0)
        val evalPop = pop.zip(eval)

        val selected = selectBest(evalPop)

        assertResult(numToSelect)(selected.length)
    }

    test("selectBest returns the expected genotypes") {
        val eval = Seq(3.0,2.0,6.0)
        val evalPop = pop.zip(eval)

        val selected = selectBest(evalPop)

        val expectedBest = (Seq(21,22,23),6.0)
        val expectedSecond = (Seq(1,2,3),3.0)

        assertResult(expectedBest)(selected.head)
        assertResult(expectedSecond)(selected(1))
    }

    test("tournament returns a pop of correct size") {
        val eval = Seq(3.0,2.0,6.0)
        val evalPop = pop.zip(eval)

        val selected = tournament(evalPop)

        val elitismSelected = tournamentWithElitism(evalPop)

        assertResult(numToSelect)(selected.length)
        assertResult(numToSelect)(elitismSelected.length)
    }

    test("tournament returns the expected genotypes") {
        val eval = Seq(3.0,2.0,6.0)
        val evalPop = pop.zip(eval)

        val selected = tournamentWithElitism(evalPop)

        val expected = (Seq(21,22,23),6.0)

        assertResult(expected)(selected.head)
        assertResult(expected)(selected(1))
    }

    test("tournament returns a pop of expected size when elitism is greater than numToSelect") {
        val eval = Seq(3.0,2.0,6.0)
        val evalPop = pop.zip(eval)

        val elitism = 3
        val tournamentWithElitism = Selection.tournament[Int](numToSelect, elitism)

        val selected = tournamentWithElitism(evalPop)

        assertResult(numToSelect)(selected.length)
    }

}
