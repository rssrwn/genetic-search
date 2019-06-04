package algorithm

import util.Types.{CrossoverOp, FitnessOp, MutationOp, SelectionOp}


class GeneticAlgorithmOperators[T](val fitnessOp: FitnessOp[T],
                                   val selectionOp: SelectionOp[T],
                                   val crossoverOp: CrossoverOp[T] = null,
                                   val mutationOp: MutationOp[T] = null,
                                   val mutationProb: Float = 1) {

    def containsCrossoverOp: Boolean = {
        crossoverOp != null
    }

    def containsMutationOp: Boolean = {
        mutationOp != null
    }

}
