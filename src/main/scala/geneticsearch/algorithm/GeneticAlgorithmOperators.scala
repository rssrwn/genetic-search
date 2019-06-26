package geneticsearch.algorithm

import geneticsearch.Types.{CrossoverOp, FitnessOp, MutationOp, SelectionOp}


class GeneticAlgorithmOperators[T](val fitnessOp: FitnessOp[T],
                                   val selectionOp: SelectionOp[T],
                                   val crossoverOp: CrossoverOp[T] = null,
                                   val mutationOp: MutationOp[T] = null) {

    def containsCrossoverOp: Boolean = {
        crossoverOp != null
    }

    def containsMutationOp: Boolean = {
        mutationOp != null
    }

}
