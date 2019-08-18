package geneticsearch.algorithm

import geneticsearch.Types.{CompletionOp, CrossoverOp, FitnessOp, MutationOp, SelectionOp}


class GeneticAlgorithmOperators[T](val fitnessOp: FitnessOp[T],
                                   val selectionOp: SelectionOp[T],
                                   val crossoverOp: CrossoverOp[T] = null,
                                   val mutationOp: MutationOp[T] = null,
                                   val completionOp: CompletionOp[T] = null,
                                   val completionCheckIter: Int = 1) {

    def containsCrossoverOp: Boolean = {
        crossoverOp != null
    }

    def containsMutationOp: Boolean = {
        mutationOp != null
    }

    def containsCompletionOp: Boolean = {
        completionOp != null
    }

}
