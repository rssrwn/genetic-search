package algorithm

import util.Types.{CrossoverOp, FitnessOp, MutationOp, SelectionOp}


class GeneticAlgorithmOperators[T](val fitnessOp: FitnessOp[T],
                                   val selectionOp: SelectionOp[T],
                                   val crossoverOp: CrossoverOp[T] = _,
                                   val mutationOp: MutationOp[T] = _,
                                   val mutationProb: Float = 1) {

}
