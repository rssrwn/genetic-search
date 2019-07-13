package geneticsearch.algorithm

import geneticsearch.Types.{CompletionOp, CrossoverOp, FitnessOp, MutationOp, SelectionOp}


class GeneticAlgorithmBuilder[T] {

    private var numIterations: Int = _
    private var fitnessOp: FitnessOp[T] = _
    private var selectionOp: SelectionOp[T] = _
    private var crossoverOp: CrossoverOp[T] = _
    private var mutationOp: MutationOp[T] = _
    private var completionOp: CompletionOp[T] = _

    def build(): GeneticAlgorithm[T] = {
        val ops = new GeneticAlgorithmOperators[T](fitnessOp, selectionOp, crossoverOp, mutationOp, completionOp)
        new GeneticAlgorithm[T](ops, numIterations)
    }

    def withNumIterations(iters: Int): GeneticAlgorithmBuilder[T] = {
        numIterations = iters
        this
    }

    def withFitnessOp(op: FitnessOp[T]): GeneticAlgorithmBuilder[T] = {
        fitnessOp = op
        this
    }

    def withSelectionOp(op: SelectionOp[T]): GeneticAlgorithmBuilder[T] = {
        selectionOp = op
        this
    }

    def withCrossoverOp(op: CrossoverOp[T]): GeneticAlgorithmBuilder[T] = {
        crossoverOp = op
        this
    }

    def withMutationOp(op: MutationOp[T]): GeneticAlgorithmBuilder[T] = {
        mutationOp = op
        this
    }

    def withCompletionOp(op: CompletionOp[T]): GeneticAlgorithmBuilder[T] = {
        completionOp = op
        this
    }

}
