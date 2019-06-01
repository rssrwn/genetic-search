package algorithm

import util.Types.{CrossoverOp, FitnessOp, MutationOp, Population, SelectionOp}


class GeneticAlgorithmBuilder[T] {

    private var pop: Population[T] = _
    private var fitnessOp: FitnessOp[T] = _
    private var selectionOp: SelectionOp[T] = _
    private var crossoverOp: CrossoverOp[T] = _
    private var mutationOp: MutationOp[T] = _
    private var mutationProb: Float = 1

    def build(): GeneticAlgorithm[T] = {
        new GeneticAlgorithm[T]()
    }

    def withPopulation(pop: Population[T]): GeneticAlgorithmBuilder[T] = {
        this.pop = pop
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

    def withMutationProb(prob: Float): GeneticAlgorithmBuilder[T] = {
        mutationProb = prob
        this
    }

}
