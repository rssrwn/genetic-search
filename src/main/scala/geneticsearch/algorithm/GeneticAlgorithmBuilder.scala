package geneticsearch.algorithm

import geneticsearch.Types.{CrossoverOp, FitnessOp, MutationOp, Population, SelectionOp}


class GeneticAlgorithmBuilder[T] {

    private var pop: Population[T] = _
    private var numIterations: Int = _
    private var fitnessOp: FitnessOp[T] = _
    private var selectionOp: SelectionOp[T] = _
    private var crossoverOp: CrossoverOp[T] = _
    private var mutationOp: MutationOp[T] = _

    def build(): GeneticAlgorithm[T] = {
        val ops = new GeneticAlgorithmOperators[T](fitnessOp, selectionOp, crossoverOp, mutationOp)
        new GeneticAlgorithm[T](ops, numIterations)
    }

    def withPopulation(pop: Population[T]): GeneticAlgorithmBuilder[T] = {
        this.pop = pop
        this
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

}
