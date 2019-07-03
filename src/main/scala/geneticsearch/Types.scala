package geneticsearch

import geneticsearch.genotype.Genotype

object Types {

    type Population[T] = List[Genotype[T]]
    type EvalPopulation[T] = List[(Genotype[T], Float)]
    type GenotypePair[T] = (Genotype[T], Genotype[T])

    type FitnessOp[T] = Genotype[T] => Float
    type MutationOp[T] = Population[T] => Population[T]
    type SelectionOp[T] = EvalPopulation[T] => Population[T]
    type CrossoverOp[T] = (Genotype[T], Genotype[T]) => GenotypePair[T]

    type MutationFunc[T] = T => T
    type DistanceFunc[T] = (T, T) => Float

}
