package geneticsearch

import geneticsearch.genotype.Genotype

object Types {

    // TODO use vector instead of list

    type Population[T] = List[Genotype[T]]
    type EvalPopulation[T] = List[(Genotype[T], Double)]
    type GenotypePair[T] = (Genotype[T], Genotype[T])

    type FitnessOp[T] = Genotype[T] => Double
    type MutationOp[T] = Population[T] => Population[T]
    type SelectionOp[T] = EvalPopulation[T] => Population[T]
    type CrossoverOp[T] = Population[T] => Population[T]

    type MutationFunc[T] = T => T
    type DistanceFunc[T] = (T, T) => Double

}
