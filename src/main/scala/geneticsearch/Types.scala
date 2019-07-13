package geneticsearch

import geneticsearch.genotype.Genotype

object Types {

    type Population[T] = Vector[Genotype[T]]
    type EvalPopulation[T] = Vector[(Genotype[T], Double)]
    type GenotypePair[T] = (Genotype[T], Genotype[T])

    type FitnessOp[T] = Genotype[T] => Double
    type MutationOp[T] = Population[T] => Population[T]
    type SelectionOp[T] = EvalPopulation[T] => EvalPopulation[T]
    type CrossoverOp[T] = Population[T] => Population[T]

    type MutationFunc[T] = T => T
    type DistanceFunc[T] = (T, T) => Double

}
