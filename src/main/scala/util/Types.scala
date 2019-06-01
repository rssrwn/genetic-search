package util

import genotype.Genotype

object Types {

    type Population[T] = List[Genotype[T]]
    type EvalPopulation[T] = List[(Genotype[T], Float)]
    private type GenotypePair[T] = (Genotype[T], Genotype[T])

    type FitnessOp[T] = Genotype[T] => Float
    type MutationOp[T] = Genotype[T] => Genotype[T]
    type SelectionOp[T] = EvalPopulation[T] => Population[T]
    type CrossoverOp[T] = GenotypePair[T] => GenotypePair[T]

}
