import genotype.Genotype

object Types {

    type Population[T] = List[Genotype[T]]
    type EvalPopulation[T] = List[(Genotype[T], Float)]
    type GenotypePair[T] = (Genotype[T], Genotype[T])

}
