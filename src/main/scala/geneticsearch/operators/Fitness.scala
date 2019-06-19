package geneticsearch.operators

import geneticsearch.Types.FitnessOp
import geneticsearch.genotype.Genotype
import geneticsearch.Util.extractSuccess


object Fitness {

    /**
      * Returns a fitness function based on how far away each genotype is from the target
      * The fitness is calculated on 1 - distance from the target
      * @param target Genotype representing the target of the algorithm
      * @tparam T Type of the elements within the genotype
      * @return Fitness function
      */
    def targetProximity[T](target: Genotype[T]): FitnessOp[T] = {
        genotype => {
            val dist = extractSuccess(genotype.distance(target))
            1 - dist
        }
    }

}
