package geneticsearch.operators

import geneticsearch.Types.SelectionOp


object Selection {

    /**
      * Returns a function which takes the top <numToSelect> genotypes from the population
      * @param numToSelect The number of genotypes to be selected
      * @tparam T Type of the elements within the genotype
      * @return Selection function
      */
    def selectBest[T](numToSelect: Int): SelectionOp[T] = {
        evalPop => {
            evalPop.sortWith(_._2 > _._2)
                    .map(_._1)
                    .take(numToSelect)
        }
    }

}
