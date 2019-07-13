package geneticsearch.operators

import geneticsearch.Types.CompletionOp
import geneticsearch.genotype.Genotype


object Completion {

    /**
      * Returns a completion operator which will return true if a population contains the target genotype
      * @param target Target genotype to compare to
      * @tparam T Type of the elements within each genotype
      * @return Completion operator
      */
    def containsTarget[T](target: Genotype[T]): CompletionOp[T] = {
        evalPop => {
            evalPop.map(_._1.elems).contains(target.elems)
        }
    }

}
