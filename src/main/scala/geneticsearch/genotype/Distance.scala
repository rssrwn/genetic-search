package geneticsearch.genotype

import geneticsearch.Types.DistanceFunc


/*
Factory for genotype distance functions
 */
object Distance {

    def hamming(): DistanceFunc[Sequence[Int]] = {
        (seq1, seq2) => {
            seq1.zip(seq2)
                    .map({case(left, right) => Math.abs(left - right)})
                    .sum
        }
    }

    def euclideanInt(): DistanceFunc[Sequence[Int]] = {
        (seq1, seq2) => {
            val sqSum = seq1.zip(seq2)
                    .map({case(num1, num2) => num1 - num2})
                    .map(Math.pow(_, 2))
                    .sum

            Math.sqrt(sqSum).asInstanceOf[Float]
        }
    }

    def euclideanFloat(): DistanceFunc[Sequence[Float]] = {
        (seq1, seq2) => {
            val sqSum = seq1.zip(seq2)
                    .map({case(num1, num2) => num1 - num2})
                    .map(Math.pow(_, 2))
                    .sum

            Math.sqrt(sqSum).asInstanceOf[Float]
        }
    }

}
