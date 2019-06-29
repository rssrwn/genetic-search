package geneticsearch.genotype

import scala.util.{Failure, Random, Success, Try}


/**
  * Genotypes of binary strings
  * @param str Binary string to use as genotype, non-binary chars are ignored
  * @param flipProb Probability of each bit being flipped when mutating
  */
class BinaryString(val str: String, val flipProb: Float) extends Genotype[Int] {

    private val binaryList: List[Int] = {
        str.toCharArray
                .filter(c => c == '0' || c == '1')
                .map(c => c.toString.toInt)
                .toList
    }

    override def length: Int = {
        binaryList.length
    }

    override def apply(idx: Int): Int = {
        binaryList.apply(idx)
    }

    override def iterator: Iterator[Int] = {
        binaryList.iterator
    }

    // TODO dont return a try, just kill ??
    override def split(index: Int): Try[(Genotype[Int], Genotype[Int])] = {
        if (index >= length) {
            val message = "Split index cannot be larger or equal to the length of the BinaryString"
            Failure(new java.lang.IndexOutOfBoundsException(message))
        } else {
            val midPoint = length / 2
            val (l1, l2) = binaryList.splitAt(midPoint)
            val binStrPair = (new BinaryString(l1.toString(), flipProb), new BinaryString(l2.toString(), flipProb))
            Success(binStrPair)
        }
    }

    override def merge(that: Genotype[Int]): Genotype[Int] = {
        val thatBinStr = that.asInstanceOf[BinaryString]
        val newStr = this.str + thatBinStr.str
        new BinaryString(newStr, flipProb)
    }

    override def mutate(): Genotype[Int] = {
        val newStr = binaryList.map(bit => BinaryString.flipBit(bit, flipProb).toString)
                .reduceLeft(_++_)

        new BinaryString(newStr, flipProb)
    }

    override def distance(that: Genotype[Int]): Try[Float] = {
        if (length != that.length) {
            val message = "Both genotypes must the same length"
            Failure(new java.lang.IndexOutOfBoundsException(message))
        } else {
            val thatBinStr = that.asInstanceOf[BinaryString]
            val dist = binaryList.zip(thatBinStr)
                    .map(elems => math.abs(elems._1 - elems._2))
                    .sum

            Success(dist)
        }
    }

}

object BinaryString {

    /*
    Flips bit with probability
    Bit must be either 0 or 1
     */
    private def flipBit(bit: Int, prob: Float): Int = {
        val rand = Random.nextFloat()
        if (rand <= prob) {
            if (bit == 0) {
                1
            } else {
                0
            }
        } else {
            bit
        }
    }

}
