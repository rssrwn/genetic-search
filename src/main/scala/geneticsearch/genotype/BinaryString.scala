package geneticsearch.genotype

import scala.util.Random


/**
  * Genotypes of binary strings
  * @param str Binary string to use as genotype, non-binary chars are ignored
  */
class BinaryString(private val str: String) extends Genotype[Int] {

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

    override def split: (Genotype[Int], Genotype[Int]) = {
        val midPoint = length / 2
        val (l1, l2) = binaryList.splitAt(midPoint)
        (new BinaryString(l1.toString()), new BinaryString(l2.toString()))
    }

    override def merge(that: Genotype[Int]): Genotype[Int] = {
        val thatBinStr = that.asInstanceOf[BinaryString]
        val newStr = this.str + thatBinStr.str
        new BinaryString(newStr)
    }

    override def mutate(prob: Float): Genotype[Int] = {
        val newStr = binaryList.map(bit => BinaryString.flipBit(bit, prob).toString)
                .reduceLeft(_++_)

        new BinaryString(newStr)
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
