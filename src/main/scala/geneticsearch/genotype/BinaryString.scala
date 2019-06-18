package geneticsearch.genotype

import scala.util.Random


/**
  * Genotypes of binary strings
  * @param str Binary string to use as genotype, non-binary chars are ignored
  * @param flipProb Probability of flipping a bit when mutating this genotype
  */
class BinaryString(private val str: String, private val flipProb: Float) extends Genotype[BinaryString] with Seq[Int] {

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

    override def split: (BinaryString, BinaryString) = {
        val midPoint = length / 2
        val (l1, l2) = binaryList.splitAt(midPoint)
        (new BinaryString(l1.toString(), flipProb), new BinaryString(l2.toString(), flipProb))
    }

    override def merge(that: BinaryString): BinaryString = {
        val newStr = this.str + that.str
        new BinaryString(newStr, flipProb)
    }

    override def mutate: Genotype[BinaryString] = {
        val newStr = binaryList.map(bit => BinaryString.flipBit(bit, flipProb).toString)
                .reduceLeft(_++_)

        new BinaryString(newStr, flipProb)
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
