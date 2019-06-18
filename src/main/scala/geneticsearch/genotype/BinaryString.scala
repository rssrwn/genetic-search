package geneticsearch.genotype


/**
  * Genotypes of binary strings
  * @param str Binary string to use as genotype, non-binary chars are ignored
  */
class BinaryString(private val str: String) extends Genotype[BinaryString] with Seq[Int] {

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
        (new BinaryString(l1.toString()), new BinaryString(l2.toString()))
    }

    override def merge(that: BinaryString): BinaryString = {
        val newStr = this.str + that.str
        new BinaryString(newStr)
    }

}
