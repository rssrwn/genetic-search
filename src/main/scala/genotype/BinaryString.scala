package genotype


/**
  * Genotypes of binary strings
  * @param str Binary string to use as genotype, non-binary chars are ignored
  */
class BinaryString(str: String) extends Genotype[Int] {

    private val binaryList: List[Int] = {
        str.toCharArray
                .filterNot(c => c == '0' || c == '1')
                .map(c => c.toInt)
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
        val thatBinStr: BinaryString = that.asInstanceOf[BinaryString]
        val newStr = this.str + thatBinStr.str
        new BinaryString(newStr)
    }

}
