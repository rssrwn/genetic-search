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

}
