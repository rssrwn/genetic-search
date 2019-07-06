package geneticsearch.genotype

class BinaryString {

    // TODO

    private def formatBinStr(str: String): Seq[Int] = {
        str.toCharArray
                .filter(c => c == '0' || c == '1')
                .map(c => c.toString.toInt)
                .toList
    }

}
