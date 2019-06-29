package geneticsearch.genotype
import scala.util.Try


class List[T](elems: Seq[T]) extends Genotype[T] {

    private val seq: Vector[T] = elems.toVector

    override def split(index: Int): Try[(Genotype[T], Genotype[T])] = ???

    override def merge(that: Genotype[T]): Genotype[T] = ???

    override def distance(that: Genotype[T]): Try[Float] = ???

    override def length: Int = {
        seq.length
    }

    override def apply(idx: Int): T = {
        seq.apply(idx)
    }

    override def iterator: Iterator[T] = {
        seq.iterator
    }

}
