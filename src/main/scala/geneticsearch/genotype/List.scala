package geneticsearch.genotype

import geneticsearch.Types.DistanceFunc

import scala.util.{Failure, Success, Try}


class List[T](elems: Seq[T], distFunc: DistanceFunc[T]) extends Genotype[T] {

    private val vec: Vector[T] = elems.toVector

    override def split(index: Int): Try[(Genotype[T], Genotype[T])] = {
        if (index >= length) {
            val message = "Split index cannot be larger or equal to the length of the BinaryString"
            Failure(new java.lang.IndexOutOfBoundsException(message))
        } else {
            val midPoint = length / 2
            val (l1, l2) = vec.splitAt(midPoint)
            val listPair = (new List(l1, distFunc), new List(l2, distFunc))
            Success(listPair)
        }
    }

    override def merge(that: Genotype[T]): Genotype[T] = {
        val thatList = that.asInstanceOf[List]
        val newElems = this.vec ++ thatList.vec
        new List(newElems, distFunc)
    }

    override def mutate(): Genotype[T] = super.mutate()

    /*
    Euclidean distance between two vectors
     */
    // TODO remove failure case, use first n elems
    override def distance(that: Genotype[T]): Try[Float] = {
        val dist = distFunc(this, that)
        Success(dist)
    }

    override def length: Int = {
        vec.length
    }

    override def apply(idx: Int): T = {
        vec.apply(idx)
    }

    override def iterator: Iterator[T] = {
        vec.iterator
    }

}
