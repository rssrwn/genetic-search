package geneticsearch.genotype

import geneticsearch.Types.{DistanceFunc, MutationFunc}

import scala.util.{Failure, Success, Try}


class Sequence[T](elems: Seq[T], mutFunc: MutationFunc[Sequence[T]], distFunc: DistanceFunc[Sequence[T]]) extends Genotype[T] {

    val vec: Vector[T] = elems.toVector

    private def Sequence(elems: Seq[T]): Sequence[T] = {
        new Sequence(elems, mutFunc, distFunc)
    }

    // TODO dont return a try?
    override def split(index: Int): Try[(Genotype[T], Genotype[T])] = {
        if (index >= length) {
            val message = "Split index cannot be larger or equal to the length of the BinaryString"
            Failure(new java.lang.IndexOutOfBoundsException(message))
        } else {
            val midPoint = length / 2
            val (l1, l2) = vec.splitAt(midPoint)
            val listPair = (Sequence(l1), Sequence(l2))
            Success(listPair)
        }
    }

    override def merge(that: Genotype[T]): Genotype[T] = {
        val thatList = that.asInstanceOf[Sequence]
        val newElems = this.vec ++ thatList.vec
        Sequence(newElems)
    }

    override def mutate(): Genotype[T] = {
        mutFunc(this)
    }

    // TODO remove try
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
