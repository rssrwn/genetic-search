package geneticsearch.genotype

import geneticsearch.Types.{SeqDistanceFunc, SeqMutationFunc}

import scala.util.{Failure, Success, Try}


class Sequence[T](elems: Seq[T], distFunc: SeqDistanceFunc[T], mutFunc: SeqMutationFunc[T]) extends Genotype[T] {

    private val vec: Vector[T] = elems.toVector

    private def this(elems: Seq[T]) = {
        this(elems, distFunc, mutFunc)
    }

    override def split(index: Int): Try[(Genotype[T], Genotype[T])] = {
        if (index >= length) {
            val message = "Split index cannot be larger or equal to the length of the BinaryString"
            Failure(new java.lang.IndexOutOfBoundsException(message))
        } else {
            val midPoint = length / 2
            val (l1, l2) = vec.splitAt(midPoint)
            val listPair = (new Sequence(l1), new Sequence(l2))
            Success(listPair)
        }
    }

    override def merge(that: Genotype[T]): Genotype[T] = {
        val thatList = that.asInstanceOf[Sequence]
        val newElems = this.vec ++ thatList.vec
        new Sequence(newElems)
    }

    override def mutate(): Genotype[T] = {
        mutFunc(this)
    }

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
