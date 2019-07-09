package geneticsearch.genotype

import geneticsearch.Types.{DistanceFunc, GenotypePair, MutationFunc}

import scala.util.{Failure, Success, Try}


class Sequence[T](seqElems: Seq[T], val mutFunc: MutationFunc[Sequence[T]], val distFunc: DistanceFunc[Sequence[T]]) extends Genotype[T] {

    private val vec = seqElems.toVector

    def withElems(elems: Seq[T]): Sequence[T] = {
        new Sequence(elems, mutFunc, distFunc)
    }

    override def elems: Vector[T] = {
        vec
    }

    override def split(index: Int): GenotypePair[T] = {
        if (index >= length) {
            val message = "Split index cannot be larger or equal to the length of the BinaryString"
            throw new IndexOutOfBoundsException(message)
        } else {
            val (l1, l2) = vec.splitAt(index)
            val listPair = (withElems(l1), withElems(l2))
            listPair
        }
    }

    override def merge(that: Genotype[T]): Genotype[T] = {
        val thatList = that.asInstanceOf[Sequence[T]]
        val newElems = this.vec ++ thatList.vec
        withElems(newElems)
    }

    override def mutate(): Genotype[T] = {
        mutFunc(this)
    }

    // Must be called with a Sequence genotype
    override def distance(that: Genotype[T]): Double = {
        val dist = distFunc(this, that.asInstanceOf[Sequence[T]])
        dist
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
