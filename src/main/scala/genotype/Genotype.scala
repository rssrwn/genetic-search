package genotype


trait Genotype[T] extends Seq[T] {

    override def length: Int

    override def apply(idx: Int): T

    override def iterator: Iterator[T]

}
