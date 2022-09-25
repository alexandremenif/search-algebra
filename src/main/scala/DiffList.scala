import cats.{Alternative, Defer, Monad}

import scala.Tuple.FlatMap
import scala.annotation.tailrec

final case class DiffList[A](append: LazyList[A] => LazyList[A] = identity)
    extends Iterable[A] {

  def concat(that: DiffList[A]): DiffList[A] = DiffList(
    this.append compose that.append
  )

  def toLazyList: LazyList[A] = append(LazyList.empty)

  def iterator: Iterator[A] = toLazyList.iterator

  def flatMap[B](f: A => DiffList[B]): DiffList[B] =
    DiffList(
      (for {
        a <- toLazyList
        b <- f(a).toLazyList
      } yield b).concat(_)
    )
}

object DiffList:

  def empty[A]: DiffList[A] = new DiffList[A](identity)

  def apply[A](a: A): DiffList[A] = DiffList[A](a #:: _)

  given monadPlus: MonadPlus[DiffList] = new Alternative[DiffList]
    with Monad[DiffList] {

    def empty[A]: DiffList[A] = DiffList.empty

    def combineK[A](fa1: DiffList[A], fa2: DiffList[A]): DiffList[A] =
      fa1.concat(fa2)

    def pure[A](a: A): DiffList[A] = DiffList(a)

    def flatMap[A, B](fa: DiffList[A])(f: A => DiffList[B]): DiffList[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => DiffList[Either[A, B]]): DiffList[B] =
      DiffList(summon[Monad[LazyList]].tailRecM(a)(f(_).toLazyList).concat(_))
  }

  given defer: Defer[DiffList] = new Defer[DiffList]:
    override def defer[A](fa: => DiffList[A]): DiffList[A] = fa
