import cats.{Defer, MonoidK}

import scala.annotation.tailrec
import cats.Applicative
import cats.Monad
import cats.Foldable

final case class Levels[M[_], A](levels: LazyList[M[A]]):

  def run(using m: MonoidK[M]): M[A] = levels.foldRight(m.empty)(m.combineK)

  def concat(that: Levels[M, A])(using M: MonoidK[M]): Levels[M, A] =
    Levels(
      M.empty #:: this.levels
        .zipAll(that.levels, M.empty, M.empty)
        .map(M.combineK)
    )

  def wrap(using M: MonoidK[M]): Levels[M, A] = Levels(M.empty #:: levels)

  def flatMap[B](
      f: A => Levels[M, B]
  )(using mo: MonadPlus[M]): Levels[M, B] =
    levels match {
      case Levels(levels) if levels.isEmpty => Levels.empty
      case Levels(xb #:: xbs) =>
        Levels(mo.flatMap(xb)(f)).concat(Levels(xbs).flatMap(f).wrap)
    }

object Levels:

  def apply[M[_], A](a: A)(using A: Applicative[M]): Levels[M, A] =
    Levels(LazyList(A.pure(a)))

  def empty[M[_], A]: Levels[M, A] = Levels(LazyList.empty)

  given defer[M[_]](using D: Defer[M]): Defer[[X] =>> Levels[M, X]] =
    new Defer[[X] =>> Levels[M, X]] {
      override def defer[A](fa: => Levels[M, A]): Levels[M, A] = fa
    }

  given monadPlus[M[_]](using M: MonadPlus[M]): MonoidK[[X] =>> Levels[M, X]] =
    new MonadPlus[[X] =>> Levels[M, X]] {

      override def empty[A]: Levels[M, A] = Levels.empty

      override def combineK[A](
          levels1: Levels[M, A],
          levels2: Levels[M, A]
      ): Levels[M, A] =
        levels1.concat(levels2)

      override def pure[A](a: A): Levels[M, A] = Levels(a)
    }
