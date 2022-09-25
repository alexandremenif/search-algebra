import cats.{Alternative, Monad}

type MonadPlus[M[_]] = Alternative[M] & Monad[M]

extension [M[_]: MonadPlus, A](fa: M[A]) {

  def map[B](f: A => B): M[B] = summon[MonadPlus[M]].map(fa)(f)

  def flatMap[B](f: A => M[B]): M[B] = summon[MonadPlus[M]].flatMap(fa)(f)

  def withFilter(predicate: A => Boolean): M[A] = for {
    a <- fa
    _ <- summon[MonadPlus[M]].guard(predicate(a))
  } yield a
}
