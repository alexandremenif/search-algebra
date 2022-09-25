import cats.{Alternative, Defer, Monad, MonoidK}
import cats.data.ContT

object Cps:

  given monadPlus[M[_]: MonoidK: Defer, A]: MonadPlus[[X] =>> ContT[M, A, X]] =
    new Alternative[[X] =>> ContT[M, A, X]] with Monad[[X] =>> ContT[M, A, X]] {

      override def empty[B]: ContT[M, A, B] =
        ContT(_ => summon[MonoidK[M]].empty)

      override def combineK[B](
          cps1: ContT[M, A, B],
          cps2: ContT[M, A, B]
      ): ContT[M, A, B] =
        ContT(f => summon[MonoidK[M]].combineK(cps1.run(f), cps2.run(f)))

      override def pure[B](b: B): ContT[M, A, B] = ContT.pure(b)

      override def flatMap[B, C](cps: ContT[M, A, B])(
          f: B => ContT[M, A, C]
      ): ContT[M, A, C] =
        cps.flatMap(f)

      override def tailRecM[B, C](b: B)(
          f: B => ContT[M, A, Either[B, C]]
      ): ContT[M, A, C] =
        ContT.tailRecM(b)(f)
    }
