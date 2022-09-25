object Pythagoras:

  def apply[M[_]: MonadPlus](n: Int): M[(Int, Int, Int)] = {
    val monadPlus = summon[MonadPlus[M]]
    for {
      a <- monadPlus.fromIterableOnce(1 to n)
      _ = println(a)
      b <- monadPlus.fromIterableOnce(1 to n)
      _ = println((a, b))
      c <- monadPlus.fromIterableOnce(1 to n)
      _ = println((a, b, c))
      if a * a + b * b == c * c
    } yield (a, b, c)
  }
