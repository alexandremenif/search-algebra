import DiffList.{defer, monadPlus}
import Cps.monadPlus
import Levels.{monoidK, pure}
import cats.{Alternative, Applicative, Defer, Monad, MonoidK}
import cats.data.ContT
import scala.collection.immutable.Queue

@main def hello: Unit =
//  Pythagoras[List](100).take(100).foreach(println)
//  Pythagoras[DiffList](1000).take(1).foreach(println)
//  Pythagoras[[X] =>> ContT[DiffList, (Int, Int, Int), X]](5).evalP
//    .take(10)
//    .foreach(println)

//  Pythagoras[[X] =>> ContT[[Y] =>> Levels[DiffList, Y], (Int, Int, Int), X]](
//    1000
//  ).evalP.run
//    .take(1)
//    .foreach(println)
  Pythagoras[Queue](1000).take(1).foreach(s => println(("SOLUTION", s)))
