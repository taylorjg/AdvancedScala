package Chapter4

import scala.language.higherKinds

object Funcy extends App {

  val someInt = Some(41)
  val noneInt = Option.empty[Int]

  val opt2 = MonadInstances.optionMonad.flatMap(someInt)(a => Some(a + 1))
  println(s"opt2: $opt2")

  val opt3 = MonadInstances.optionMonad.map(someInt)(a => a + 1)
  println(s"opt3: $opt3")

  val opt4 = MonadInstances.optionMonad.flatMap(noneInt)(a => Some(a + 1))
  println(s"opt4: $opt4")

  val opt5 = MonadInstances.optionMonad.map(noneInt)(a => a + 1)
  println(s"opt5: $opt5")

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }

  object MonadInstances {
    implicit val optionMonad = new Monad[Option] {
      override def pure[A](a: A): Option[A] = Some(a)
      override def flatMap[A, B](value: Option[A])(func: A => Option[B]): Option[B] =
        value match {
          case Some(a) => func(a)
          case None => None
        }
    }
  }
}
