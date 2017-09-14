package Chapter4

import scala.language.higherKinds
import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._

object IdentityApp extends App {

  type Id[A] = A

  def sumSquare[M[_]: Monad](a: M[Int], b: M[Int]): M[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  val a = Monad[Id].pure(3)
  val b = Monad[Id].flatMap(a)(_ + 1)
  println(s"sumSquare(a, b): ${sumSquare(a, b)}")

  object MonadInstances {
    implicit val idMonad: Monad[Id] = new Monad[Id] {
      override def pure[A](a: A): Id[A] = a
      override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] =
        func(value)
      override def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
      override def tailRecM[A, B](a: A)(f: (A) => Id[Either[A, B]]): Id[B] = ???
    }
  }
}
