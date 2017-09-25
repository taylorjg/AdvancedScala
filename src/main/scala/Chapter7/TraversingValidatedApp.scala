package Chapter7

import scala.language.higherKinds
import cats.syntax.apply._
import cats.Applicative
import cats.syntax.applicative._
import cats.data.Validated
import cats.instances.list._

object TraversingValidatedApp extends App {

  type ErrorsOr[A] = Validated[List[String], A]

  private def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  val result1 = process(List(2, 4, 6))
  val result2 = process(List(1, 2, 3))
  println(s"result1: $result1")
  println(s"result2: $result2")

  private def listTraverse[F[_]: Applicative, A, B](list: List[A])(
      func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }
}
