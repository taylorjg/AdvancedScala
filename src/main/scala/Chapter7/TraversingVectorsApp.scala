package Chapter7

import scala.language.higherKinds
import cats.syntax.apply._
import cats.Applicative
import cats.syntax.applicative._
import cats.instances.vector._

object TraversingVectorsApp extends App {

  val result1 = listSequence(List(Vector(1, 2), Vector(3, 4)))
  val result2 = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  println(s"result1: $result1")
  println(s"result2: $result2")

  private def listTraverse[F[_]: Applicative, A, B](list: List[A])(
      func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  private def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)
}
